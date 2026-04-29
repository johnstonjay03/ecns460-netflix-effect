# 01_wrangle.R
# Netflix Effect on Local Real Estate
# Purpose: Import, clean, and join all data sources into analysis-ready panels

library(tidyverse)
library(readxl)
library(rvest)
library(lubridate)


# Wikipedia URLs for each show
show_urls <- tibble(
  show = c("Yellowstone", "Breaking Bad", "The White Lotus",
           "Stranger Things", "Outer Banks", "Ozark"),
  url  = c(
    "https://en.wikipedia.org/wiki/Yellowstone_(American_TV_series)",
    "https://en.wikipedia.org/wiki/Breaking_Bad",
    "https://en.wikipedia.org/wiki/The_White_Lotus",
    "https://en.wikipedia.org/wiki/Stranger_Things",
    "https://en.wikipedia.org/wiki/Outer_Banks_(TV_series)",
    "https://en.wikipedia.org/wiki/Ozark_(TV_series)"
  )
)

# This function pulls the first 4-digit year from a piece of text.
# I use it to find the year a show first aired.
extract_year <- function(x) {
  if (is.na(x) || length(x) == 0) return(NA_integer_)
  m <- str_extract(x, "\\b(19|20)\\d{2}\\b")
  if (is.na(m)) NA_integer_ else as.integer(m)
}

# This function opens each show's Wikipedia page
# and tries to find the year the show first premiered.
# Wikipedia pages do not all store premiere dates in the same way,
# so the code checks for the year in two different ways.
# First try: look through the infobox rows.
# If a row is labeled something like "Original release" or "First aired,"
# pull the year from that row.
# Second try: if the first method does not work,
# search all of the infobox text for a release-related word
# and then pull the closest 4-digit year.
scrape_premiere_year <- function(url) {
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NA_integer_)
  
  infobox <- page %>% html_element("table.infobox")
  if (length(infobox) == 0 || is.na(infobox)) return(NA_integer_)
  
  premiere_pattern <- regex(
    "original release|first aired|released|release date|premiere",
    ignore_case = TRUE
  )
  
  # Pass 1: structured row scraping (handles nested rows too) 

  rows <- infobox %>% html_elements("tr")
  
  for (row in rows) {
    th <- row %>% html_element("th") %>% html_text2()
    td <- row %>% html_element("td") %>% html_text2()
    if (is.na(th) || is.na(td)) next
    if (str_detect(th, premiere_pattern)) {
      yr <- extract_year(td)
      if (!is.na(yr)) return(yr)
    }
  }
  
  #Pass 2: fallback — search the whole infobox text
  # Look for a release-keyword followed (within ~80 chars) by a 4-digit year.
  full_text <- infobox %>% html_text2()
  m <- str_match(
    full_text,
    regex(
      "(original release|first aired|released|release date|premiere)[^\\n]{0,80}?\\b((?:19|20)\\d{2})\\b",
      ignore_case = TRUE
    )
  )
  if (!is.na(m[1, 3])) return(as.integer(m[1, 3]))
  
  NA_integer_
}

# Run the scrape over all six shows
scraped <- show_urls %>%
  mutate(scraped_year = map_int(url, scrape_premiere_year))

message("Wikipedia scrape results:")
print(scraped)



# SECTION 1: SHOWS + FILMING LOCATIONS (from web scraping)

shows <- tibble(
  show = c(
    "Yellowstone",
    "Breaking Bad",
    "The White Lotus",  # NOTE: Season 1 filmed in Hawaii; international seasons excluded
    "Stranger Things",
    "Outer Banks",
    "Ozark"
  ),
  release_year = scraped$scraped_year,
  county     = c("Gallatin County",    "Bernalillo County", "Maui County",
                 "Fulton County",      "Charleston County", "Camden County"),
  state_abbr = c("MT", "NM", "HI", "GA", "SC", "MO"),
  state_name = c("Montana", "New Mexico", "Hawaii", "Georgia",
                 "South Carolina", "Missouri")
)

# NOTE: Game of Thrones (Croatia) and White Lotus Season 2+ (Sicily) are
# excluded — no U.S. county-level housing data available for those locations.
# Ozark uses Camden County, MO (Lake of the Ozarks filming area).

# SECTION 2: FHFA HOUSE PRICE INDEX

hpi_raw <- read_excel(
  "hpi_at_county.xlsx",
  skip = 6,                          # skip the 6-row header block
  col_names = c("state", "county", "fips", "year",
                "annual_change_pct", "hpi", "hpi_1990base", "hpi_2000base")
)

hpi_clean <- hpi_raw %>%
  # Remove any rows where state or county is missing (footer artifacts)
  filter(!is.na(state), !is.na(county), !is.na(year)) %>%
  # Keep only 2000–2024 to match Zillow era and show release window
  filter(year >= 2000, year <= 2024) %>%
  # Use HPI_2000base as primary outcome: all counties start at 100 in 2000,
  # making cross-county comparisons meaningful
  # NAs in hpi_2000base occur for counties with no data in 2000 — keep them
  # but flag; they will be excluded from DiD if control counties are also NA
  mutate(
    fips = as.character(fips),
    county_key = paste0(state, "_", county)   # e.g. "MT_Gallatin"
  ) %>%
  select(state, county, fips, county_key, year,
         annual_change_pct, hpi, hpi_2000base)

# confirm all 6 treatment counties are present
treatment_keys <- c("MT_Gallatin", "NM_Bernalillo", "HI_Maui",
                    "GA_Fulton",   "SC_Charleston",  "MO_Camden")

missing_hpi <- treatment_keys[!treatment_keys %in% hpi_clean$county_key]
if (length(missing_hpi) > 0) {
  warning("Missing treatment counties in HPI: ", paste(missing_hpi, collapse = ", "))
} else {
  message("All 6 treatment counties found in HPI data.")
}

# SECTION 3: ZILLOW RENTAL INDEX (ZORI) — wide to long

zori_raw <- read_csv("County_zori_uc_sfrcondomfr_sm_month.csv")

zori_long <- zori_raw %>%
  # Pivot all date columns (format: YYYY-MM-DD) to long format
  pivot_longer(
    cols      = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to  = "date",
    values_to = "zori"
  ) %>%
  mutate(
    date  = as.Date(date),
    year  = year(date),
    month = month(date)
  ) %>%
  # Keep only the identifier columns we need
  select(RegionID, RegionName, State, StateCodeFIPS, MunicipalCodeFIPS,
         date, year, month, zori) %>%
  rename(
    region_id   = RegionID,
    county      = RegionName,
    state       = State,
    fips_state  = StateCodeFIPS,
    fips_county = MunicipalCodeFIPS
  ) %>%
  # Build 5-digit FIPS to match HPI
  mutate(
    fips = paste0(
      str_pad(fips_state,  2, pad = "0"),
      str_pad(fips_county, 3, pad = "0")
    ),
    county_key = paste0(state, "_", str_remove(county, " County$"))
  ) %>%
  filter(year >= 2015, year <= 2025)   # Zillow starts 2015

# Annual average ZORI per county (for joining with annual HPI)
zori_annual <- zori_long %>%
  group_by(county_key, state, county, fips, year) %>%
  summarise(
    zori_mean = mean(zori, na.rm = TRUE),
    zori_n_months = sum(!is.na(zori)),
    .groups = "drop"
  ) %>%
  # Flag county-years with fewer than 6 months of data as unreliable
  mutate(zori_reliable = zori_n_months >= 6)

# SECTION 4: AIRBNB BOZEMAN (supplementary — Yellowstone treatment)

airbnb_raw <- read_csv(
  "listingsbozeman.csv.gz",
  show_col_types = FALSE
)

airbnb_clean <- airbnb_raw %>%
  select(
    id, room_type, bedrooms, accommodates,
    price, minimum_nights,
    review_scores_rating, number_of_reviews,
    estimated_occupancy_l365d, estimated_revenue_l365d,
    first_review, last_review,
    latitude, longitude
  ) %>%
  # price is stored as "$90.00" — strip $ and commas, convert to numeric
  mutate(
    price_num = as.numeric(str_remove_all(price, "[$,]")),
    first_review = as.Date(first_review),
    last_review  = as.Date(last_review),
    # Flag listings that started after Yellowstone premiere (June 2018)
    post_yellowstone = first_review >= as.Date("2018-06-20")
  ) %>%
  # Drop listings with missing price (53 rows, ~9% — likely inactive listings)
  filter(!is.na(price_num)) %>%
  # Drop extreme outliers: price > $2,000/night (likely data entry errors
  # or ultra-luxury outliers that would distort averages; n = handful of rows)
  filter(price_num <= 2000, price_num >= 10)

# SECTION 5: TAG TREATMENT COUNTIES IN HPI AND ZORI

# Map each show to its county_key and release year
treatment_map <- shows %>%
  mutate(
    county_key = paste0(state_abbr, "_", str_remove(county, " County$"))
  ) %>%
  select(show, release_year, county_key, state_abbr)

# HPI panel with treatment tags 
hpi_panel <- hpi_clean %>%
  left_join(treatment_map, by = "county_key") %>%
  mutate(
    is_treated  = !is.na(show),
    # post = 1 if year >= release year for treated counties
    post        = case_when(
      is_treated  ~ as.integer(year >= release_year),
      TRUE        ~ NA_integer_
    ),
    # years_since_release: negative = pre, 0 = release year, positive = post
    years_since = case_when(
      is_treated  ~ year - release_year,
      TRUE        ~ NA_integer_
    )
  )

#ZORI annual panel with treatment tags
zori_panel <- zori_annual %>%
  left_join(treatment_map, by = "county_key") %>%
  mutate(
    is_treated  = !is.na(show),
    post        = case_when(
      is_treated  ~ as.integer(year >= release_year),
      TRUE        ~ NA_integer_
    ),
    years_since = case_when(
      is_treated  ~ year - release_year,
      TRUE        ~ NA_integer_
    )
  ) %>%
  filter(zori_reliable)    # drop county-years with < 6 months of ZORI data

# SECTION 6: JOINED PANEL (HPI + ZORI, treatment counties only, 2015–2024)

# For the DiD we keep both treated and all available control counties.
# The "treatment_only" panel is used for descriptive visualizations.

treatment_panel <- hpi_panel %>%
  filter(is_treated) %>%
  filter(year >= 2015) %>%
  left_join(
    zori_panel %>% select(county_key, year, zori_mean, zori_reliable),
    by = c("county_key", "year")
  )

# Full panel for DiD (all counties in both HPI and ZORI, 2015–2024)
full_panel <- hpi_panel %>%
  filter(year >= 2015) %>%
  left_join(
    zori_annual %>% select(county_key, year, zori_mean, zori_reliable),
    by = c("county_key", "year")
  )

# SECTION 7: SAVE OUTPUTS
# Create cleaned data folder if it doesn't exist
dir.create("Cleaned Data", showWarnings = FALSE)

save(shows,            file = "Cleaned Data/shows.RData")
save(hpi_clean,        file = "Cleaned Data/hpi_clean.RData")
save(zori_long,        file = "Cleaned Data/zori_long.RData")
save(zori_annual,      file = "Cleaned Data/zori_annual.RData")
save(airbnb_clean,     file = "Cleaned Data/airbnb_clean.RData")
save(treatment_panel,  file = "Cleaned Data/treatment_panel.RData")
save(full_panel,       file = "Cleaned Data/full_panel.RData")

message("Wrangling complete. Cleaned Data/ folder populated.")

# SECTION 8: QUICK DIAGNOSTIC SUMMARY

cat("\n--- Treatment panel summary ---\n")
treatment_panel %>%
  group_by(show, county_key, release_year) %>%
  summarise(
    years        = n(),
    hpi_nas      = sum(is.na(hpi_2000base)),
    zori_nas     = sum(is.na(zori_mean)),
    hpi_range    = paste0(round(min(hpi_2000base, na.rm=TRUE)),
                          "–", round(max(hpi_2000base, na.rm=TRUE))),
    zori_range   = paste0("$", round(min(zori_mean, na.rm=TRUE)),
                          "–$", round(max(zori_mean, na.rm=TRUE))),
    .groups = "drop"
  ) %>%
  print()

cat("\n--- Airbnb Bozeman summary ---\n")
airbnb_clean %>%
  summarise(
    n_listings        = n(),
    median_price      = median(price_num),
    pct_post_yellow   = mean(post_yellowstone, na.rm = TRUE) * 100,
    pct_entire_home   = mean(room_type == "Entire home/apt") * 100
  ) %>%
  print()

