library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(ggrepel)

# Load cleaned data saved from wrangle.R
load("Cleaned Data/shows.RData")
load("Cleaned Data/hpi_clean.RData")
load("Cleaned Data/zori_annual.RData")
load("Cleaned Data/zori_long.RData")
load("Cleaned Data/treatment_panel.RData")
load("Cleaned Data/airbnb_clean.RData")

# Output folder for saving figures
out <- "Output"
dir.create(out, showWarnings = FALSE)

#Shared theme for all visualizations
theme_netflix <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40", size = 11),
      plot.caption  = element_text(color = "grey55", size = 9),
      axis.title    = element_text(size = 11),
      legend.position = "bottom",
      legend.title  = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      strip.text    = element_text(face = "bold")
    )
}

# One consistent color per show, used across all figures
show_colors <- c(
  "Yellowstone"    = "#8B4513",
  "Breaking Bad"   = "#2E8B57",
  "The White Lotus"= "#1E90FF",
  "Stranger Things"= "#8A2BE2",
  "Outer Banks"    = "#FF8C00",
  "Ozark"          = "#2F4F4F"
)

#FIGURE 1: HPI Trends for Treated Counties (2000–2024) with Release Lines
#Build release year reference lines
release_lines <- shows %>%
  mutate(county_key = paste0(state_abbr, "_", str_remove(county, " County$")))
#Pull HPI for treated counties 2000–2024
hpi_treated <- hpi_clean %>%
  inner_join(release_lines %>% select(show, county_key, release_year, state_abbr),
             by = "county_key") %>%
  filter(!is.na(hpi_2000base))
fig1 <- ggplot(hpi_treated, aes(x = year, y = hpi_2000base, color = show)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.4) +
  #vertical release line per show
  geom_vline(
    data = release_lines %>%
      inner_join(hpi_treated %>% distinct(show), by = "show"),
    aes(xintercept = release_year, color = show),
    linetype = "dashed", linewidth = 0.7, alpha = 0.7
  ) +
  scale_color_manual(values = show_colors, name = "Show / Filming Location") +
  scale_y_continuous(labels = label_number(suffix = "")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  labs(
    title    = "House Price Index in Treated Counties, 2000–2024",
    subtitle = "Dashed lines mark each show's premiere year. Index = 100 in 2000.",
    x        = "Year",
    y        = "HPI (2000 = 100)",
    caption  = "Source: FHFA County House Price Index (All-Transactions, Not Seasonally Adjusted)"
  ) +
  theme_netflix()
ggsave(file.path(out, "fig1_hpi_treated_trends.png"),
       fig1, width = 10, height = 6, dpi = 300)

#FIGURE 2: Annual HPI Change — Pre vs. Post Premiere (Event-Study Style)
#Index each county to 100 in its release year so slopes are comparable
event_study <- hpi_treated %>%
  group_by(show, county_key) %>%
  mutate(
    hpi_release = hpi_2000base[year == release_year][1],  # HPI value in premiere year
    hpi_indexed = (hpi_2000base / hpi_release) * 100,     # re-scale so premiere year = 100
    years_since = year - release_year
  ) %>%
  ungroup() %>%
  # keep 8 years pre and 6 years post. enough context without going too far out
  filter(years_since >= -8, years_since <= 6)

fig2 <- ggplot(event_study, aes(x = years_since, y = hpi_indexed, color = show)) +
  # vertical line at 0 marks the premiere year
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.6) +
  # label sits just right of the vline near the top so it doesn't cover any data
  annotate("text", x = 0.2, y = max(event_study$hpi_indexed, na.rm = TRUE) * 0.99,
           label = "Premiere", hjust = 0, color = "grey40", size = 3.5) +
  scale_color_manual(values = show_colors, name = "Show") +
  scale_x_continuous(breaks = seq(-8, 6, by = 2)) +
  scale_y_continuous(labels = label_number(suffix = "")) +
  labs(
    title    = "Housing Price Trajectory Relative to Show Premiere",
    subtitle = "Each county's HPI re-indexed to 100 in its premiere year.\nYears since premiere on x-axis (0 = premiere year).",
    x        = "Years Since Premiere",
    y        = "HPI (Premiere Year = 100)",
    caption  = "Source: FHFA County House Price Index"
  ) +
  theme_netflix()

ggsave(file.path(out, "fig2_event_study_hpi.png"),
       fig2, width = 10, height = 6, dpi = 300)

# FIGURE 3: ZORI Monthly Rental Trends for Treated Counties
# Pull monthly ZORI for treated counties
treated_keys <- release_lines %>%
  mutate(county_key_zori = paste0(state_abbr, "_", str_remove(county, " County$"))) %>%
  select(show, county_key_zori, release_year)

# filter monthly ZORI down to our 6 counties and drop missing values
zori_treated <- zori_long %>%
  inner_join(treated_keys, by = c("county_key" = "county_key_zori")) %>%
  filter(!is.na(zori))

# geom_vline needs a full Date object, so convert release year to Jan 1 of that year
release_dates <- shows %>%
  mutate(
    release_date  = as.Date(paste0(release_year, "-01-01")),
    county_key    = paste0(state_abbr, "_", str_remove(county, " County$"))
  )

fig3 <- ggplot(zori_treated, aes(x = date, y = zori, color = show)) +
  geom_line(linewidth = 1.05) +
  # only draw vlines for shows that actually appear in the ZORI data
  geom_vline(
    data = release_dates %>% inner_join(zori_treated %>% distinct(show), by = "show"),
    aes(xintercept = release_date, color = show),
    linetype = "dashed", linewidth = 0.7, alpha = 0.7
  ) +
  scale_color_manual(values = show_colors, name = "Show / County") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Monthly Observed Rental Index (ZORI) in Treated Counties",
    subtitle = "Dashed lines mark each show's premiere year. Note: Gallatin (MT) and Maui (HI)\ndata only available from 2021 due to Zillow coverage limits.",
    x        = NULL,
    y        = "ZORI ($/month)",
    caption  = "Source: Zillow Research — Observed Rent Index, County Level"
  ) +
  theme_netflix() +
  # angled labels so years don't overlap on the x-axis
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out, "fig3_zori_treated_trends.png"),
       fig3, width = 11, height = 6, dpi = 300)

#FIGURE 4: HPI — Treated vs. National Median (Control Comparison)
#National median HPI across all counties by year
national_hpi <- hpi_clean %>%
  filter(year >= 2000) %>%
  group_by(year) %>%
  summarise(hpi_national = median(hpi_2000base, na.rm = TRUE), .groups = "drop") %>%
  # give it a show and county_key so it stacks cleanly with the treated data below
  mutate(show = "National Median", county_key = "national")

# stack treated counties and national median into one data frame for plotting
treated_vs_national <- hpi_treated %>%
  select(show, county_key, year, hpi_2000base) %>%
  rename(hpi_national = hpi_2000base) %>%
  bind_rows(national_hpi) %>%
  # line_type drives both thickness and linetype so the national median stands out
  mutate(
    line_type = if_else(show == "National Median", "National Median", "Treated County")
  )

fig4 <- ggplot(treated_vs_national,
               aes(x = year, y = hpi_national,
                   color = show,
                   linewidth = line_type,
                   linetype = line_type)) +
  geom_line() +
  scale_color_manual(
    values = c(show_colors, "National Median" = "black"),
    name   = NULL
  ) +
  # national median is thicker and dotted; treated counties are thinner solid lines
  scale_linewidth_manual(
    values = c("Treated County" = 0.9, "National Median" = 1.5),
    guide  = "none"
  ) +
  scale_linetype_manual(
    values = c("Treated County" = "solid", "National Median" = "dotted"),
    guide  = "none"
  ) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  scale_y_continuous(labels = label_number()) +
  labs(
    title    = "Treated Counties vs. National Median HPI, 2000–2024",
    subtitle = "Black dotted line = national median across all U.S. counties.\nColored lines = counties where shows were filmed.",
    x        = "Year",
    y        = "HPI (2000 = 100)",
    caption  = "Source: FHFA County House Price Index"
  ) +
  theme_netflix()

ggsave(file.path(out, "fig4_treated_vs_national_hpi.png"),
       fig4, width = 10, height = 6, dpi = 300)

# FIGURE 5: Airbnb Bozeman - Price Distribution Pre vs. Post Yellowstone
airbnb_plot <- airbnb_clean %>%
  mutate(
    period = if_else(post_yellowstone, "Post-Premiere (2018+)", "Pre-Premiere (before 2018)")
  ) %>%
  filter(!is.na(period))

fig5 <- ggplot(airbnb_plot, aes(x = price_num, fill = period)) +
  # use density on y-axis so pre/post groups are comparable despite different sizes
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40, alpha = 0.65, position = "identity", color = "white", linewidth = 0.2
  ) +
  # density curve on top to smooth out the shape
  geom_density(aes(color = period), linewidth = 1, fill = NA) +
  # compute medians on the fly and draw a dashed line for each group
  geom_vline(
    data = airbnb_plot %>%
      group_by(period) %>%
      summarise(med = median(price_num), .groups = "drop"),
    aes(xintercept = med, color = period),
    linetype = "dashed", linewidth = 1
  ) +
  # fill and color are separate scales since histogram and lines use different aesthetics
  scale_fill_manual(
    values = c("Pre-Premiere (before 2018)"  = "#B0C4DE",
               "Post-Premiere (2018+)"        = "#8B4513"),
    name = NULL
  ) +
  scale_color_manual(
    values = c("Pre-Premiere (before 2018)"  = "#4682B4",
               "Post-Premiere (2018+)"        = "#5C2A0E"),
    name = NULL
  ) +
  # cap x-axis at $800 — a few extreme outliers were already dropped in wrangle.R
  scale_x_continuous(labels = dollar_format(), limits = c(0, 800)) +
  labs(
    title    = "Airbnb Nightly Price Distribution in Bozeman, MT",
    subtitle = "Pre vs. Post Yellowstone premiere (June 2018). Dashed lines = group medians.\nListings first reviewed before/after premiere date used as proxy for listing age.",
    x        = "Nightly Price (USD)",
    y        = "Density",
    caption  = "Source: Inside Airbnb — Bozeman, MT listings (scraped November 2025)"
  ) +
  theme_netflix()

ggsave(file.path(out, "fig5_airbnb_bozeman_prices.png"),
       fig5, width = 10, height = 6, dpi = 300)

# FIGURE 6: Scatter — Post-Premiere HPI Growth vs. Pre-Premiere HPI Growth
#average annual HPI change in 5 years before and 5 years after premiere
growth_compare <- hpi_treated %>%
  group_by(show, county_key, release_year) %>%
  summarise(
    pre_growth  = mean(annual_change_pct[year >= (release_year - 5) &
                                           year <  release_year], na.rm = TRUE),
    post_growth = mean(annual_change_pct[year >= release_year &
                                           year <= (release_year + 5)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # drop any county that doesn't have data in both windows
  filter(!is.na(pre_growth), !is.na(post_growth))

fig6 <- ggplot(growth_compare, aes(x = pre_growth, y = post_growth, color = show)) +
  # 45-degree line = no change between pre and post; above it means growth accelerated
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey60", linewidth = 0.9) +
  geom_point(size = 5, alpha = 0.9) +
  # ggrepel keeps labels from overlapping each other
  geom_label_repel(
    aes(label = show),
    size = 3.5, fontface = "bold",
    box.padding = 0.4, point.padding = 0.3,
    show.legend = FALSE
  ) +
  # plain text annotation to help reader interpret the reference line
  annotate("text", x = 0.5, y = 11.5,
           label = "Above line = faster growth post-premiere",
           hjust = 0, color = "grey40", size = 3.3) +
  scale_color_manual(values = show_colors, guide = "none") +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title    = "Pre- vs. Post-Premiere Annual HPI Growth in Treated Counties",
    subtitle = "Each point = average annual HPI % change in 5 years before vs. 5 years after premiere.\nPoints above the dashed line grew faster after the show premiered.",
    x        = "Avg. Annual HPI Growth — Pre-Premiere (5 yrs)",
    y        = "Avg. Annual HPI Growth — Post-Premiere (5 yrs)",
    caption  = "Source: FHFA County House Price Index"
  ) +
  theme_netflix()
ggsave(file.path(out, "fig6_pre_post_growth_scatter.png"),
       fig6, width = 9, height = 7, dpi = 300)

