
# Difference-in-Differences Analysis
# What this script does:
#   1. Loads the cleaned HPI panel and the six-show treatment crosswalk
#   2. Builds a county-year DiD panel with treatment indicators
#   3. Estimates three two-way fixed effects models:
#        m1 = baseline DiD on HPI level
#        m2 = robustness: drop COVID-era premieres
#        m3 = robustness: outcome = annual % change instead of level
#   4. Saves a regression table and the fitted model objects


library(tidyverse)
library(fixest)        # feols() for fast fixed-effects regressions (rather than lm)
library(modelsummary)  # nicely formatted regression tables

# 1. LOAD DATA

# shows: six treated counties + show name + premiere year
# hpi_clean: full FHFA county HPI panel, 2000-2024 (~2,400 counties)
load("Cleaned Data/shows.RData")
load("Cleaned Data/hpi_clean.RData")

# Output folder for the regression table
out <- "Output"
dir.create(out, showWarnings = FALSE)

# 2. BUILD THE DiD PANEL

# every county-year row in hpi_clean gets two flags:
#   treated = 1 if the county is one of the six filming counties (in any year)
#   post = 1 if the county is treated AND year >= its own premiere year

#staggered DiD design:
#   - treated counties before premiere: treated=1, post=0
#   - treated counties after premiere: treated=1, post=1   <- the treated obs
#   - all control counties (every year): treated=0, post=0   <- the controls

# str_remove(...) strips the trailing " County" so "Gallatin County" becomes
# "Gallatin", matching the county_key format used in hpi_clean.
treatment_map <- shows %>%
  mutate(county_key = paste0(state_abbr, "_", str_remove(county, " County$"))) %>%
  select(show, release_year, county_key)

# Join to the full HPI panel.
# left_join keeps every county-year; treated counties get a non-NA `show`,
# control counties get NA for `show` and `release_year`.
did_panel <- hpi_clean %>%
  filter(!is.na(hpi_2000base)) %>%                 # drop county-years with no HPI
  left_join(treatment_map, by = "county_key") %>%
  mutate(
    treated = as.integer(!is.na(show)),
    post    = if_else(treated == 1 & year >= release_year, 1L, 0L)
  )

# Sanity check: confirm panel size and that we have all six treated counties
cat("Panel dimensions:\n")
cat("  Total county-years:", nrow(did_panel), "\n")
cat("  Unique counties:   ", n_distinct(did_panel$county_key), "\n")
cat("  Treated counties:  ", n_distinct(did_panel$county_key[did_panel$treated == 1]), "\n\n")


# 3. MODEL 1 - BASELINE DiD WITH TWO-WAY FIXED EFFECTS
# fixest syntax:  outcome ~ regressors | fixed effects
m1 <- feols(
  hpi_2000base ~ post | county_key + year,
  data    = did_panel,
  cluster = ~ county_key
)

cat("=== Model 1: Baseline DiD (HPI level) ===\n")
print(summary(m1))


# 4. MODEL 2 - ROBUSTNESS: DROP COVID-ERA PREMIERES

# Outer Banks (2020) and The White Lotus (2021) premiered into the COVID
# housing boom. If the headline effect is really just the pandemic, dropping
# these two should shrink it.

# keep the row if EITHER the show is not one of these two, OR the
# row is a control county (show is NA). The `| is.na(show)`  is
# key — without it, all 2,408 control counties would be dropped,
# because `NA %in% c(...)` returns NA, and filter treats NA as FALSE.

did_no_covid <- did_panel %>%
  filter(!(show %in% c("Outer Banks", "The White Lotus")) | is.na(show))

m2 <- feols(
  hpi_2000base ~ post | county_key + year,
  data    = did_no_covid,
  cluster = ~ county_key
)

cat("\n=== Model 2: Excluding COVID-era premieres ===\n")
print(summary(m2))

# 5. MODEL 3 - ROBUSTNESS: ANNUAL % CHANGE AS OUTCOME
# Same DiD specification, different outcome variable. annual_change_pct is
# the year-over-year % change in HPI. 

m3 <- feols(
  annual_change_pct ~ post | county_key + year,
  data    = did_panel,
  cluster = ~ county_key
)

cat("\n=== Model 3: Annual HPI growth rate ===\n")
print(summary(m3))


# 6. SAVE REGRESSION TABLE AND MODEL OBJECTS
modelsummary(
  list(
    "HPI Level"            = m1,
    "HPI Level (no COVID)" = m2,
    "Annual Growth %"      = m3
  ),
  stars    = TRUE,
  gof_omit = "AIC|BIC|Log|Adj|Within|Pseudo",
  output   = file.path(out, "did_results_table.html"),
  title    = "Difference-in-Differences: Effect of TV Show Premiere on Local Housing Prices"
)

save(m1, m2, m3, file = "Cleaned Data/did_models.RData")

cat("\nDone. Regression table saved to", file.path(out, "did_results_table.html"), "\n")
