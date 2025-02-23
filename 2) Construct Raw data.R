# Clear environment and console
rm(list = ls())
cat("\014")

setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Advanced Macroeconomics - Empirical Analysis")

# Load necessary libraries
library(eurostat)
library(dplyr)
library(purrr)
library(lubridate)

source("R kode/Functions.R")

# List of countries: all Eurozone countries plus Denmark
#countries <- c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "EL", "IE", 
#               "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES", "DK")
countries <- "DE"
# Create the full dataset and filter out data beyond 2024 Q3
data <- purrr::map_dfr(countries, ~ get_country_dataset(.x)) 
# Calculate var as percentage change:
d <- calc_pct_change(
  df   = data,
  vars = c("HICP", "rGDP", "Consumption"),
  freq = "qoq"
) %>% 
  filter(year > 1999 & (year < 2024 | (year == 2024 & quarter <= 2)))


