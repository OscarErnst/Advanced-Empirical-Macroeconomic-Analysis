# Clear environment and console
rm(list = ls())
cat("\014")

# Set working directory
setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Advanced Macroeconomics - Empirical Analysis/Advanced-Empirical-Macroeconomic-Analysis")

# Load necessary libraries
library(eurostat)
library(dplyr)
library(purrr)
library(lubridate)

# Source your custom functions
source("Functions/Functions.R")

# List of countries: using Germany ("DE") for this example
countries <- "DE"

# Create the full dataset for the specified country
data <- purrr::map_dfr(countries, ~ get_country_dataset(.x))

# Create a 'date' variable (using the first month of the quarter)
data <- data %>%
  mutate(date = as.Date(paste0(year, "-", (quarter * 3 - 2), "-01")))

# Calculate quarter-on-quarter percentage changes for selected variables
d <- calc_pct_change(
  df   = data,
  vars = c("HICP", "rGDP", "Consumption"),
  freq = "qoq"
) %>% 
  filter(year > 1999 & (year < 2024 | (year == 2024 & quarter <= 2))) %>%
  mutate(date = as.Date(paste0(year, "-", (quarter * 3 - 2), "-01")))

summary(d)
library(dplyr)
library(tidyr)
library(moments)

desc_stats <- d %>%
  select(where(is.numeric)) %>%  # Select all numeric columns
  summarise(
    across(everything(), list(
      Mean = ~ mean(., na.rm = TRUE),
      SD = ~ sd(., na.rm = TRUE),
      Min = ~ min(., na.rm = TRUE),
      Max = ~ max(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Skewness = ~ moments::skewness(., na.rm = TRUE),
      Kurtosis = ~ moments::kurtosis(., na.rm = TRUE),
      N = ~ sum(!is.na(.))
    ))
  ) %>%
  pivot_longer(everything(), names_to = c("Variable", "Statistic"), names_pattern = "(.*)_(.*)", values_to = "Value") %>% #Make it pretty
  pivot_wider(names_from = "Statistic", values_from = Value)

print(desc_stats)
