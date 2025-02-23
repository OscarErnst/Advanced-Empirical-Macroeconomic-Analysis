# Clear environment and console
rm(list = ls())
cat("\014")

setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Advanced Macroeconomics - Empirical Analysis")

# Load necessary libraries
library(eurostat)
library(dplyr)
library(purrr)
library(lubridate)
library(New)

source("R kode/Functions.R")
source("R kode/LP functions.R")

# Suppose your 'data' has multiple countries and you only want DE:
data_de <- d %>%
  filter(country == "DE") %>%          # Keep Germany only
  arrange(year, quarter) %>%           # Make sure it's in chronological order
  select(HICP_change, rGDP_change, Unemployment_change)     # Select only the 3 time-series columns

lp_results <- estimateLP(
  data   = data_de,  # Our 3-column time-series data
  h      = 20,        # Horizon: 0 to 4
  p      = 5,        # 2 lags for each variable
  c_case = 1,        # 1 => Include a constant in the regressions
  exdata = NULL,     # No additional exogenous variables
  alpha  = 90,       # 90% confidence intervals
  NWSE   = F      # Newey-West standard errors
)

# Extract arrays
Gamma    <- lp_results$Gamma
Gammalo  <- lp_results$Gammalo
Gammaup  <- lp_results$Gammaup

# Let's say i=2 (rGDP) and j=3 (Unemployment),
# horizon goes from 0 to h-1 (if lp_results$h = 4, then 0..3)
i <- 2
j <- 3
horizon_count <- lp_results$h  # e.g. 4

df_plot <- data.frame(
  horizon = 0:(horizon_count - 1),
  coeff   = Gamma[i, j, ],
  lower   = Gammalo[i, j, ],
  upper   = Gammaup[i, j, ]
)

library(ggplot2)

ggplot(df_plot, aes(x = horizon, y = coeff)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  geom_point() +
  labs(
    title    = "Local Projection: Response of rGDP to Unemployment shock",
    x        = "Horizon (quarters)",
    y        = "Impulse Response"
  ) +
  theme_minimal()




