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

# ----------------------------
# Plot 1: Original Series (Levels/Index)
# ----------------------------
# Open PDF device (or use windows()/quartz() for interactive plotting)
pdf("Figures/EuroArea_Levels.pdf", width = 8, height = 5)

# Plot HICP in blue
plot(data$date, data$HICP, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Value", 
     main = "Euro Area Series in Levels/Index")
# Add rGDP in red
lines(data$date, data$rGDP, col = "red", lwd = 2)
# Add Consumption in green
lines(data$date, data$Consumption, col = "green", lwd = 2)
# Add legend
legend("topright", legend = c("HICP", "rGDP", "Consumption"),
       col = c("blue", "red", "green"), lwd = 2)
dev.off()

# ----------------------------
# Plot 2: Quarter-on-Quarter Log Changes
# ----------------------------
pdf("Figures/EuroArea_QoQ_Changes.pdf", width = 8, height = 5)

# Plot HICP_change in blue
plot(d$date, d$HICP_change, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Log Change (%)",  # Changed y-axis label
     main = "Quarter-on-Quarter Log Changes")
# Add rGDP_change in red
lines(d$date, d$rGDP_change, col = "red", lwd = 2)
# Add Consumption_change in green
lines(d$date, d$Consumption_change, col = "green", lwd = 2)
# Add legend
legend("topleft", legend = c("HICP", "rGDP", "Consumption"),
       col = c("blue", "red", "green"), lwd = 2)
dev.off()
