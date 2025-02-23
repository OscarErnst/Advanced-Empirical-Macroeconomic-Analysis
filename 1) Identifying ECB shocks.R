rm(list=ls())
# --

#--------------------------------------------------
# Loading packages:
library(readxl)
library(dplyr)

setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Advanced Macroeconomics - Empirical Analysis")

#--------------------------------------------------
# EA_MPD - Euro Area Monetary Policy event study Database
#
# This dataset is split into three sheets, each representing a specific event window:
#
# 1. Press Release Window:
#    - Measures the change in the median quote for various OIS maturities.
#    - Uses the pre-press release window (13:25–13:35 CET) and post-press release window (14:00–14:10 CET).
#    - Captures the immediate market reaction to the ECB's press release.
#
# 2. Press Conference Window:
#    - Measures the change in the median quote for various OIS maturities.
#    - Uses the pre-press conference window (14:15–14:25 CET) and post-press conference window (15:40–15:50 CET).
#    - Captures the market's response to the additional information provided during the ECB's press conference.
#
# 3. Monetary Event Window:
#    - Aggregates the overall event impact on the market.
#    - Uses the full window from before the press release (13:25–13:35 CET) to after the press conference (15:40–15:50 CET).
#    - Provides an overall view of the market reaction to the ECB policy event.
#--------------------------------------------------
file_path <- "Data/Dataset_EA-MPD.xlsx"

mp <- read_excel(file_path)

# Specify the sheet names you want to load
sheet_names <- c("Press Release Window", "Press Conference Window", "Monetary Event Window")

# Load the sheets into a list
EA_MPD <- lapply(sheet_names, function(sheet) read_excel(file_path, sheet = sheet))

# Name each element in the list according to its sheet name
names(EA_MPD) <- sheet_names

rm(file_path, sheet_names)


