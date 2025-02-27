library(eurostat)
library(dplyr)
library(lubridate)
library(purrr)

# ----------------------------------------------------------------
# 1) HICP (monthly data) -> group monthly to quarterly
# ----------------------------------------------------------------
get_HICP <- function(geo) {
  message("Loading HICP for country: ", geo)
  
  HICP_data <- suppressMessages(
    get_eurostat(
      "prc_hicp_midx",
      time_format = "date",
      filters = list(
        geo    = geo,
        coicop = "CP00",
        unit   = "I15"
      )
    )
  )
  
  HICP_data <- HICP_data %>%
    mutate(
      year    = lubridate::year(time),
      quarter = lubridate::quarter(time)
    ) %>%
    group_by(year, quarter) %>%
    summarise(
      HICP = mean(values, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      year_quarter = paste0(year, "-Q", quarter)
    ) %>%
    select(year, quarter, year_quarter, HICP)
  
  return(HICP_data)
}

# ----------------------------------------------------------------
# 2) Real GDP (quarterly data) -> can just take as is, or group
# ----------------------------------------------------------------
get_real_GDP <- function(geo) {
  message("Loading Real GDP for country: ", geo)
  
  rGDP_data <- suppressMessages(
    get_eurostat(
      "naidq_10_gdp",
      time_format = "date",
      filters = list(
        geo    = geo,
        na_item= "B1GQ",
        s_adj  = "SCA",
        unit   = "CLV_I10"
      )
    )
  )
  
  # we’ll still mutate year/quarter for consistency:
  rGDP_data <- rGDP_data %>%
    mutate(
      year    = lubridate::year(time),
      quarter = lubridate::quarter(time)
    ) %>%
    group_by(year, quarter) %>%
    summarise(
      rGDP = mean(values, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      year_quarter = paste0(year, "-Q", quarter)
    ) %>%
    select(year, quarter, year_quarter, rGDP)
  
  return(rGDP_data)
}

# ----------------------------------------------------------------
# 3) Final Consumption by Households (quarterly data)
# ----------------------------------------------------------------
get_final_consumption <- function(geo) {
  message("Loading Final Consumption by Households for country: ", geo)
  
  # Fetch from "namq_10_fcs" with your desired filters
  consumption_data <- suppressMessages(
    get_eurostat(
      "namq_10_fcs",
      time_format = "num",   # numeric quarters like 2000.0, 2000.25, etc.
      filters = list(
        geo    = geo,
        na_item= "P31_S14",   # Final consumption expenditure of households
        s_adj  = "SCA",       # Seasonally and calendar adjusted
        unit   = "CLV10_MEUR" # Chain-linked volumes, 2010 reference, million EUR
      )
    )
  )
  
  # Convert numeric "time" to (year, quarter)
  # e.g., 2020.0 = Q1 2020, 2020.25 = Q2, etc.
  consumption_data <- consumption_data %>%
    mutate(
      year = floor(time),
      # If time is 2000.75, the decimal part is 0.75 => Q4
      # 0.00 => Q1, 0.25 => Q2, 0.50 => Q3, 0.75 => Q4
      quarter = as.integer(1 + (time - floor(time)) * 4)
    ) %>%
    group_by(year, quarter) %>%
    summarise(
      Consumption = mean(values, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      year_quarter = paste0(year, "-Q", quarter)
    ) %>%
    select(year, quarter, year_quarter, Consumption)
  
  return(consumption_data)
}
# ----------------------------------------------------------------
# 4) Merge all three variables into one data frame
# ----------------------------------------------------------------
get_country_dataset <- function(geo) {
  # 1) Get HICP
  hicp <- get_HICP(geo)
  
  # 2) Get Real GDP
  rgdp <- get_real_GDP(geo)
  
  # 3) Get Household Consumption 
  consumption <- get_final_consumption(geo)
  
  message("Merging HICP, GDP, and Final Consumption for country: ", geo)
  
  # 4) Merge them into one data frame by (year, quarter, year_quarter)
  df_merged <- list(hicp, rgdp, consumption) %>%
    reduce(left_join, by = c("year", "quarter", "year_quarter"))
  
  # 5) Add a column for the country name/code
  df_merged <- df_merged %>%
    mutate(country = geo)
  
  return(df_merged)
}

calc_pct_change <- function(df,
                            vars,
                            freq  = c("qoq", "yoy"),
                            suffix = "_change") {
  # freq can be "qoq" (quarter-over-quarter) or "yoy" (year-over-year)
  freq <- match.arg(freq)
  
  # Group (by country, for example) and sort by time:
  df <- df %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(year, quarter, .by_group = TRUE)
  
  # Decide how many periods to lag:
  # - "qoq" => lag by 1 quarter
  # - "yoy" => lag by 4 quarters (same quarter last year)
  lag_n <- if (freq == "qoq") 1 else 4
  
  # Loop through each variable; create new column: "<var>_change"
  for (v in vars) {
    new_col <- paste0(v, suffix)  # e.g. "rGDP_change"
    df <- df %>%
      dplyr::mutate(
        !!new_col := 100 * (log(.data[[v]]) - dplyr::lag(log(.data[[v]]), lag_n))
      )
  }
  
  # Ungroup before returning
  df <- dplyr::ungroup(df)
  
  return(df)
}


create_qoq_plot <- function(data, filename = "EuroArea_QoQ_Changes.pdf", width = 8, height = 5) {
  # 'data' is your data frame, must have 'date', 'HICP_change', 'rGDP_change', 'Consumption_change'
  # 'filename' is the output file name (default is "EuroArea_QoQ_Changes.pdf")
  # width and height control figuere size
  
  pdf(filename, width = width, height = height)
  
  # Plot HICP_change in blue
  plot(data$date, data$HICP_change, type = "l", col = "blue", lwd = 2,
       xlab = "Date", ylab = "Log Change (%)",
       main = "Quarter-on-Quarter Log Changes")
  # Add rGDP_change in red
  lines(data$date, data$rGDP_change, col = "red", lwd = 2)
  # Add Consumption_change in green
  lines(data$date, data$Consumption_change, col = "green", lwd = 2)
  # Add legend
  legend("topleft", legend = c("HICP", "rGDP", "Consumption"),
         col = c("blue", "red", "green"), lwd = 2)
  
  dev.off()
}




