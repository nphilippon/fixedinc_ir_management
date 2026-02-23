library(shiny)
library(shinydashboard) # cool package for making dashboards easily
library(quantmod)
library(tidyverse)
library(tidyquant)
library(DT)

# FRED US Treasury Symbols
treasury_symbols <- c(
  "1-Month"  = "DGS1MO",
  "3-Month"  = "DGS3MO",
  "6-Month"  = "DGS6MO",
  "1-Year"   = "DGS1",
  "2-Year"   = "DGS2",
  "3-Year"   = "DGS3",
  "5-Year"   = "DGS5",
  "7-Year"   = "DGS7",
  "10-Year"  = "DGS10",
  "20-Year"  = "DGS20",
  "30-Year"  = "DGS30")

# Get FRED data
get_treasury_data <- function(symbols) {
  # Get data using tidyquant
  raw_data <- symbols %>% 
    as.character() %>% 
    set_names() %>% 
    map_df(~tq_get(.x, get = "economic.data", from = "1992-01-01"), .id = "symbol")
  
  # Map FRED symbols to clean names
  symbol_map <- tibble(
    symbol = as.character(symbols),
    tenor = names(symbols)
  )
  
  # Convert to wide df
  clean_data <- raw_data %>% 
    left_join(symbol_map, by = "symbol") %>% 
    select(date, tenor, price) %>% 
    pivot_wider(names_from = tenor, values_from = price) %>% 
    arrange(desc(date))
  
  return(clean_data)
}

# Pull data on launch
treasury_yields <- get_treasury_data(treasury_symbols)


# Create bond
bond_cf <- function(start_date, end_date = NA, c, T2M = 0, periodicity = 2, FV, quantity = 1) {
  # Start_date: beginning date for the bond, the inception and first payment date (CRITICAL)
  # End date: date when last payment is made
  # C: coupon RATE (%) for CF stuff
  # T2M: user can provide either start and end date, or just start date and T2M. Defaults to 0, in years.
  # Periodicity: number of payments per year, default semi-annual payments 
  # FV: face value
  # Quantity: How many of those bonds do you have, defaults to 1.
  
  
  # Makes sure the start and end dates are actually dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # 2 different seq.Dates because it can either be start date + T2M or start to end date
  if (T2M != 0 & is.na(end_date)){
    date_col <- seq.Date(start_date, start_date + years(T2M), by = "day")
  }
  else {
    date_col <- seq.Date(start_date, end_date, by = "day")
  }
  
  
  # Gets the notional value of payments, just coupon * Face value.
  payment_notional <- FV * c
  
  # Gets payment months
  # Assumes first payment is on the START DATE !!!
  months_between <- 12 / periodicity # How many months between payments
  start_month <- month(start_date)
  start_day <- day(start_date)
  
  pmt_months <- c() # initializes empty vector, this gets filled by the for loop below
  
  for (num in seq(1, periodicity)) {
    # vector that contains the numerical value of the payment months.
    # start_month is the month of the start_date input
    # months_betweeen is how many months between payments, its multiplied by
    # num because we need 3 (3 * 1) months, then 6 (3 * 2) months, then 9 (3 * 3), then 12 (3 * 4) months ahead
    # mod 12 because im not silly and remembered there are 12 months in a year
    pmt_months[num] <- (start_month + (months_between * (num - 1))) %% 12
  }
  
  output_df <- data.frame(date = date_col)
  
  output_df <- output_df %>%
    dplyr::mutate(payment = case_when(
      date == end_date ~ payment_notional + FV,
      month(date) %% 12 %in% pmt_months & day(date) == day(start_date) ~ payment_notional,
      TRUE ~ 0
    ))
  
  return(output_df)
}
