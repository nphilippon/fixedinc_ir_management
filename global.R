library(shiny)
library(shinydashboard) # cool package for making dashboards easily
library(quantmod)
library(tidyverse)
library(tidyquant)
library(DT)
library(RTL)
library(plotly)
library(shinyjs)
library(bslib)

# FRED US Treasury Symbols (name = readable clean tenor, character = FRED symbol)
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

# Function for getting and cleaning FRED data using Tidyquant
get_treasury_data <- function(symbols) {
  raw_data <- symbols %>% 
    # Only search for FRED symbol
    as.character() %>% 
    set_names() %>% 
    # Loops through all symbols and gets data from 1992-01-02 on, because no data for 1992-01-01
    # returns as long df with added col for corresponding symbol
    # (This does take awhile, there might be a better way to do it? Or FRED API is just slow)
    map_df(~tq_get(.x, get = "economic.data", from = "1992-01-02"), .id = "symbol")
  
  # Makes Map tibble for converting FRED symbols to clean names
  symbol_map <- tibble(
    symbol = as.character(symbols),
    tenor = names(symbols)
  )
  
  # Joins raw data with clean name map and converts to wide df of yields by date for each tenor
  clean_data <- raw_data %>% 
    left_join(symbol_map, by = "symbol") %>% 
    select(date, tenor, price) %>% 
    pivot_wider(names_from = tenor, values_from = price) %>% 
    # Sorts by date descending
    arrange(desc(date)) %>% 
    # Fills missing dates with last available
    fill(everything(), .direction = "up")
  
  return(clean_data)
}

# Pull data on launch
treasury_yields <- get_treasury_data(treasury_symbols)

# NOTE: CF function does not work properly rn, payments are made every 2 years instead of semi-annual
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

# Function for calculating TTM (moved to its own so we can use either dates or ttm input)
get_bond_ttm <- function(settlement_date, maturity_date) {
  # settlement_date: date of bond ownership transfer
  # maturity_date: maturity date of bond
  ttm <- as.numeric(difftime(maturity_date, settlement_date, units = "days")) / 365
  
  return(ttm)
}


# Function for calculating bond metrics
get_bond_metrics <- function(ttm, FV = 100, yield, c, periodicity = 2) {
  # ttm: time to maturity
  # FV: face/par value of bond (defaults to 100)
  # yield: yield of bond (decimal format)
  # c: coupon rate of bond (decimal format)
  # periodicity: coupon payment frequency (defaults to 2, semi-annual)
  
  # Calculate # of payment periods remaining
  n_pmt_periods <- floor(ttm * periodicity)
  
  # Calculate time of each coupon payment (in years from settlement date)
  pmt_times <- seq(from = 1/periodicity, length.out = n_pmt_periods, by = 1/periodicity)
  # Set last payment to maturity date
  pmt_times[length(pmt_times)] <- ttm
  
  # Calculate Coupon payment
  c_pmt <- (FV * c)/periodicity
  # Generate Cash Flow stream
  cashflows <- rep(c_pmt, length(pmt_times))
  # Add final bond redemption cash flow
  cashflows[length(cashflows)] <- cashflows[length(cashflows)] + FV
  
  # Calculate PV of Cash Flows for entered yield
  pv_cashflows <- cashflows * (1/ (1 + yield/periodicity)^(periodicity * pmt_times))
  
  # Internal function for repricing bond given yield
  get_bond_price <- function(yield) {
    # Calculate PV of Cash Flows
    pv_cashflows <- cashflows * (1/ (1 + yield/periodicity)^(periodicity * pmt_times))
    # Calculate bond price (sum of cash flow PVs)
    bond_price <- sum(pv_cashflows)
  }
  
  # Calculate bond price using function above
  bond_price <- get_bond_price(yield) 
  
  # Traditional Bond Metrics
  # Calculate Macaulay Duration
  macaulay_duration <- sum(pmt_times * pv_cashflows) / bond_price
  # Calculate Modified Duration
  modified_duration <- macaulay_duration / (1 + yield/periodicity)
  # Calculate Convexity
  convexity <- sum(pmt_times * (pmt_times + 1/periodicity) * pv_cashflows) / (bond_price * (1 + yield / periodicity)^2)
  
  # Modern Bond Metrics
  # Re-Price with plus and minus step size 
  step_size = 0.0001 # Set to 1bp (0.01%)
  price_plus <- get_bond_price(yield + step_size)
  price_minus <- get_bond_price(yield - step_size)
  
  
  return(list(
    n_pmt_periods = n_pmt_periods,
    pmt_times = pmt_times,
    c_pmt = c_pmt,
    cashflows = cashflows,
    pv_cashflows = pv_cashflows,
    bond_price = bond_price,
    macaulay_duration = macaulay_duration,
    modified_duration = modified_duration,
    convexity = convexity,
    price_plus = price_plus,
    price_minus = price_minus))
}

# Testing functions
test_bond_ttm <- get_bond_ttm(settlement_date = "2016-01-01", maturity_date = "2026-01-01")
test_bond_metrics <- get_bond_metrics(ttm = 10, yield = 0.05, c = 0.05)
test_bond_cf <- bond_cf(start_date = "2020-01-01", end_date = "2026-01-01", c = 0.05, FV = 100)

# Pull US Treasury data on startup 
treasury_yields <- get_treasury_data(treasury_symbols)



#Portfolio Builder Functions

init_new <- function(){
  
  temp_build <- tibble::tibble(length = 0,
                               coupon_rate = 0,
                               TTM = 0,
                               N = 0,
                               Face_Value = 0,
                               Quantity = 0,
                               Nominal_Value = 0)
  
  temp_build
  
}


add_row <- function(start, end, coupon, periodocity, FV, quantity){
  
  T2M <- as.numeric(as_date(end)) - as.numeric(as_date(Sys.Date()))
  
  years <- (as.numeric(as_date(end)) - as.numeric(as_date(start)))/365
  
  
  
  temp_build <- tibble::tibble(length = years,
                               coupon_rate = coupon,
                               TTM = T2M,
                               N = periodocity,
                               Face_Value = FV,
                               Quantity = quantity,
                               Nominal_Value = quantity*FV)
  
  temp_build
  
}




























