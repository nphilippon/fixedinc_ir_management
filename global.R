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
library(rstudioapi) #using this to set working directory to wherever this file is located (I think there may be something better)
library(splines)
library(RcppRoll)
library(Rcpp)

# C++ Calculation Function
sourceCpp("bond_calcs.cpp")

# FRED US Treasury Symbols
treasury_symbols <- c(
  "DGS1MO", "DGS3MO", "DGS6MO",
  "DGS1", "DGS2", "DGS3",
  "DGS5", "DGS7", "DGS10",
  "DGS20", "DGS30")

# Be SURE that this vector corresponds 1:1 with the one above, shit will brick otherwise.
treasury_names <- c(
  "1-Month", "3-Month", "6-Month",
  "1-Year", "2-Year", "3-Year",
  "5-Year", "7-Year", "10-Year",
  "20-Year", "30-Year"
)

# This also needs to correspond 1:1
treasury_maturities <- c(
  round(1/12,3), 3/12, 6/12,
  1, 2, 3,
  5, 7, 10,
  20, 30
)

# A function to pull the treasury data
get_treasury_data <- function(symbols, names, maturities, from = "1992-01-02", dim = "long"){
  # Symbols: tickers for pulling, must be economic.data
  # Names: Vector. Names to swap out the tickers with, only compatible with wide df
  # from: date to start pulling from, default is 1992-01-02
  # dim: Dimension, either "long" or "wide", default is long
  
  output <- tq_get(symbols, get = "economic.data", from = from) %>%
    dplyr::mutate(
      maturity = maturities[match(symbol, symbols)],
      rate = price / 100) %>% 
    na.omit() %>% 
    arrange(date) %>% 
    dplyr::select(symbol, maturity, date, rate)
  
  return(output)
}


# Pull data on launch
treasury_yields <- get_treasury_data(treasury_symbols, maturities = treasury_maturities)

treasury_yields_wide <- treasury_yields %>% 
  dplyr::select(-maturity) %>% 
  tidyr::pivot_wider(., names_from = symbol, values_from = rate) %>%
  dplyr::rename(
    "1-Month" = "DGS1MO", 
    "3-Month" = "DGS3MO", 
    "6-Month" = "DGS6MO",
    "1-Year" = "DGS1", 
    "2-Year" = "DGS2", 
    "3-Year" = "DGS3",
    "5-Year" = "DGS5", 
    "7-Year" = "DGS7", 
    "10-Year" = "DGS10",
    "20-Year" = "DGS20", 
    "30-Year" = "DGS30") %>% 
  dplyr::arrange(desc(date))
    
treasury_yields_named <- treasury_yields_wide %>% 
  tidyr::pivot_longer(cols = -date, names_to = "tenor", values_to = "rate") %>% 
  dplyr::mutate(tenor = factor(tenor, levels = treasury_names))

# Function for getting expanded yield data
get_yield_metrics <- function(yields, m = 2, price = 100) {
  # yields: dataframe containing yield data with cols maturity, date, rate
  # m: coupon rate (defaults to 2)
  # price: bond price (defaults to 100)
  
  # Get simple metrics using R
  yields_simple_metrics <- yields %>% 
    group_by(maturity) %>% 
    arrange(date) %>% 
    dplyr::mutate(
      # Daily change (in bps)
      changeBasisPoints = (rate - lag(rate)) * 10000,
      # Standard Deviation
      sd = roll_sd(changeBasisPoints, n = 4, align = "right", fill = NA)
      ) %>% 
    ungroup()
  
  # Get more complex metrics using C++ function
  yields_complex_metrics <- cpp_get_portfolio_metrics(
    ttm = yields_simple_metrics$maturity,
    yield = yields_simple_metrics$rate,
    c = yields_simple_metrics$rate,   # ASSUMES COUPON RATE = YIELD
    FV = price,
    periodicity = m
  )
  
  # Combine 
  yields_metrics <- yields_simple_metrics %>% 
    bind_cols(yields_complex_metrics) %>% 
    mutate(
      # Set NAs to 0 and renames cols
      duration = ifelse(is.na(macaulay_duration), 0, macaulay_duration),
      convexity = ifelse(is.na(convexity), 0, convexity),
      delta = ifelse(is.na(delta_central_approx), 0, delta_central_approx),
      gamma = ifelse(is.na(gamma_approx), 0, gamma_approx),
      
      # Add back constant columns (to match 5.2 df)
      m = m,
      price = price
    ) %>% 
    dplyr::select(maturity, date, rate, changeBasisPoints, sd, m, price, duration, convexity, delta, gamma)
  
  return(yields_metrics)
}


# NOTE: CF function does not work properly rn, payments are made every 2 years instead of semi-annual
# Create bond
bond_cf <- function(start_date, end_date = NA, c, ytm,  T2M = 0, periodicity = 2, FV, quantity = 1) {
  # Start_date: beginning date for the bond, the inception and first payment date (CRITICAL)
  # End date: date when last payment is made
  # C: coupon RATE (%) for CF stuff
  # ytm: Yield to market rate.
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
  
  
  # Gets the notional value of payments, just coupon * Face value * quantity.
  payment_notional <- FV * (c / periodicity) * quantity
  
  # Gets payment months
  # Assumes first payment is on the START DATE !!!
  months_between <- 12 / periodicity # How many months between payments
  start_month <- month(start_date) # Month of the starting date
  start_day <- day(start_date) # Day of the starting date
  
  pmt_months <- c() # initializes empty vector, this gets filled by the for loop below
  
  for (num in seq(1, periodicity)) {
    # vector that contains the numerical value of the payment months.
    # start_month is the month of the start_date input
    # months_betweeen is how many months between payments, its multiplied by
    # num because we need 3 (3 * 1) months, then 6 (3 * 2) months, then 9 (3 * 3), then 12 (3 * 4) months ahead
    # mod 12 because im not silly and remembered there are 12 months in a year
    pmt_months[num] <- (start_month + (months_between * (num - 1))) %% 12
  }
  
  # Initializes the output df, just has date to starat becuase its easy
  output_df <- data.frame(date = date_col)
  
  # Adds in all the pther columns we need.
  output_df <- output_df %>%
    dplyr::mutate(
      payment = case_when(
      date == end_date ~ payment_notional + FV, # Last payment
      month(date) %% 12 %in% pmt_months & day(date) == day(start_date) ~ payment_notional, # %% 12 because we want 12 to be 0.
      TRUE ~ 0 # Non-payment days
    ),
    T2M = as.numeric(difftime(end_date, date, units = "days"))/365, # Difference between end and today, in years
    years_past = as.numeric(difftime(date, start_date, units = "day"))/365, # Difference between start and today, in years
    discount_factor = 1 / (1 + ytm/periodicity)^(years_past*periodicity), # Discount facotr for the cashflows
    pv_cf = payment * discount_factor) %>% # Present value of the cf
    dplyr::filter(payment != 0)
  
  return(output_df)
}

bond_cfs <- function(start_dates, end_dates, coupons, periodicities, face_values, quantities) {
  # Start_dates: Vector, starting dates (first payments), in date format for all the bonds
  # End_dates: Vector, ending dates (final payments), in date format for all the bnods
  # Coupons: Vector, numerical coupon rates for the bonds (MUST BE SAME TIME FRAME COMPOUNDING)
  # Periodicities: Vector, numerical. Number of payments within a year.
  # Face_values: Vector, numerical. Face value of each individual bond. 
  # Quantities: Vector, numerical amount of each bond owned in the portfolio
  # If you have questions about this function or need it modified, text Alex. 
  
  
  # Makes sure everything is in its proper format and class
  start_dates <- as.Date(start_dates)
  end_dates <- as.Date(end_dates)
  
  coupons <- as.numeric(coupons)
  periodicities <- as.numeric(periodicities)
  face_values <- as.numeric(face_values)
  quantities <- as.numeric(quantities)
  # -------------------------------------------------------
  
  # Global variables for absolute max and min date for the building of the tibble a bit later
  min_date <- min(start_dates)
  max_date <- max(end_dates)
  num_days <- as.numeric(difftime(max_date, min_date, units = c('days')))
  first_payments <- as.numeric(day(start_dates))
  # -----------------------------------------------------------------------------------------
  
  
  # Initializes and fills out vector of payment amounts in notional terms.
  payment_notional <- c()
  for (bond in 1:length(quantities)){
    # formula is: (c/m) * FV * Quantity
    payment_notional[bond] <- (coupons[bond]/periodicities[bond]) * face_values[bond] * quantities[bond]
  }
  # ----------------------------------------------------------------------
  
  # Initializes and fills out vector of payment months (this is a critical part, please read carefully beore changing shit)
  payment_months <- list()
  for (bond in 1:length(quantities)){
    months_between <- 12/periodicities[bond] # How many months between payments
    start_month <- as.numeric(month(start_dates[bond])) %% 12 # Month of the first payment
    
    payment_months[[bond]] <- unique(seq(start_month, start_month + 12, by = months_between) %% 12)
  }
  # -----------------------------------------------------------------------------------------------------------------------
  
  # Initalizes date column for the df!!!!
  date_col <- seq.Date(min_date, max_date, by = "day")
  # -------------------------------------
  
  # Creates the bond cashflow columns, its complicated becuase they all need to be 
  # O(n^2) is bad i know, im sorry D:
  bond_columns <- list()
  for (num in 1:length(quantities)){
    bond <- c() # Empty vector that gets updated then added to the whole list of other bonds, it gets reset every time
    
    # Sets all the bond-dependant variables for ease of reading and debugging. 
    pmt <- payment_notional[num] # Payment amount of the bond we're currently working on
    fv <- face_values[num] # Face value of the urrent bond
    pmt_months <- payment_months[[num]] # Vector of the payment months
    
    p1 <- start_dates[num] # Date of the first payment for the bond
    pn <- end_dates[num] # Date the bond is done.
    
    for (date in 1:length(date_col)){
      
      # Sets the date-dependant variables for ease of reading and debugging
      curr_date <- date_col[date]
      curr_month <- month(curr_date)
      
      
      bond[date] = case_when(
        (curr_date < p1 | curr_date > pn) ~ 0, # Before the bond begins or after it ends
        curr_date == pn ~ (pmt + fv), # If its the last payment date
        ((month(curr_date) %% 12) %in% pmt_months) & (day(curr_date) == day(p1)) ~ pmt, # Its the correct payment month, on the payment date. 
        TRUE ~ 0 # All other cases
      )
    }
    bond_columns[[paste0("B", num)]] <- bond # Adds the current bond we just made to the big list of bonds we already have :-)
  }
  # ------------------------------------------------------------------------------
  
  # Final output loop, this was written at 11:30pm so please be forgiving
  output_df <- data.frame(date = date_col)
  
  for (bond in 1:length(bond_columns)){
    # Finds the name of the bond, should be like "B1", "B2", "B3" and so on....
    col_name <- names(bond_columns)[bond]
    
    output_df <- output_df %>%
      dplyr::mutate(!!col_name := bond_columns[[bond]]) # God bless the whalrus, it doesnt work if you just use a normal = sign
    
  }
  output_df <- output_df %>%
    dplyr::mutate(cf = rowSums(across(starts_with("B"))))
  
  # Hello Mathew Frame if you're reading this
  return(output_df)
}

system.time(
TEST_BOND_CFS <- bond_cfs(start_dates = c("2026-01-01", "2025-06-19", "2024-12-25", "2023-04-22"), 
                          end_dates = c("2032-12-31", "2028-02-14", "2029-12-24", "2031-01-01"), 
                          coupons = c(0.05, 0.09, 0.04, 0.12), 
                          periodicities = c(2, 2, 2, 2), 
                          face_values = c(100000, 200000, 300000, 400000), 
                          quantities = c(1, 2, 3, 4))
)
# Function for calculating TTM (moved to its own so we can use either dates or ttm input)
get_bond_ttm <- function(settlement_date, maturity_date) {
  # settlement_date: date of bond ownership transfer
  # maturity_date: maturity date of bond
  ttm <- as.numeric(difftime(maturity_date, settlement_date, units = "days")) / 365
  
  return(ttm)
}


# Testing functions
test_bond_ttm <- get_bond_ttm(settlement_date = "2016-01-01", maturity_date = "2026-01-01")
test_bond_metrics <- cpp_get_bond_metrics(ttm = 10, yield = 0.05, c = 0.05)
test_bond_cf <- bond_cf(start_date = "2020-01-01", end_date = "2026-01-01", ytm = 0.04, c = 0.05, FV = 100)

treasury_yields_metrics <- get_yield_metrics(treasury_yields)   # <----  THIS TAKES FOREVER, until we implement it as a .cpp
                                                                # function I would only run it if you have to


#Portfolio Builder Functions


add_row <- function(p_price, start, end, coupon, periodocity, FV, quantity){
  
  T2M <- round(as.numeric(as_date(end)) - as.numeric(as_date(Sys.Date())),4)
  
  years <- round((as.numeric(as_date(end)) - as.numeric(as_date(start)))/365,4)
  
  
  
  temp_build <- tibble::tibble(purchase_price = p_price,
                               length = years,
                               coupon_rate = coupon/100,
                               TTM = T2M,
                               N = periodocity,
                               Face_Value = FV,
                               Quantity = quantity,
                               Nominal_Value = quantity*FV)
  
  temp_build
  
}


#Portfolio Builder list of portfolios in "Portfolios" Folder

update_portfolios <- function(){

port_path <- paste(dirname(getActiveDocumentContext()$path), "/Portfolios", sep = "") 
#This means the 'Portfolios' folder must live BESIDE the global file


portfolio_list <- list.files(path = port_path, pattern = "\\.csv$")

portfolio_paths <- lapply(portfolio_list, function(x) paste0(port_path,"/", x, sep = ""))

portfolio_files <- lapply(portfolio_paths, read.csv, check.names = FALSE)
#So from this I learnt that the csv file needs to have something in it for this entire function to work
#Two solutions - require at least one row of info or reduce fragility in the case of one or two NAs

names(portfolio_files) <- portfolio_list

portfolio_files

}




portfolio_files <- update_portfolios()

names(portfolio_files) <- names(portfolio_files) %>% 
  lapply(., function(x){gsub(".csv", "", x)})

#Small function (barely helpful) to save portfolio into csv

port_path <- paste(dirname(getActiveDocumentContext()$path), "/Portfolios", sep = "") 

save_portfolio <- function(name, table){
  
  connection <- paste(port_path,"/", name, ".csv", sep = "")
  
  readr::write_csv(table, connection, append = FALSE)
  
}

#--- Cubic Spline interpolation and discount factor calcs below ---#
#need to know the most recent quotes, since our function can always build the forward curve given the most recent fed quotes
recentday <- max(treasury_yields$date)

quotes <- treasury_yields %>% dplyr::filter(date == recentday) %>%
  dplyr::select(maturity, rate)

#here is the model, inspired from fintechII notes
spline <- stats::lm(
  rate ~ splines::bs(maturity, knots = c(2,5,10), degree = 3),
  quotes
)
#tested with fake data frame
fakedf <- data.frame(date = as.Date(c("2026-06-01", "2028-01-01", "2032-01-01", "2040-08-01")), cf = c(1200, 100, 10000, 10000))

discount_factor <- function(data) {
  #must input the bond cash flows in a data frame containing dates and cash flows ie) data
  df <- data %>% 
    dplyr::mutate(maturity = as.numeric(date - recentday) / 365.25)
  
  new_maturities <- df %>% dplyr::select(maturity)
  
  predictions <- spline %>% 
    stats::predict(
      newdata = new_maturities,
      interval = "prediction",
      level = 0.95
    )
  
  pred <- cbind(new_maturities, predictions) %>% 
    dplyr::transmute(maturity, rate = fit / 100) 
  
  df <- df %>% dplyr::left_join(pred) %>% 
    dplyr::mutate(df = (1 / (1 + rate)^maturity),
                  pv = cf * df) 
  
  df
}
#here is the output from the test
discount_factor(fakedf)

#--- end of rate interpolation via cubic spline and discount factor formula/calculator function work ---#