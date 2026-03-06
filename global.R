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

# Get treasury yield metrics for all dates and tenors
treasury_yields_metrics <- get_yield_metrics(treasury_yields)

# Function for getting bond cashflows with C++ function
get_bond_cfs <- function(start_dates, end_dates, coupons, periodicities, face_values, quantities) {
  # Start_dates: Vector, starting dates (first payments), in date format for all the bonds
  # End_dates: Vector, ending dates (final payments), in date format for all the bonds
  # Coupons: Vector, numerical coupon rates for the bonds (MUST BE SAME TIME FRAME COMPOUNDING)
  # Periodicities: Vector, numerical. Number of payments within a year.
  # Face_values: Vector, numerical. Face value of each individual bond. 
  # Quantities: Vector, numerical amount of each bond owned in the portfolio
  
  # Get long cashflows df from c++ function
  bond_cfs_long <- cpp_get_portfolio_cfs(as.Date(start_dates), as.Date(end_dates), 
                                         as.numeric(coupons), as.integer(periodicities),
                                         as.numeric(face_values), as.numeric(quantities))
  
  bond_cfs_long <- bond_cfs_long %>% 
    dplyr::mutate(
      date = as.Date(date) # Makes sure date is of proper type (although I think the C++ output is already)
    ) %>% 
    arrange(date)
  
  return(bond_cfs_long)
}



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
test_cpp_bond_cfs <- get_bond_cfs(start_dates = c("2026-01-01", "2025-06-19", "2024-12-25", "2023-04-22"), 
                                  end_dates = c("2032-12-31", "2028-02-14", "2029-12-24", "2031-01-01"), 
                                  coupons = c(0.05, 0.09, 0.04, 0.12), 
                                  periodicities = c(2, 2, 2, 2), 
                                  face_values = c(100000, 200000, 300000, 400000), 
                                  quantities = c(1, 2, 3, 4))


                                                                


#Portfolio Builder Functions


add_row <- function(p_price, start, end, coupon, periodocity, FV, quantity){
  
  T2M <- round(as.numeric(as_date(end)) - as.numeric(as_date(Sys.Date())),4)
  
  years <- round((as.numeric(as_date(end)) - as.numeric(as_date(start)))/365,4)
  
  
  
  temp_build <- tibble::tibble(start_date = start,
                               end_date = end,
                               purchase_price = p_price,
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

#port_path <- paste(dirname(getActiveDocumentContext()$path), "/Portfolios", sep = "") 
#This means the 'Portfolios' folder must live BESIDE the global file
port_path <- file.path(getwd(), "Portfolios")

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


port_path <- file.path(getwd(), "Portfolios")


save_portfolio <- function(name, table){
  
  connection <- paste(port_path,"/", name, ".csv", sep = "")
  
  readr::write_csv(table, connection, append = FALSE)
  
}


get_portfolio_metrics_table <- function(portfolio_df){
  
  df <- portfolio_df %>% 
    dplyr::mutate(bond_id = row_number())
  
  portfolio_df <- df %>% 
    dplyr::group_by(bond_id) %>% 
    dplyr::reframe(date = seq.Date(from = as.Date(start_dates), to = as.Date(end_dates), by = "day")) %>% 
    dplyr::left_join(df, by = join_by(bond_id)) %>% 
    dplyr::mutate(ttm = get_bond_ttm(date, as.Date(end_dates))) %>% 
    ungroup() %>% 
    select(bond_id, date, ttm, c = coupon_rate, FV = Face_Value, quantity = Quantity)
  
  portfolio_dates <- portfolio_df %>% 
    distinct(date)
  
  portfolio_discount_factors <- discount_factor(portfolio_dates) %>% 
    select(date, yield)
  
  portfolio_df <- left_join(portfolio_df, portfolio_discount_factors, by = "date")

  portfolio_metrics <- cpp_get_portfolio_metrics(ttm = portfolio_df$ttm,
                                                 yield = portfolio_df$yield,
                                                 c = portfolio_df$c,
                                                 FV = portfolio_df$FV)
  
  output_df <- portfolio_df %>% 
    bind_cols(portfolio_metrics)
  
  return(output_df)
}


















#--- Cubic Spline interpolation and discount factor calcs below ---#
#need to know the most recent quotes, since our function can always build the forward curve given the most recent fed quotes
treasury_yields <- treasury_yields %>% drop_na()

past_discount_factor <- function(data, target_date){
  
  target_date <- as.Date(target_date)
  
  treasury_yields <- treasury_yields %>% 
    dplyr::filter(date <= target_date)
  
  recentday <- max(treasury_yields$date)
  
  if(target_date <= recentday) {recentday = target_date}
  
  quotes <- treasury_yields %>% dplyr::filter(date == recentday) %>%
    dplyr::select(maturity, rate)
  
  #here is the model, inspired from fintechII notes
  spline <- stats::lm(
    rate ~ splines::bs(maturity, knots = c(2,5,10), degree = 3),
    quotes
  )
  
  int_discount_factor <- function(data = data) {
    #use cf = TRUE if your df has a cf column, otherwise you just need a date column
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
      dplyr::transmute(maturity, rate = fit) #/ 100) 
    
    #if(cf == FALSE) {
    
    #df <- df %>% dplyr::left_join(pred) %>% 
    #dplyr::mutate(df = (1 / (1 + rate)^maturity))
    
    #} else {
    
    df <- df %>% dplyr::left_join(pred) %>% 
      dplyr::mutate(df = (1 / (1 + rate)^maturity),
                    pv = cf * df)  
    #}
    
    return(df)
  }
  return(int_discount_factor(data))
}


marking_to_market <- function(target_date, portfolio_table){
  
  x <- cpp_get_portfolio_cfs(start_dates = as.Date(portfolio_table$start_date),
                             end_dates = as.Date(portfolio_table$end_date),
                             coupons = as.numeric(portfolio_table$coupon_rate),
                             periodicities = as.integer(portfolio_table$N),
                             face_values = as.numeric(portfolio_table$Face_Value),
                             quantities = as.numeric(portfolio_table$Quantity)) %>% 
    tidyr::pivot_wider(names_from = bond_id, values_from = cf) %>% 
    dplyr::mutate(cf = rowSums(across(-date), na.rm = TRUE)) %>% past_discount_factor(target_date = target_date) %>% 
    dplyr::arrange((date)) %>% 
    dplyr::mutate(
      dummy_future = case_when(target_date >= date ~ 0, .default = 1),
      dummy_past = (dummy_future - 1)^2,
      cash_received = dummy_past * cf,
      pv_noncash = dummy_future * pv,
    )
  
  mtm <- sum(x$cash_received) + sum(x$pv_noncash)
  
  return(mtm)
}

today <- Sys.Date()

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
  
  return(df)
}