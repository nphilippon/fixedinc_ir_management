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