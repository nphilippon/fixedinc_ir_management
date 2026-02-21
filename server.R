library(shiny)
library(RTL)
library(tidyquant)
library(tidyverse)

function(input, output, session) {

  # Render Raw Yield Data Table
  output$yieldTable <- renderDT({
    datatable(treasury_yields, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = names(treasury_symbols), digits = 2)
  })
  
  
  output$build_bond_price <- renderText({
    
    #building components of RTL bond function
    ytm_bond <- tidyquant::tq_get(input$build_note,
                                 get = "economic.data",
                                 from = Sys.Date() - 30)
    
    ytm <- ytm_bond$price %>% tail(1)
    
    coupon <- (input$build_coupon/100)
    
  
    RTL::bond(ytm = ytm/100, C = coupon, T2M = input$build_ttm, m = input$build_n, output = "price")
    
    
    
    
    
  })
  
  
  
  
  
}

