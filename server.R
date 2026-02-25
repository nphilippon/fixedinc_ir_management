library(shiny)
library(RTL)
library(tidyquant)
library(tidyverse)
library(plotly)

function(input, output, session) {

  # Yield Data Table
  output$yieldTable <- renderDT({
    datatable(treasury_yields, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = names(treasury_symbols), digits = 2)
  })
  
  # Historical Yield Chart
  output$hist_yield_chart <- renderPlotly({
    req(treasury_yields)
    
    # Convert into long df with cols date, tenor, yield for chart
    plot_data <- treasury_yields %>% 
      pivot_longer(cols = -date, names_to = "Tenor", values_to = "Yield")
    
    # Makes plotly line chart (Will format later)
    plot_ly(plot_data, x = ~date, y = ~Yield, color = ~Tenor,
            type = 'scatter', mode = 'lines')
    
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

