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
      pivot_longer(cols = -date, names_to = "Tenor", values_to = "Yield") %>%
      mutate(
        # Converts Tenor to a factor in order of maturity (for legend ordering)
        Tenor = factor(Tenor, levels = names(treasury_symbols)),
        # Converts Yield to actual % (for proper axis)
        Yield = Yield/100) 
    
    # Makes plotly line chart (Will format later)
    plot_ly(plot_data, x = ~date, y = ~Yield, color = ~Tenor,
            type = 'scatter', mode = 'lines') %>%
      layout(
        xaxis = list(
          # Adds date range selector buttons
          rangeselector = list(
            buttons = list(
              list(count = 3, label = "3Mo", step = "month", stepmode = "backward"),
              list(count = 6, label = "6Mo", step = "month", stepmode = "backward"),
              list(count = 1, label = "1Yr", step = "year", stepmode = "backward"),
              list(count = 2, label = "2Yr", step = "year", stepmode = "backward"),
              list(count = 5, label = "5Yr", step = "year", stepmode = "backward"),
              list(count = 10, label = "10Yr", step = "year", stepmode = "backward"),
              list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
              list(step = "all")))),
        yaxis = list(
          # Format as %X.XX
          tickformat = ".2%")
      )
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

