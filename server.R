


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
    
    # Custom Colour Palette
    plot_colors <- c(
      "#d45087", "#f95d6a", "#ff7c43", "#ffa600",
      "#488f31", "#83af70", "#bad0af", "#003f5c",
      "#2f4b7c", "#665191", "#a05195")
    
    # Makes plotly line chart
    plot_ly(treasury_yields_named, x = ~date, y = ~rate, color = ~tenor,
            type = 'scatter', mode = 'lines', colors = plot_colors) %>%
      layout(
        # Adds hover info for all tenors on that date
        hovermode = "x unified",
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
              list(label = "All", step = "all"))),
          title = "Date",
          # Removes gridlines
          showgrid = FALSE,
          # Adds tick marks
          ticks = "outside",
          # Makes hover title show full date
          hoverformat = "%b %d, %Y"),
        yaxis = list(
          # Format as %X.XX
          tickformat = ".2%",
          # Removes gridlines
          showgrid = FALSE,
          # Adds axis line with tick marks every 1%
          showline = TRUE, ticks = "outside", dtick = 0.01,
          # Sets min y-axis to 0 and adds line
          rangemode = "tozero", zeroline = TRUE)
      )
  })
  
  # Historical Duration Chart
  
  output$hist_duration_chart <- renderPlotly({
    req(treasury_yields_metrics)
    
    # Custom Colour Palette
    plot_colors <- c(
      "#ffa600","#488f31", "#83af70", "#bad0af", 
      "#003f5c","#2f4b7c", "#665191", "#a05195")
    
    duration_plot_data <- treasury_yields_metrics %>% 
      dplyr::mutate(
        tenor = treasury_names[match(maturity, treasury_maturities)],
        tenor = factor(tenor, levels = treasury_names),
        duration = round(duration, digits = 4)
      ) %>% 
      filter(maturity > 0.5) %>% 
      select(date, duration, tenor)
    
    plot_ly(duration_plot_data, x = ~date, y = ~duration, color = ~tenor,
            type = 'scatter', mode = 'lines', colors = plot_colors) %>%
      layout(
        # Adds hover info for all tenors on that date
        hovermode = "x unified",
        xaxis = list(
          title = "Date",
          # Removes gridlines
          showgrid = FALSE,
          # Adds tick marks
          ticks = "outside",
          # Makes hover title show full date
          hoverformat = "%b %d, %Y"),
        yaxis = list(
          title = "Macaulay Duration",
          # Format as X.XX
          tickformat = "X.XX",
          # Removes gridlines
          showgrid = FALSE,
          # Adds axis line with tick marks every 2
          showline = TRUE, ticks = "outside", dtick = 2,
          # Sets min y-axis to 0 and adds line
          rangemode = "tozero", zeroline = TRUE)
      )
  }) 

  
  # Historical Delta Chart
  output$hist_delta_chart <- renderPlotly({
    req(treasury_yields_metrics)
    
    # Custom Colour Palette
    plot_colors <- c(
      "#ffa600","#488f31", "#83af70", "#bad0af", 
      "#003f5c","#2f4b7c", "#665191", "#a05195")
    
    delta_plot_data <- treasury_yields_metrics %>% 
      dplyr::mutate(
        tenor = treasury_names[match(maturity, treasury_maturities)],
        tenor = factor(tenor, levels = treasury_names),
        delta = round(delta, digits = 4)
      ) %>% 
      filter(maturity > 0.5) %>% 
      select(date, delta, tenor)
    
    plot_ly(delta_plot_data, x = ~date, y = ~delta, color = ~tenor,
            type = 'scatter', mode = 'lines', colors = plot_colors) %>%
      layout(
        # Adds hover info for all tenors on that date
        hovermode = "x unified",
        xaxis = list(
          title = "Date",
          # Removes gridlines
          showgrid = FALSE,
          # Adds tick marks
          ticks = "outside",
          # Makes hover title show full date
          hoverformat = "%b %d, %Y",
          # Adds like to bottom
          showline = TRUE),
        yaxis = list(
          title = "Delta (Central Approximation)",
          # Format as X.XXXX
          tickformat = ".4f",
          # Removes gridlines
          showgrid = FALSE,
          # Adds axis line with tick marks every 0.05
          showline = TRUE, ticks = "outside", dtick = 0.05,
          # Sets min y-axis to 0 and adds line
          zeroline = FALSE)
      )
  }) 
  
  #Portfolio Builder Section Below
  
  #Initializing temp tables
  
  temp_table <- reactiveValues(data = NULL) #For the portfolio builder
  
  temp_table_rm <- reactiveValues(data = NULL) #For the risk manager
  
 
  
  
  
  observeEvent(input$initialize_new, {
    
    shinyjs::showElement(id = "input_well")
    shinyjs::showElement(id = "save_portfolio")
    shinyjs::hideElement(id = "start_well")
    
    
  })
  
  observeEvent(input$existing_port_pull, {
    
    shinyjs::showElement(id = "input_well")
    shinyjs::showElement(id = "save_portfolio")
    shinyjs::hideElement(id = "start_well")
    
    
  })
  
#Multiple things occur when user presses "back"
  
  observeEvent(input$build_exit, {
    
    showModal(
      modalDialog(
        
        paste("All progress will be lost. To prevent this, save portfolio first"),
        easyClose = TRUE,
        footer = tagList(
          actionButton("confirm_back",
                               "Continue",
                               style = "float:left"),
          modalButton("Stay")
        )
      )
    )
  })
  
  observeEvent(input$confirm_back, {
    
    shinyjs::showElement(id = "start_well")
    shinyjs::hideElement(id = "save_portfolio")
    shinyjs::hideElement(id = "input_well")
    
    temp_table$data <- NULL
    removeModal()
    
  })
  
  
  # Updating table
  observeEvent(input$add_port_row, {
    
    start <- isolate(input$build_start)
    end <- isolate(input$build_end)
    coupon <- isolate(input$build_coupon)
    periods <- isolate(input$build_periodicity)
    face_val <- isolate(input$build_FV)
    quantity <- isolate(input$build_quantity)
    p_price <- isolate(input$build_p_price)
    
    new_row <- add_row(p_price,
                       start,
                       end,
                       coupon,
                       periods,
                       face_val,
                       quantity)
    
      
    if(is.null(temp_table$data)){
        
        temp_table$data <- new_row
      
    }else{
      
        temp_table$data <- rbind(temp_table$data, new_row)
      
    }
  
  })
  
  
  #Initializing portfolio_files as a server variable
  
  updating_list <- reactiveValues(data = portfolio_files)
  
  
  #Showing Existing Portfolio Immediately with following:

  
  output$portfolio_list <- renderUI({ #For Portfolio Builder
      selectInput("portfolio_list",
                  "Choose Portfolio",
                  choices = names(updating_list$data))
  
  })
  

  output$portfolio_list_rm <- renderUI({ #For Risk Manager
    selectInput("portfolio_list",
                "Choose Portfolio",
                choices = names(updating_list$data),
                selected = names(updating_list$data)[1])
    
  })
  
  
  
  
  
  observeEvent(input$existing_port_pull, {
    
    exist_port <- isolate(input$portfolio_list)
    
    temp_table$data <- as.data.frame(updating_list$data[[exist_port]])
    
  })
  
  
  output$temp_table <- renderDT({
    temp_table$data
  })
  

  #Saving Portfolio
  
  port_name <- reactiveValues(data = NULL)

  
  output$portfolio_name <- renderText({
    port_name$data
  })
  
  
  observeEvent(input$save_portfolio, {

    showModal(modalDialog(
      
      paste("Saving a portfolio with the same name as an existing portfolio will overwrite the existing portfolio."),
      footer = tagList(actionButton("save_continue",
                            "Continue",
                            style = "float:left"),
                       modalButton("Back")
               ),
      easyClose = TRUE

    ))
    
    
  })
  
  
  observeEvent(input$save_continue,{
    
    new_port <- isolate(input$new_port)
    exist_port <- isolate(input$portfolio_list)
    
    
    if(input$new_old == "New Portfolio"){
      
      port_name$data <- new_port
      
      updating_list$data <- append(updating_list$data, setNames(list(temp_table$data), as.character(new_port)))
      
    }else if(input$new_old == "Existing Portfolio"){
      
      port_name$data <- exist_port
      
      updating_list$data[[as.character(exist_port)]] <- list(temp_table$data)
      
    }
    
    save_portfolio(port_name$data, temp_table$data)
    
    removeModal()
    
    
  })
  
  
  
  
  
  
  
    
  #Deleting Rows
  
  selected_rows <- reactive({
    input$temp_table_rows_selected
    })
  
  
  output$row_id <- renderPrint(as.vector(selected_rows()))
  
  observeEvent(input$delete_button, {
    
    row_id <- as.vector(selected_rows())
    
    if(is.null(row_id) == TRUE){
      showModal(
        modalDialog("No Rows Selected",
                    easyClose = TRUE,
                    footer = modalButton("OK"))
      )
      
    }else{
      
      showModal(
        modalDialog(paste("Please confirm: Rows ", 
                          paste(row_id, collapse = ", "), 
                          " will be deleted."),
                    easyClose = TRUE,
                    footer = tagList(
                      actionButton("delete_modal",
                                   "Delete Selected Rows",
                                   style = "color: #fff; background-color: #FF0000; border-color: #000000; float:left"),
                      
                      modalButton("Go Back"))
                    )
      )
    }
  })
  
  
  observeEvent(input$delete_modal, {
    
    row_id <- as.vector(selected_rows())
    
    temp_table$data <- temp_table$data %>% 
      slice(-row_id)
    
    removeModal()
    
  })  
  
  
  
  #Risk Manager Output
  
  
  #BOXES AT THE TOP
  
  output$pv_rm <- renderValueBox({
    valueBox(
      value = 120, #switch with reactive portfolio value here
      subtitle = "Portfolio Value",
      icon = icon("chart-line"),
      color = "navy"
    )
    
  })
  
  
  output$cashflow_rm <- renderValueBox({
    valueBox(
      value = 200, #switch with reactive portfolio value here
      subtitle = "Next Cashflow",
      icon = icon("dollar-sign"),
      color = "maroon"
    )
    
  })
  
  
  output$dailych_rm <- renderValueBox({
    valueBox(
      value = 3, #switch with reactive portfolio value here
      subtitle = "Daily Change ($)",
      icon = icon("plus-minus"),
      color = "olive"
    )
    
  })
  
  
  #Risk manager inputs well panels:
  
  shinyjs::showElement(id = "rm_text_panel")
  
  
  observeEvent(input$VaR_button, {
    
    shinyjs::hideElement(id = "rm_text_panel")
    shinyjs::hideElement(id = "stress_inputs")
    shinyjs::hideElement(id = "other_panel")
    shinyjs::showElement(id = "var_inputs")
    
  })
  
  
  observeEvent(input$stress_rm_button, {
    
    shinyjs::hideElement(id = "rm_text_panel")
    shinyjs::showElement(id = "stress_inputs")
    shinyjs::hideElement(id = "other_panel")
    shinyjs::hideElement(id = "var_inputs")
    
  })
  
  observeEvent(input$button_3, {
    
    shinyjs::hideElement(id = "rm_text_panel")
    shinyjs::hideElement(id = "stress_inputs")
    shinyjs::showElement(id = "other_panel")
    shinyjs::hideElement(id = "var_inputs")
    
    
  })
  
  
  observeEvent(input$set_rm_port, {
    
    rm_port <- isolate(input$set_rm_port)
    
    temp_table_rm$data <- as.data.frame(updating_list$data[[rm_port]])
    
    
  })
  
 
  output$rm_temp_table <- renderDataTable(
    
    temp_table_rm$data,
    options = list(scrollX = TRUE)
    
  )
  
  
  output$portfolio_cashflows <- renderDataTable({
    
    portfolio_table <- temp_table_rm$data
    
    cpp_get_portfolio_cfs(start_dates = as.Date(portfolio_table$start_date),
                         end_dates = as.Date(portfolio_table$end_date),
                         coupons = as.numeric(portfolio_table$coupon_rate),
                         periodicities = as.integer(portfolio_table$N),
                         face_values = as.numeric(portfolio_table$Face_Value),
                         quantities = as.numeric(portfolio_table$Quantity))

  })

  
  
  
  shinyjs::hide(id = "rm_cashflow_panel")
  
  observeEvent(input$set_rm_port, {
    

      
      shinyjs::showElement(id = "rm_cashflow_panel")
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    

  
  # Bond Metrics tab reactive calculations (waits for user to run)
  bond_metrics_results <- eventReactive(input$run_metrics_calc, {
    get_bond_metrics(
      FV = input$calc_fv,
      c = input$calc_c / 100,
      yield = input$calc_yield / 100,
      ttm = input$calc_ttm,
      periodicity = as.numeric(input$calc_periodicity)
    )
  })
  
  # Bond Metrics Output boxes
  # Bond Price Output
  output$bond_price_box <- renderValueBox({
    valueBox(formatC(bond_metrics_results()$bond_price, format = "f", digits = 2), "Bond Price")
  })
  # Modified Duration Output
  output$modified_duration_box <- renderValueBox({
    valueBox(formatC(bond_metrics_results()$modified_duration, format = "f", digits = 2), "Modified Duration")
  })
  # Convexity Output
  output$convexity_box <- renderValueBox({
    valueBox(formatC(bond_metrics_results()$convexity, format = "f", digits = 2), "Convexity")
  })
  # Central Delta Approximation Output
  output$central_delta_approx_box <- renderValueBox({
    valueBox(formatC(bond_metrics_results()$delta_central_approx, format = "f", digits = 8), "Delta Central Approximation")
  })
  # Gamma Approximation Output
  output$gamma_approx_box <- renderValueBox({
    valueBox(formatC(bond_metrics_results()$gamma_approx, format = "f", digits = 8), "Gamma Approximation")
  })
  
  
  #output$build_bond_price <- renderText({
    
    #building components of RTL bond function
  #  ytm_bond <- tidyquant::tq_get(input$build_note,
  #                               get = "economic.data",
  #                               from = Sys.Date() - 30)
  #  
  #  ytm <- ytm_bond$price %>% tail(1)
  #  
  #  coupon <- (input$build_coupon/100)
  #  
  #
  #  RTL::bond(ytm = ytm/100, C = coupon, T2M = input$build_ttm, m = input$build_n, output = "price")
    
  #})
  
  output$forward_curve <- renderPlot({
    
    quotes %>% ggplot2::ggplot(aes(x = maturity, y = rate)) +
    ggplot2::geom_point(size = 3, color = "limegreen") +
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ splines::bs(x, knots = c(2, 5, 10), degree = 3),
      se = FALSE,
      color = "darkred",
      linewidth = 1
    ) + 
      ggplot2::scale_x_continuous(breaks = c(round(quotes$maturity),0)) + 
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.2)) +
      ggplot2::labs(y = "interest rate",
                    x = "maturity (years from most recent Fed quotes)")
      
  })
    
  
  
}

