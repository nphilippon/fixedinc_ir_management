

dashboardPage(
  dashboardHeader(title = "IR App"),
  # Icon Options Here: https://fontawesome.com/  
  dashboardSidebar(
    sidebarMenu(
      menuItem("US Treasury Yield Data", tabName = "data", icon = icon("table")),
      menuItem("Portfolio Builder", tabName = "builder", icon = icon("wrench")),
      menuItem("Bond Metrics Calculator", tabName = "metrics", icon = icon("circle-info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, title = "Historical Treasury Yields", status = "primary",
                    plotlyOutput("hist_yield_chart", height = "400px"))
              ),
              fluidRow(
                box(width = 12, title = "Daily Treasury Yield Data", status = "info",
                    DTOutput("yieldTable"))
              )
      ),
      tabItem(tabName = "builder",
              sidebarLayout(
                sidebarPanel(
      
                    wellPanel(
                      id = "start_well",
                      
                      useShinyjs(),
                        
                      
                      selectInput("new_old",
                                  " ",
                                  choices = c("No Selection", "New Portfolio", "Existing Portfolio"),
                                  selected = "No Selection"),
                      
                      conditionalPanel(
                        
                        condition = "input.new_old == 'Existing Portfolio'",
                        uiOutput("portfolio_list"), #replace with list of portfolios once its developed
                        actionButton("existing_port_pull",
                                     "Pull Portfolio")
                      ),
                      conditionalPanel(
                        condition = "input.new_old == 'New Portfolio'",
                        textInput("new_port",
                                  "Name Your Portfolio",
                                  value = ""),
                        actionButton("initialize_new",
                                     "Initialize Portfolio")
                      )
                    ),
                    shinyjs::hidden(wellPanel(
                      id =  "input_well",
                      
                      dateInput(inputId = "build_start",
                                label = "Date of First Payment",
                                value = NULL,
                                min = "1624-12-10", #date of the oldest active bond, issued by Hoogheemraadschap Lekdijk Bovendams
                                max = NULL,
                                startview = "month"
                                ),
                      dateInput(inputId = "build_end",
                                label = "Date of Last Payment",
                                value = NULL,
                                startview = "month"
                                ),
                      numericInput("build_coupon",
                                   "Coupon Rate (%)",
                                   value = 0,
                                   min = 0,
                                   max = 100,
                                   step = 0.01),
                      numericInput("build_periodicity",
                                   "Coupons Per Year",
                                   value = 1,
                                   min = 1,
                                   step = 1),
                      numericInput(inputId = "build_FV",
                                   label = "Face Value of 1 Bond",
                                   value = 1000),
                      numericInput("build_quantity",
                                   "Quantity of Bonds",
                                   value = 0,
                                   min = 0),
                      actionButton("add_port_row",
                                   "Add Position"),
                      actionButton("build_exit",
                                   "Back")
                      
                    
                    )),
                    
                    shinyjs::hidden(actionButton("save_portfolio",
                                 "Save Portfolio"))
                  
                ),
                mainPanel(
                  
                  DTOutput("temp_table"),
                  shinyjs::hidden(textOutput("portfolio_name"))
                )
                
                
                
                
              )
              
      ),
      tabItem(tabName = "metrics",
              fluidRow(
                box(width = 3, title = "Input Bond Details", status = "warning",
                    numericInput("calc_fv", "Face/Par Value:", value = 100, min = 0),
                    numericInput("calc_c", "Annual Coupon Rate (%):", value = 5.0, step = 0.1),
                    selectInput("calc_periodicity", "Coupon Payment Frequency:",
                                choices = c("Annual" = 1, "Semi-Annual" = 2, "Quarterly" = 4, "Monthly" = 12),
                                selected = 2),
                    numericInput("calc_yield", "Current Yield (%):", value = 6.0, step = 0.1),
                    numericInput("calc_ttm", "Time to Maturity (Yrs):", value = 5.0),
                    hr(),
                    actionButton("run_metrics_calc", "Calculate Metrics")
                ),
                box(width = 9, title = "Bond Metrics", status = "primary",
                    fluidRow(
                      valueBoxOutput("bond_price_box", width = 3),
                      valueBoxOutput("modified_duration_box", width = 3),
                      valueBoxOutput("convexity_box", width = 3)
                    ),
                    fluidRow(
                      valueBoxOutput("central_delta_approx_box", width = 4),
                      valueBoxOutput("gamma_approx_box", width = 4)
                    )
                )
              )
        )
    )
  )
)
      
  

