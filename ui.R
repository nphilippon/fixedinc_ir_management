

dashboardPage(
  dashboardHeader(title = "IR App"),
  # Icon Options Here: https://fontawesome.com/  
  dashboardSidebar(
    sidebarMenu(
      menuItem("US Treasury Yield Data", tabName = "data", icon = icon("table")),
      menuItem("Portfolio Builder", tabName = "builder", icon = icon("wrench")),
      menuItem("Bond Metrics Calculator", tabName = "metrics", icon = icon("circle-info")),
      menuItem("Risk Manager", tabName = "risk_manager", icon = icon("dollar-sign"))
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
                box(width = 12, title = "Forward Curve - as of most recent Fed quotes", status = "info",
                    plotOutput("forward_curve"))
              ),
              fluidRow(
                box(width = 12, title = "Duration", status = "info",
                    plotlyOutput("hist_duration_chart", height = "300px"))
              ),
              fluidRow(
                box(width = 12, title = "Delta", status = "info",
                    plotlyOutput("hist_delta_chart", height = "300px"))
              )
      ),
      tabItem(tabName = "builder",
              fluidRow(
                column(3,
      
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
                      numericInput("build_p_price",
                                   "Purchase Price",
                                   value = 100,
                                   min = 0,
                                   step = 5),
                      numericInput("build_coupon",
                                   "Coupon Rate (%)",
                                   value = 0,
                                   min = 0,
                                   max = 100,
                                   step = 0.5),
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
                                   "Back"),
                      actionButton("delete_button", "Delete",
                                   style = "color: #fff; background-color: #FF0000; border-color: #000000")
                      
                    
                    )),
                    
                    shinyjs::hidden(actionButton("save_portfolio",
                                 "Save Portfolio"))
                  
                ),
                column(9,
                  box(
                   title = "Current Portfolio",
                   width = NULL,
                   DTOutput("temp_table", height = "900px"),
                   verbatimTextOutput('row_id')
                   
                  )   
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
      ),
      tabItem(tabName = "risk_manager",
              
              
              
              
              
              
              
                fluidRow(
                  column(
                    4,
                    box(
                      title = "Risk Manager",
                      status = "primary",
                      id = "top_box_rm",
                      collapsible = TRUE,
                      solidHeader = T,
                      width = 12,
                      uiOutput("portfolio_list_rm"), #will try to use same logic as port builder,
                      style = "background: rgb(224,224,224)"
                    )
                  ),
                  column(
                    8,
                    valueBoxOutput("pv_rm"),
                    valueBoxOutput("cashflow_rm"),
                    valueBoxOutput("dailych_rm")
                  )
                ),
                fluidRow(
                  column(
                    4,
                    fluidRow(
                      column(
                        6,
                        actionButton("VaR_button",
                                     "VaR",
                                     style = "color: #FFFFFF; background-color: #006633; border-color: #000000",
                                     width = "100%")
                      ),
                      column(
                        6,
                        actionButton("stress_rm__button",
                                     "Stress Testing",
                                     style = "color: #FFFFFF;background-color: #330066; border-color: #000000",
                                     width = "100%")
                      ),
                    ),
                    fluidRow(
                    #Replacing all these with well panels that will be shown with a click of each button above.
                       box(
                         title = "VaR",
                         status = "primary",
                         style = "background: rgb(224,224,224)",
                         solidHeader = T,
                         width = 12,
                         collapsible = TRUE,
                         numericInput("confidence_rm",
                                      "Confidence ($)",
                                      value = 5,
                                      min = 0,
                                      max = 100,
                                      step = 2.5)
                       ),
                       box(
                         title = "Stress Test",
                         status = "primary",
                         style = "background: rgb(224,224,224)",
                         solidHeader = T,
                         width = 12,
                         collapsible = TRUE,
                         numericInput("basis_ch_rm",
                                      "Basis Point Change",
                                      value = 0,
                                      step = 1)
                       )
                    )
                )
                  
                
                
                
                
              )
              
      
              
              
              
              
              
              
        )
    )
  )
)
      
  

