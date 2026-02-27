

dashboardPage(
  dashboardHeader(title = "IR App"),
  # Icon Options Here: https://fontawesome.com/  
  dashboardSidebar(
    sidebarMenu(
      menuItem("US Treasury Yield Data", tabName = "data", icon = icon("table")),
      menuItem("Portfolio Builder", tabName = "builder", icon = icon("wrench"))
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
                        selectInput("portfolio_list",
                                    "Choose Portfolio",
                                    choices = c("hello","bye")), #replace with list of portfolios once its developed
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
                      
                    
                    ))
                  
                ),
                mainPanel(
                  
                  DTOutput("temp_table")
                  
                )
                
                
                
                
              )
              
            )
          )
        )
  )
      
  

