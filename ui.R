library(shiny)
library(bslib)


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
              fluidRow(
                box(width = 4, title = "Input Bond to Portfolio", status = "info",
                  textInput(inputId = "bond_name",
                            label = "Bond Name",
                            value = "1-Year Zero Coupon Bond"), 
                  dateInput(inputId = "start_date",
                            label = "Date of First Payment",
                            value = NULL,
                            min = "1624-12-10", #date of the oldest active bond, issued by Hoogheemraadschap Lekdijk Bovendams
                            max = NULL,
                            startview = "month"
                            ),
                  dateInput(inputId = "end_date",
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
                  numericInput("T2M",
                               "Time to Maturity (Years)",
                               value = 0,
                               min = 0), #may be redundant
                  numericInput("periodicity",
                               "Coupons Per Year",
                               value = 1,
                               min = 1,
                               step = 1),
                  numericInput(inputId = "FV",
                               label = "Face Value of 1 Bond",
                               value = 100000),
                  numericInput("quantity",
                               "Quantity of Bonds",
                               value = 0,
                               min = 0),
                  actionButton("enter_bond", "Add Bond to Portfolio", class = "btn-block btn-success")
                    ),
                box(width = 8, title = "put charts here", status = "info")
              )

                 

              
            )
          )
        )
  )
      
  

