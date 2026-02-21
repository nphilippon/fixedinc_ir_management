library(shiny)
library(bslib)



dashboardPage(
  dashboardHeader(title = "IR App"),
  # Icon Options Here: https://fontawesome.com/  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raw Data", tabName = "data", icon = icon("table")),
      menuItem("Portfolio Builder", tabName = "builder", icon = icon("wrench"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, title = "Treasury Yield Data", status = "info",
                    DTOutput("yieldTable"))
              )
      ),
      tabItem(tabName = "builder",
              sidebarLayout(
                sidebarPanel(
                  numericInput("build_quantity",
                               "Quantity",
                               value = 0,
                               min = 0),
                  selectInput("build_note",
                              "Treasury Note",
                              choices = treasury_symbols),
                  numericInput("build_coupon",
                               "Coupon %",
                               value = 0,
                               min = 0,
                               max = 100,
                               step = 0.5),
                  numericInput("build_ttm",
                               "Bond TTM (in years",
                               value = 0,
                               min = 0), #may be redundant
                  numericInput("build_n",
                               "Coupon Periods Yearly",
                               value = 1,
                               min = 1,
                               step = 1),
                  textOutput("build_bond_price")
                  
                ),
                mainPanel(
                  
                  "Hello World"
                  
                )
                
                
                
                
              )
              
            )
          )
        )
  )
      
  

