library(shiny)

dashboardPage(
  dashboardHeader(title = "IR App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, title = "Treasury Yield Data", status = "info",
                    DTOutput("yieldTable"))
              )
      ))
  )
