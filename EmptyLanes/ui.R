library(shiny)
library(DT)
library(plotly)

header <- dashboardHeader(
  title = "Empty Lanes"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Maps", tabName = "maps", icon = icon("map")),
    menuItem("Plot", tabName = "plot", icon = icon("area-chart")),
    menuItem("Data", tabName = "data", icon = icon("table"))
  )
)

body <- dashboardBody(
  tabItems(
    
    source("ui/01-maps-ui.R", local = TRUE)$value, 
    
    source("ui/02-plot-ui.R", local = TRUE)$value,
    
    tabItem(tabName = "data", 
            fluidRow(
              box(width = 12,
                  DT::dataTableOutput("table")
              )
            )
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)


