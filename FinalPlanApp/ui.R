library(shiny)
library(DT)
library(plotly)

header <- dashboardHeader(
  title = "Driver Plan Dashboard"
)

sidebar <- dashboardSidebar(
  sidebarMenu(id = "menu", 
    menuItem("Maps", tabName = "maps", icon = icon("map")),
    menuItem("Plot", tabName = "plot", icon = icon("area-chart")),
    menuItem("Data", tabName = "data", icon = icon("table")),
    br(),
    br(),
    br(), 
    gradientBox(title = "NP Final Plan", 
                icon = "fa fa-user", 
                gradientColor = "teal", 
                boxToolSize = "sm", 
                width = 12, height = "100", 
                footer = optsummary$UserName,
                paste("Published -", optsummary$SubmissionDT)
                )
    # textInput(inputId = "plantime", 
    #           label = "NP Plan Publish DT", 
    #           value = optsummary$SubmissionDT)
    # widgetUserBox(title = optsummary$UserName, subtitle = NULL, 
    #               type = NULL, 
    #               src = ifelse(optsummary$User == 'ggoll', 
    #                            "www/img/ggoll.png", NULL), 
    #               color = "aqua-active",
    #               closable = TRUE, 
    #               footer = paste0("Published at ", optsummary$SubmissionDT))
  )
)

body <- dashboardBody(
  tags$head(
    tags$script(src = "custom.js")
  ),
  tabItems(
    
    source("ui/01-maps-ui.R", local = TRUE)$value, 
    # tabItem(tabName = "maps", 
    #         fluidRow(
    #           column(width = 12, 
    #                  leafletOutput("map", height = "500px"))
    #         )
    #),
    
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

          