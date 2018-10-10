tabItem(
  "maps",
  fluidRow(
    box(
      width = 10, 
      leafletOutput("map", height = "850px") %>% 
        withSpinner(color="#0dc5c1"), 
      footer = paste0("This map includes all the empty lanes from ", format(min(dat$`Dispatch Date-Time`), "%b %d, %y"), 
                      " to ", format(max(dat$`Dispatch Date-Time`), "%b %d, %y"))
    ), 
    valueBoxOutput("drivercount", width = 2), 
    br(),
    box(width = 2, 
        title = "Ops Division", solidHeader = TRUE, 
        status = "primary", 
        tags$style(HTML("#term {height:650px; overflow-y:scroll}")), 
        checkboxGroupInput("term", "", choices = sort(unique(dat$`Ops Div`)) )
    )
  )
)