#UI - Maps tab

tabItem(
  "maps",
  fluidRow(
    column(
      width = 9,
      fluidRow(
        valueBoxOutput("drivercount"),
        valueBoxOutput("acceptrate"),
        valueBoxOutput("bhrate")
      ),
      fluidRow(
        box(
          width = 12,  
          shinyWidgets::radioGroupButtons(inputId = "iconSelect", 
                                             label = "Show Truck Icons", 
                                             choices = c("On", "Off"), 
                                             selected = "On", 
                                             justified = TRUE,
                                             status = "primary", 
                                             checkIcon = list(
                                               yes = icon("ok", lib = "glyphicon"), 
                                               no = icon("remove", lib = "glyphicon")
                                             )
          ), 
          leafletOutput("map", height = "700px") %>% 
            withSpinner(color="#0dc5c1")
          )
      )
    ), 
    box(width = 3, 
          title = "Trip Type", 
          status = "primary", 
          checkboxGroupInput(inputId = "call", 
                              label =  NULL, 
                              choices = c("Called-in", "Not Called-in"), 
                              selected = c("Called-in", "Not Called-in"), 
                              inline = TRUE),
        #  br(), 
          checkboxGroupInput(inputId = "type", 
                              label = NULL, 
                              choices = c("Sent Home", "Backhaul"), 
                              selected = c("Sent Home", "Backhaul"), 
                              inline = TRUE)
    ),
    column(
      width = 3, 
         box(
           #width = NULL,
           title = "Ops Divison", solidHeader = TRUE, 
           status = "warning",
           #div(style = 'overflow-y: scroll'), 
           tags$style(HTML("#term {height:650px; overflow-y:scroll}")), 
           checkboxGroupInput("term", "", choices = sort(unique(data$Home)) )
         ), 
         box(
           #width = NULL,
           title = "Sent To Terminal", solidHeader = TRUE, 
           status = "warning", 
           div(style = 'overflow-y: scroll'), 
           tags$style(HTML("#stt {height:650px; overflow-y:scroll}")), 
           checkboxGroupInput("stt", "", choices = sort(unique(data$STT_Review)) )
      )
    )
  )
)