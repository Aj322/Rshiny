library(shiny)
library(leaflet)
library(DT)

function(input, output) {
  
  ops_filters <- reactive({
    
    if(is.null(input$term)) {
      
      out <- dat
      
    } else {
      
      out <- dat %>% 
        filter(`Ops Div` %in% input$term)
      
    }
    
    req( nrow(out) > 0 )
    
    out
    
  })
  
  
  source("server/01-maps-srv.R", local = TRUE)
  
  
  #Calculate the driver count for value box
  output$drivercount <- renderValueBox({
    valueBox(
      value = length(ops_filters()$`Driver ID`), subtitle = "Drivers",
      icon = icon("truck"), color = "orange"
    )
  })
  
  
  output$table <- DT::renderDataTable({
    out <- ops_filters()
    
    DT::datatable(out, 
                  rownames = FALSE, 
                  extensions = "Buttons", 
                  filter = 'top',
                  options = list(
                    dom = 'Bfrtip',            ##table control elements appear on the page in specific order
                    scrollX = TRUE,
                    buttons = list(
                      'colvis', 
                      list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'copy'),
                        text = 'Download', 
                        filename = 'Data export'
                      )
                    )
                  )
    )
  }, server = FALSE)
  
  
}