#Shiny server

library(shiny)
library(leaflet)
library(DT)

function(input, output, session) {
  
  
  ## 1. called filters
  
  called_filters <- reactive({
    
    if(is.null(input$call)) {
      
      out <- data
      
    } else {
      
      out <- data %>% 
        filter(Called %in% input$call)
      
    }
    
    req( nrow(out) > 0 )
    
    out
    
  })
  
  
  ##2. trip filters
  
  type_filters <- reactive({
    
    if(is.null(input$type)) {
      
      out <- called_filters()
      
    } else {
      
      out <- called_filters() %>% 
        filter(TripType %in% input$type)
      
    }
      
    req( nrow(out) > 0 )
    
    out
    
  })
  
  
  ##3. ops div filters
  
  ops_filters <- reactive({
    
    if(is.null(input$term)) {
      
      out <- type_filters()
      
    } else {
      
      out <- type_filters() %>% 
        filter(Home %in% input$term)
      
    }
    
    req( nrow(out) > 0 )
    
    out
    
  })
  
  
  
  ##4. stt div filters
  
  stt_filters <- reactive({
    
    if(is.null(input$stt)) {
      
      out <- ops_filters()
      
    } else {
      
      out <- ops_filters() %>% 
        filter(STT_Review %in% input$stt)
      
    }
    
    req( nrow(out) > 0 )
    
    out
    
  })
  
  
  ##5. final reactive dataset
  
  dat <- reactive({
    
    stt_filters()
    
  })
  
  
  source("server/01-maps-srv.R", local = TRUE)
  
  
  timer <- reactiveVal()
  active <- reactiveVal()
  
  observeEvent(input$menu, {
    
    if(input$menu == "plot") {
      active(TRUE)
    } else {
      active(FALSE)
    }
    
  })
  
  observe({
    
    req(input$menu)
      invalidateLater(5000, session)
      isolate({
        if(active()) {
          timer(paste(round(as.numeric(OptRun$BHPercent), digits = 2), " %"))
          active(FALSE)
        } else {
          timer(OptRun$BHDrivers)
          active(TRUE)
        }
      })  
    
  })
  
  
  
  #Calculate the driver count for value box
  output$drivercount <- renderValueBox({
    valueBox(
      value = length(unique(dat()$DriverNum)), subtitle = "Drivers",
      icon = icon("truck"), color = "orange"
    )
  })
  
  #Calculate BH rate
  output$bhrate <- renderValueBox({
    x <- optsummary
    valueBox(
      value = paste0(100*x$`BH Rate`, " %"), subtitle = "BH Rate",
      icon = icon("truck"), color = "green"
    )
  })
  
  #Calculate sent home drivers
  output$acceptrate <- renderValueBox({
    x <- optsummary
    valueBox(
      value = paste0(100*x$`SCG Accept Rate`, " %"), subtitle = "Acceptance Rate",
      icon = icon("truck"), color = "blue"
    )
  })
  
  #Change BH Count/rate every 5 seconds
  output$bh <- renderValueBox({
    valueBox(value = timer(), 
             subtitle = "Backhaul Drivers Count/Rate", color = "green", 
             icon = icon("users"), 
             width = 3
    )
  })
  
   
  output$table <- DT::renderDataTable({
    out <- dat()
    out <- out[, !colnames(out) %in% to_remove]
    
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
                        buttons = c('csv', 'excelHtml5', 'pdf', 'copy'),
                        text = 'Download', 
                        filename = 'Data export'
                      )
                    )
                  )
                ) %>% formatRound(to_round, digits = 2) %>% formatRound('LF_Review', 1)
  }, server = FALSE)
  
}
