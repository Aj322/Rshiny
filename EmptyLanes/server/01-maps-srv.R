## maps tab - creating leaflet map

output$map <- renderLeaflet({
  
  leaflet()%>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road Map")%>%
    mapOptions(zoomToLimits = "always")%>% 
    fitBounds(min(dat$`Last Drop Longitude`), min(dat$`Last Drop Latitude`), 
              max(dat$`Last Drop Longitude`), max(dat$`Last Drop Latitude`))%>%
    addMarkers(lng = as.numeric(term$Terminal.Longitude), lat = as.numeric(term$Terminal.Latitude), 
               group = "Terminals", icon = home, 
               options = markerOptions(riseOnHover = TRUE), 
               label = paste("Term: ", term$Ops.Div, " - ", term$Ops.Div.Name),
               popup = paste("Term: ", term$Ops.Div, " - ", term$Ops.Div.Name, "<br>", 
                             "# Drivers Sent: ", term$Count), 
               clusterOptions = markerClusterOptions()
    )
  
})


observe({ 
  
  d <- ops_filters()
  
  progress <- shiny::Progress$new()
  
  progress$set(message = "Loading Map ", value = 0)
  
  m <- leafletProxy("map", data = d) %>% clearMarkers() %>% clearShapes() %>% 
    clearGroup(c("Sent Home"))
  
  progress$inc(amount = 0.25, detail = "Creating Trips")
  
  for(j in 1:nrow(d)){
    m %>% addMarkers(lng = as.numeric(d$`Last Drop Longitude`[j]), lat = as.numeric(d$`Last Drop Latitude`[j]), 
                       layerId = 1, clusterId = 1, 
                       group = "Sent Home", icon = truck_org,
                       popup = paste0("Driver: ", d$Driver[j], " - ", d$`Driver ID`[j], "<br>", 
                                      "Home: ", d$`Ops Div`[j], " - ", d$`Ops Div Name`[j], "<br>", 
                                      "Trip Num: ", d$`Trip Number`[j], "<br>", 
                                      "LD: ", d$LastDropCity[j], ", ", d$`Last Drop State`[j], "<br>", 
                                      "Truck: ", d$`Truck Number`[j], " - ", d$`Truck Type`[j], "<br>", 
                                      "Called In: ", d$`Callin Datetime`[j], " - ", 
                                      d$`CallinUser First Name`[j], " ", d$`CallinUser Last Name`[j], "<br>",  
                                      "Empty Miles: ", d$`Empty Miles`[j], " miles", "<br>", 
                                      "Loaded Miles: ", d$`Original Loaded Miles`[j], " miles", "<br>", 
                                      "Running Miles: ", d$`Original Running Miles`[j], " miles", "<br>", 
                                      "Dispatch DateTime: ", dat$`Dispatch Date-Time`[j], "<br>", 
                                      "ETD DateTime: ", d$`ETD Date-Time`[j]
                       ), label = paste("Driver ID: ", d$`Driver ID`[j]), 
                       options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
      addLabelOnlyMarkers(lng = as.numeric(d$`Last Drop Longitude`[j]), lat = as.numeric(d$`Last Drop Latitude`[j]), 
                          layerId = 1, group = "Sent Home", 
                          label = substr(d$`Ops Div`[j], regexpr("[^0]", d$`Ops Div`[j]), nchar(d$`Ops Div`[j])), 
                          labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
                                                      direction = 'center', offset = c(0,5), 
                                                      textOnly = TRUE, textsize = "10px")) %>% 
      addPolylines(lng = as.numeric(d[j, c(24, 26)]), lat = as.numeric(d[j, c(23, 25)]), layerId = 1, 
                   group = "Sent Home", label = paste("Trip No.: ", d$`Trip Number`[j]),
                   # popup = paste("Trip# ", d$`Trip Number`[j], "<br>", 
                   #               "Driver ID: ", d$`Driver ID`[j], "<br>",
                   #               "From ", d$LastDropCity[j], ", ", d$`Last Drop State`, " to ", d$`Ops Div Name`[j], "<br>",
                   #               "Empty Miles: ", d$`Empty Miles`[j]),
                   color = "red", weight = 1#, dashArray = "5, 3, 2"#,
                   #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
      )
  }
  
  progress$inc(amount = 0.75, detail = "Rendering Trips")
  
  on.exit(progress$close())
  
})
  
  
  