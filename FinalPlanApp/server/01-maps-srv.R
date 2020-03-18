## maps tab - creating leaflet map

output$map <- renderLeaflet({
  
  leaflet()%>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road Map")%>%
    mapOptions(zoomToLimits = "always")%>% 
    fitBounds(min(data$LDLong), min(data$LDLat), max(data$LDLong), max(data$LDLat))%>%
    addMarkers(lng = as.numeric(dTerm$Long), lat = as.numeric(dTerm$Lat), group = "Terminals", icon = home, 
               options = markerOptions(riseOnHover = TRUE), 
               label = paste("Term: ", dTerm$Terminal, " - ", dTerm$TerminalName),
               popup = paste("Term: ", dTerm$Terminal, " - ", dTerm$TerminalName, "<br>", 
                             "# Drivers Received: ", dTerm$DriversSent, "<br>", 
                             "# Sent Home: ", dTerm$DomicileCount, "<br>", 
                             "# BH Received: ", dTerm$BHreceived, "<br>", 
                             "# BH Given: ", dTerm$BHgivenCount), 
               clusterOptions = markerClusterOptions()
    ) %>% 
    addLegend(position = "bottomleft", 
              labels = c("Sent Empty", "Backhaulers", "Called-in Drivers", "Not Called-in Drivers","Terminals"), 
              colors = c("red", "green", "orange", "black", "blue"), 
              opacity = 0.75
    )
  
})

observe({

  d <- dat()
  
  if(input$iconSelect == "On") {

    progress <- shiny::Progress$new()
  
    progress$set(message = "Loading Map ", value = 0)
  
    m <- leafletProxy("map", data = d) %>% clearMarkers() %>% clearShapes() %>%
      clearGroup(c("Called-in Sent Home", "Not Called-in Driving Home", "Called-in Backhaulers", "Not Called-in Backhaulers"))
  
    #withProgress(message = "Loading map", value = 0, {
  
    for(j in which(d$FlagCalled == 1 & (d$STP_Review == 5 | d$STP_Review == 6))){
      if(!is.na(d$GISLat[j])) {
        
        m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 1, clusterId = 1,
                         group = "Called-in Sent Home", icon = truck_org,
                         popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]),
                         options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 1,
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])),
                              group = "Called-in Sent Home", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE,
                                                                                         direction = 'center', offset = c(0,5),
                                                                                         textOnly = TRUE, textsize = "10px")) %>%
          addPolylines(lng = as.numeric(d[j, c(18, 46)]), lat = as.numeric(d[j, c(17, 45)]), layerId = 1,
                       group = "Called-in Sent Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     #"From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "red", weight = 1, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          ) %>%
          addMarkers(lng = as.numeric(d$GISLong[j]), lat = as.numeric(d$GISLat[j]), layerId = 1, clusterId = 1,
                     group = "Called-in Sent Home", icon = pin,
                     popup = paste("Current Loc:", d$CurSTCity[j])
          ) %>%
          addPolylines(lng = as.numeric(d[j, c(46, 41)]), lat = as.numeric(d[j, c(45, 40)]), layerId = 1,
                       group = "Called-in Sent Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       color = "red", weight = 1#, dashArray = "5, 3, 2"
          )
      } else {
        m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 1, clusterId = 1,
                         group = "Called-in Sent Home", icon = truck_org,
                         popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]),
                         options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 1,
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])),
                              group = "Called-in Sent Home", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE,
                                                                                         direction = 'center', offset = c(0,5),
                                                                                         textOnly = TRUE, textsize = "10px")) %>%
          addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 1,
                       group = "Called-in Sent Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     #"From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "red", weight = 1#, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          )
      }
    }
  
    ##Step 1 completed
    progress$inc(amount = 0.25, detail = "Creating Trip 1/4")
  
  
    for(j in which(d$FlagCalled == 0 & (d$STP_Review == 5 | d$STP_Review == 6))){
      if(!is.na(d$GISLat[j])) {
        m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 2, clusterId = 2,
                         group = "Not Called-in Driving Home", icon = truck_org,
                         popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]),
                         options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 2,
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])),
                              group = "Not Called-in Driving Home", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE,
                                                                                         direction = 'center', offset = c(0,5),
                                                                                         textOnly = TRUE, textsize = "10px")) %>%
          addPolylines(lng = as.numeric(d[j, c(18, 46)]), lat = as.numeric(d[j, c(17, 45)]), layerId = 2,
                       group = "Not Called-in Driving Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     #"From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "red", weight = 1, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          ) %>%
          addMarkers(lng = as.numeric(d$GISLong[j]), lat = as.numeric(d$GISLat[j]), layerId = 2, clusterId = 2,
                     group = "Not Called-in Driving Home", icon = pin,
                     popup = paste("Current Loc:", d$CurSTCity[j])
          ) %>%
          addPolylines(lng = as.numeric(d[j, c(46, 41)]), lat = as.numeric(d[j, c(45, 40)]), layerId = 2,
                       group = "Not Called-in Driving Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       color = "red", weight = 1#, dashArray = "5, 3, 2"
          )
      } else {
        m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 2, clusterId = 2,
                         group = "Not Called-in Driving Home", icon = truck_black,
                         popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]),
                         options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 2,
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])),
                              group = "Not Called-in Driving Home", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE,
                                                                                                direction = 'center', offset = c(0,5),
                                                                                                textOnly = TRUE, textsize = "10px")) %>%
          addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 2,
                       group = "Not Called-in Driving Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "red", weight = 1#, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          )
      }
      
    }
  
    ##Step 2 completed
    progress$inc(amount = 0.25, detail = "Creating Trip 2/4")
  
    for(j in which(d$FlagCalled == 1 & (d$STP_Review >= 1 & d$STP_Review <= 4))){
      if(!is.na(d$GISLat[j])) {
        m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 3, clusterId = 3,
                         group = "Called-in Backhaulers", icon = truck_org,
                         popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]),
                         options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 3,
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])),
                              group = "Called-in Backhaulers", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE,
                                                                                                direction = 'center', offset = c(0,5),
                                                                                                textOnly = TRUE, textsize = "10px")) %>%
          addPolylines(lng = as.numeric(d[j, c(18, 46)]), lat = as.numeric(d[j, c(17, 45)]), layerId = 3,
                       group = "Called-in Backhaulers", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     #"From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "green", weight = 1, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          ) %>%
          addMarkers(lng = as.numeric(d$GISLong[j]), lat = as.numeric(d$GISLat[j]), layerId = 3, clusterId = 3,
                     group = "Called-in Backhaulers", icon = pin,
                     popup = paste("Current Loc:", d$CurSTCity[j])
          ) %>%
          addPolylines(lng = as.numeric(d[j, c(46, 41)]), lat = as.numeric(d[j, c(45, 40)]), layerId = 3,
                       group = "Called-in Backhaulers", label = paste("Trip No.: ", d$TripNumber[j]),
                       color = "green", weight = 1#, dashArray = "5, 3, 2"
          )
      } else {
        m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 3, clusterId = 3,
                         group = "Called-in Backhaulers", icon = truck_org,
                         popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]),
                         options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 3,
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])),
                              group = "Called-in Backhaulers", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE,
                                                                                           direction = 'center', offset = c(0,5),
                                                                                           textOnly = TRUE, textsize = "10px")) %>%
          addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 3,
                       group = "Called-in Backhaulers", #label = paste("Trip No.: ", dat1$Trip.Number[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]), color = "green", weight = 1#,
                       #weight = ifelse(d$DR[j] == 0 , max(d$DR), max(d$DR)-d$DR[j]+1),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          )  
      }
      
    }
  
    ##Step 3 completed
    progress$inc(amount = 0.25, detail = "Creating Trip 3/4")
  
  
    for(j in which(d$FlagCalled == 0 & (d$STP_Review >= 1 & d$STP_Review <= 4))){
      if(!is.na(d$GISLat[j])) {
        m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 4, clusterId = 4,
                         group = "Not Called-in Backhaulers", icon = truck_org,
                         popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]),
                         options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 4,
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])),
                              group = "Not Called-in Backhaulers", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE,
                                                                                           direction = 'center', offset = c(0,5),
                                                                                           textOnly = TRUE, textsize = "10px")) %>%
          addPolylines(lng = as.numeric(d[j, c(18, 46)]), lat = as.numeric(d[j, c(17, 45)]), layerId = 3,
                       group = "Not Called-in Backhaulers", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     #"From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "green", weight = 1, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          ) %>%
          addMarkers(lng = as.numeric(d$GISLong[j]), lat = as.numeric(d$GISLat[j]), layerId = 3, clusterId = 3,
                     group = "Not Called-in Backhaulers", icon = pin,
                     popup = paste("Current Loc:", d$CurSTCity[j])
          ) %>%
          addPolylines(lng = as.numeric(d[j, c(46, 41)]), lat = as.numeric(d[j, c(45, 40)]), layerId = 3,
                       group = "Not Called-in Backhaulers", label = paste("Trip No.: ", d$TripNumber[j]),
                       color = "green", weight = 1#, dashArray = "5, 3, 2"
          )
      } else {
        m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 4, clusterId = 4,
                         group = "Not Called-in Backhaulers", icon = truck_black,
                         popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]),
                         options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 4,
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])),
                              group = "Not Called-in Backhaulers", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE,
                                                                                               direction = 'center', offset = c(0,5),
                                                                                               textOnly = TRUE, textsize = "10px")) %>%
          addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 4,
                       group = "Not Called-in Backhaulers", #label = paste("Trip No.: ", dat1$Trip.Number[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]), color = "green", weight = 1#,
                       #weight = ifelse(d$DR[j] == 0 , max(d$DR), max(d$DR)-d$DR[j]+1),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          )
      }
      
    }
  
    ##Step 4 completed
    progress$inc(amount = 0.25, detail = "Creating Trip 4/4")
  
    #})
    on.exit(progress$close())
    
  } else {
    
    progress <- shiny::Progress$new()
    
    progress$set(message = "Loading Map ", value = 0)
    
    m <- leafletProxy("map", data = d) %>% clearMarkers() %>% clearShapes() %>% 
      clearGroup(c("Called-in Sent Home", "Not Called-in Driving Home", "Called-in Backhaulers", "Not Called-in Backhaulers"))
    
    #withProgress(message = "Loading map", value = 0, { 
    
    for(j in which(d$FlagCalled == 1 & (d$STP_Review == 5 | d$STP_Review == 6))){
      if(!is.na(d$GISLat[j])) {
        m %>% 
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 1, 
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
                              group = "Called-in Sent Home", 
                              labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
                                                          direction = 'center', offset = c(0,5),  
                                                          textOnly = TRUE, textsize = "10px", 
                                                          style = list(color = "orange")) 
          ) %>% 
          addPolylines(lng = as.numeric(d[j, c(18, 46)]), lat = as.numeric(d[j, c(17, 45)]), layerId = 1, 
                       group = "Called-in Sent Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     #"From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "red", weight = 1, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          ) %>%
          addMarkers(lng = as.numeric(d$GISLong[j]), lat = as.numeric(d$GISLat[j]), layerId = 1, clusterId = 1,
                     group = "Called-in Sent Home", icon = pin,
                     popup = paste("Current Loc:", d$CurSTCity[j])
          ) %>%
          addPolylines(lng = as.numeric(d[j, c(46, 41)]), lat = as.numeric(d[j, c(45, 40)]), layerId = 1,
                       group = "Called-in Sent Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       color = "red", weight = 1#, dashArray = "5, 3, 2"
          )
      } else {
        m %>% 
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 1, 
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
                              group = "Called-in Sent Home", 
                              labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
                                                          direction = 'center', offset = c(0,5),  
                                                          textOnly = TRUE, textsize = "10px", 
                                                          style = list(color = "orange")) 
          ) %>% 
          addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 1, 
                       group = "Called-in Sent Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "red", weight = 1, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          )  
      }
      
    }
    
    ##Step 1 completed
    progress$inc(amount = 0.25, detail = "Creating Trip 1/4")
    
    
    for(j in which(d$FlagCalled == 0 & (d$STP_Review == 5 | d$STP_Review == 6))){ 
      if(!is.na(d$GISLat[j])) {
        m %>% 
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 2, 
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
                              group = "Not Called-in Driving Home", 
                              labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
                                                          direction = 'center', offset = c(0,5),  
                                                          textOnly = TRUE, textsize = "10px", 
                                                          style = list(color = "orange")) 
          ) %>% 
          addPolylines(lng = as.numeric(d[j, c(18, 46)]), lat = as.numeric(d[j, c(17, 45)]), layerId = 2, 
                       group = "Not Called-in Driving Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     #"From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "red", weight = 1, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          ) %>%
          addMarkers(lng = as.numeric(d$GISLong[j]), lat = as.numeric(d$GISLat[j]), layerId = 2, clusterId = 2,
                     group = "Not Called-in Driving Home", icon = pin,
                     popup = paste("Current Loc:", d$CurSTCity[j])
          ) %>%
          addPolylines(lng = as.numeric(d[j, c(46, 41)]), lat = as.numeric(d[j, c(45, 40)]), layerId = 2,
                       group = "Not Called-in Driving Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       color = "red", weight = 1#, dashArray = "5, 3, 2"
          )
      } else {
        m %>% 
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 2, 
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
                              group = "Not Called-in Driving Home", 
                              labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
                                                          direction = 'center', offset = c(0,5), 
                                                          textOnly = TRUE, textsize = "10px")) %>% 
          addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 2, 
                       group = "Not Called-in Driving Home", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "red", weight = 1, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          )
      }
      
    }
    
    ##Step 2 completed
    progress$inc(amount = 0.25, detail = "Creating Trip 2/4")
    
    for(j in which(d$FlagCalled == 1 & (d$STP_Review >= 1 & d$STP_Review <= 4))){
      if(!is.na(d$GISLat[j])) {
        m %>% 
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 3, 
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
                              group = "Called-in Backhaulers", 
                              labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
                                                          direction = 'center', offset = c(0,5),  
                                                          textOnly = TRUE, textsize = "10px", 
                                                          style = list(color = "orange")) 
          ) %>% 
          addPolylines(lng = as.numeric(d[j, c(18, 46)]), lat = as.numeric(d[j, c(17, 45)]), layerId = 3, 
                       group = "Called-in Backhaulers", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     #"From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "red", weight = 1, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          ) %>%
          addMarkers(lng = as.numeric(d$GISLong[j]), lat = as.numeric(d$GISLat[j]), layerId = 3, clusterId = 3,
                     group = "Called-in Backhaulers", icon = pin,
                     popup = paste("Current Loc:", d$CurSTCity[j])
          ) %>%
          addPolylines(lng = as.numeric(d[j, c(46, 41)]), lat = as.numeric(d[j, c(45, 40)]), layerId = 3,
                       group = "Called-in Backhaulers", label = paste("Trip No.: ", d$TripNumber[j]),
                       color = "red", weight = 1#, dashArray = "5, 3, 2"
          )
      } else {
        m %>% 
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 3, 
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
                              group = "Called-in Backhaulers", 
                              labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
                                                          direction = 'center', offset = c(0,5),  
                                                          textOnly = TRUE, textsize = "10px", 
                                                          style = list(color = "orange"))
          ) %>% 
          addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 3, 
                       group = "Called-in Backhaulers", #label = paste("Trip No.: ", dat1$Trip.Number[j]), 
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>", 
                                     "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]), color = "green", weight = 1#, 
                       #weight = ifelse(d$DR[j] == 0 , max(d$DR), max(d$DR)-d$DR[j]+1), 
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          )
      }
      
    }
    
    ##Step 3 completed
    progress$inc(amount = 0.25, detail = "Creating Trip 3/4")
    
    
    for(j in which(d$FlagCalled == 0 & (d$STP_Review >= 1 & d$STP_Review <= 4))){
      if(!is.na(d$GISLat[j])) {
        m %>% 
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 4, 
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
                              group = "Not Called-in Backhaulers", 
                              labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
                                                          direction = 'center', offset = c(0,5),  
                                                          textOnly = TRUE, textsize = "10px", 
                                                          style = list(color = "orange")) 
          ) %>% 
          addPolylines(lng = as.numeric(d[j, c(18, 46)]), lat = as.numeric(d[j, c(17, 45)]), layerId = 4, 
                       group = "Not Called-in Backhaulers", label = paste("Trip No.: ", d$TripNumber[j]),
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
                                     #"From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]),
                       color = "red", weight = 1, dashArray = "5, 3, 2"#,
                       #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          ) %>%
          addMarkers(lng = as.numeric(d$GISLong[j]), lat = as.numeric(d$GISLat[j]), layerId = 4, clusterId = 4,
                     group = "Not Called-in Backhaulers", icon = pin,
                     popup = paste("Current Loc:", d$CurSTCity[j])
          ) %>%
          addPolylines(lng = as.numeric(d[j, c(46, 41)]), lat = as.numeric(d[j, c(45, 40)]), layerId = 4,
                       group = "Not Called-in Backhaulers", label = paste("Trip No.: ", d$TripNumber[j]),
                       color = "red", weight = 1#, dashArray = "5, 3, 2"
          )
      } else {
        m %>% 
          addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 4, 
                              label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
                              group = "Not Called-in Backhaulers", 
                              labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
                                                          direction = 'center', offset = c(0,5), 
                                                          textOnly = TRUE, textsize = "10px")
          ) %>% 
          addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 4, 
                       group = "Not Called-in Backhaulers", #label = paste("Trip No.: ", dat1$Trip.Number[j]), 
                       popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>", 
                                     "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
                                     "Drops Remain: ", d$DR[j]), color = "green", weight = 1#, 
                       #weight = ifelse(d$DR[j] == 0 , max(d$DR), max(d$DR)-d$DR[j]+1), 
                       #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
          )
      }
      
    }
    
    ##Step 4 completed
    progress$inc(amount = 0.25, detail = "Creating Trip 4/4")
    
    #})
    on.exit(progress$close())
    
  }

})


# observeEvent(input$iconSelect, { 
#   
#   d <- dat()
#   
#   if(input$iconSelect == "Off") {
#   
#     progress <- shiny::Progress$new()
#     
#     progress$set(message = "Loading Map ", value = 0)
#     
#     m <- leafletProxy("map", data = d) %>% clearMarkers() %>% clearShapes() %>% 
#       clearGroup(c("Called-in Sent Home", "Not Called-in Driving Home", "Called-in Backhaulers", "Not Called-in Backhaulers"))
#     
#     #withProgress(message = "Loading map", value = 0, { 
#     
#     for(j in which(d$FlagCalled == 1 & (d$STP_Review == 5 | d$STP_Review == 6))){
#       m %>% 
#         addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 1, 
#                             label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
#                             group = "Called-in Sent Home", 
#                             labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
#                                                         direction = 'center', offset = c(0,5),  
#                                                         textOnly = TRUE, textsize = "10px", 
#                                                         style = list(color = "orange")) 
#                             ) %>% 
#         addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 1, 
#                      group = "Called-in Sent Home", label = paste("Trip No.: ", d$TripNumber[j]),
#                      popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
#                                    "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
#                                    "Drops Remain: ", d$DR[j]),
#                      color = "red", weight = 1, dashArray = "5, 3, 2"#,
#                      #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
#                      #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
#         )
#     }
#     
#     ##Step 1 completed
#     progress$inc(amount = 0.25, detail = "Creating Trip 1/4")
#     
#     
#     for(j in which(d$FlagCalled == 0 & (d$STP_Review == 5 | d$STP_Review == 6))){
#       m %>% 
#         addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 2, 
#                             label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
#                             group = "Not Called-in Driving Home", 
#                             labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
#                                                         direction = 'center', offset = c(0,5), 
#                                                         textOnly = TRUE, textsize = "10px")) %>% 
#         addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 2, 
#                      group = "Not Called-in Driving Home", label = paste("Trip No.: ", d$TripNumber[j]),
#                      popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
#                                    "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
#                                    "Drops Remain: ", d$DR[j]),
#                      color = "red", weight = 1, dashArray = "5, 3, 2"#,
#                      #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
#                      #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
#         )
#     }
#     
#     ##Step 2 completed
#     progress$inc(amount = 0.25, detail = "Creating Trip 2/4")
#     
#     for(j in which(d$FlagCalled == 1 & (d$STP_Review >= 1 & d$STP_Review <= 4))){
#       m %>% 
#         addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 3, 
#                             label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
#                             group = "Called-in Backhaulers", 
#                             labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
#                                                         direction = 'center', offset = c(0,5),  
#                                                         textOnly = TRUE, textsize = "10px", 
#                                                         style = list(color = "orange"))
#                             ) %>% 
#         addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 3, 
#                      group = "Called-in Backhaulers", #label = paste("Trip No.: ", dat1$Trip.Number[j]), 
#                      popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>", 
#                                    "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
#                                    "Drops Remain: ", d$DR[j]), color = "green", weight = 1#, 
#                      #weight = ifelse(d$DR[j] == 0 , max(d$DR), max(d$DR)-d$DR[j]+1), 
#                      #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
#         )
#     }
#     
#     ##Step 3 completed
#     progress$inc(amount = 0.25, detail = "Creating Trip 3/4")
#     
#     
#     for(j in which(d$FlagCalled == 0 & (d$STP_Review >= 1 & d$STP_Review <= 4))){
#       m %>% 
#         addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 4, 
#                             label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
#                             group = "Not Called-in Backhaulers", 
#                             labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
#                                                         direction = 'center', offset = c(0,5), 
#                                                         textOnly = TRUE, textsize = "10px")
#                             ) %>% 
#         addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 4, 
#                      group = "Not Called-in Backhaulers", #label = paste("Trip No.: ", dat1$Trip.Number[j]), 
#                      popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>", 
#                                    "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
#                                    "Drops Remain: ", d$DR[j]), color = "green", weight = 1#, 
#                      #weight = ifelse(d$DR[j] == 0 , max(d$DR), max(d$DR)-d$DR[j]+1), 
#                      #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
#         )
#     }
#     
#     ##Step 4 completed
#     progress$inc(amount = 0.25, detail = "Creating Trip 4/4")
#     
#     #})
#     on.exit(progress$close())
#   
#   } else if (input$iconSelect == "On") {
#     
#     d <- dat()
#     
#     progress <- shiny::Progress$new()
#     
#     progress$set(message = "Loading Map ", value = 0)
#     
#     m <- leafletProxy("map", data = d) %>% clearMarkers() %>% clearShapes() %>% 
#       clearGroup(c("Called-in Sent Home", "Not Called-in Driving Home", "Called-in Backhaulers", "Not Called-in Backhaulers"))
#     
#     #withProgress(message = "Loading map", value = 0, { 
#     
#     for(j in which(d$FlagCalled == 1 & (d$STP_Review == 5 | d$STP_Review == 6))){
#       m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 1, clusterId = 1, 
#                        group = "Called-in Sent Home", icon = truck_org,
#                        popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]), 
#                        options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
#         addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 1, 
#                             label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
#                             group = "Called-in Sent Home", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
#                                                                                        direction = 'center', offset = c(0,5),  
#                                                                                        textOnly = TRUE, textsize = "10px")) %>% 
#         addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 1, 
#                      group = "Called-in Sent Home", label = paste("Trip No.: ", d$TripNumber[j]),
#                      popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
#                                    "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
#                                    "Drops Remain: ", d$DR[j]),
#                      color = "red", weight = 1, dashArray = "5, 3, 2"#,
#                      #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
#                      #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
#         )
#     }
#     
#     ##Step 1 completed
#     progress$inc(amount = 0.25, detail = "Creating Trip 1/4")
#     
#     
#     for(j in which(d$FlagCalled == 0 & (d$STP_Review == 5 | d$STP_Review == 6))){
#       m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 2, clusterId = 2, 
#                        group = "Not Called-in Driving Home", icon = truck_black,
#                        popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]), 
#                        options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
#         addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 2, 
#                             label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
#                             group = "Not Called-in Driving Home", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
#                                                                                               direction = 'center', offset = c(0,5), 
#                                                                                               textOnly = TRUE, textsize = "10px")) %>% 
#         addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 2, 
#                      group = "Not Called-in Driving Home", label = paste("Trip No.: ", d$TripNumber[j]),
#                      popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>",
#                                    "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
#                                    "Drops Remain: ", d$DR[j]),
#                      color = "red", weight = 1, dashArray = "5, 3, 2"#,
#                      #weight = ifelse(d$DR[i] == 0, max(d$DR), max(d$DR)-d$DR[i]+2),
#                      #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
#         )
#     }
#     
#     ##Step 2 completed
#     progress$inc(amount = 0.25, detail = "Creating Trip 2/4")
#     
#     for(j in which(d$FlagCalled == 1 & (d$STP_Review >= 1 & d$STP_Review <= 4))){
#       m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 3, clusterId = 3, 
#                        group = "Called-in Backhaulers", icon = truck_org,
#                        popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]), 
#                        options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
#         addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 3, 
#                             label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
#                             group = "Called-in Backhaulers", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
#                                                                                          direction = 'center', offset = c(0,5),  
#                                                                                          textOnly = TRUE, textsize = "10px")) %>% 
#         addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 3, 
#                      group = "Called-in Backhaulers", #label = paste("Trip No.: ", dat1$Trip.Number[j]), 
#                      popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>", 
#                                    "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
#                                    "Drops Remain: ", d$DR[j]), color = "green", weight = 1#, 
#                      #weight = ifelse(d$DR[j] == 0 , max(d$DR), max(d$DR)-d$DR[j]+1), 
#                      #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
#         )
#     }
#     
#     ##Step 3 completed
#     progress$inc(amount = 0.25, detail = "Creating Trip 3/4")
#     
#     
#     for(j in which(d$FlagCalled == 0 & (d$STP_Review >= 1 & d$STP_Review <= 4))){
#       m %>% addMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 4, clusterId = 4, 
#                        group = "Not Called-in Backhaulers", icon = truck_black,
#                        popup = content(d, j), label = paste("Current Loc - ", d$CurSTCity[j]), 
#                        options= markerOptions(riseOnHover = TRUE), clusterOptions = markerClusterOptions())%>%
#         addLabelOnlyMarkers(lng = as.numeric(d$LDLong[j]), lat = as.numeric(d$LDLat[j]), layerId = 4, 
#                             label = substr(d$Home[j], regexpr("[^0]", d$Home[j]), nchar(d$Home[j])), 
#                             group = "Not Called-in Backhaulers", labelOptions = labelOptions(clickable = TRUE, noHide = TRUE, 
#                                                                                              direction = 'center', offset = c(0,5), 
#                                                                                              textOnly = TRUE, textsize = "10px")) %>% 
#         addPolylines(lng = as.numeric(d[j, c(18, 41)]), lat = as.numeric(d[j, c(17, 40)]), layerId = 4, 
#                      group = "Not Called-in Backhaulers", #label = paste("Trip No.: ", dat1$Trip.Number[j]), 
#                      popup = paste("Trip# ", d$TripNumber[j], "<br>", "Driver ID: ", d$DriverNum[j], "<br>", 
#                                    "From ", d$LastDrop[j], " to ", d$STT_TermName[j], "<br>",
#                                    "Drops Remain: ", d$DR[j]), color = "green", weight = 1#, 
#                      #weight = ifelse(d$DR[j] == 0 , max(d$DR), max(d$DR)-d$DR[j]+1), 
#                      #highlightOptions = highlightOptions(color = 'blue', weight = 3, bringToFront = TRUE)
#         )
#     }
#     
#     ##Step 4 completed
#     progress$inc(amount = 0.25, detail = "Creating Trip 4/4")
#     
#     #})
#     on.exit(progress$close())
#     
#   }
#   
# })
