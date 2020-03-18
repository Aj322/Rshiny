#UI - Plot tab

tabItem(
  "plot",
  fluidRow(
    column(
      width = 12,
      fluidRow(
        valueBox(value = length(unique(data$DriverNum)), 
                 subtitle = "Total Drivers", color = "orange", 
                 icon = icon("user", lib = "font-awesome"), 
                 width = 4
                 ), 
        valueBox(value = sum(dTerm$DomicileCount), 
                 subtitle = "Sent Home Drivers" , color = "red", 
                 icon = icon("users"), 
                 width = 4
                 ), 
        # valueBox(value = sum(dTerm$BHgivenCount), 
        #          subtitle = "Backhaul Drivers", color = "green", 
        #          icon = icon("truck"), 
        #          width = 3
        #          ), 
        # valueBox(value = OptRun$GoHomeDrivers, 
        #          subtitle = "Sent Home Drivers Count", color = "red", 
        #          icon = icon("users"), 
        #          width = 3
        # ), 
        valueBoxOutput("bh", width = 4), 
        # valueBox(value = OptRun$BHDrivers, 
        #          subtitle = "Backhaul Drivers Count", color = "green", 
        #          icon = icon("users"), 
        #          width = 3
        # ), 
        valueBox(value = paste(round(as.numeric(OptRun$LMPercent), digits = 2), "%"), 
                 subtitle = "Loaded Mile Percent", color = "aqua", 
                 icon = icon("percent"), 
                 width = 4
        ), 
        valueBox(value = prettyNum(x = OptRun$UnitsDelivered, big.mark = ","), 
                 subtitle = "Total Units Delivered", color = "yellow", 
                 icon = icon("shopping-cart"), 
                 width = 4
        ), 
        valueBox(value = prettyNum(x = OptRun$SavedEmptyMiles, big.mark = ","), 
                 subtitle = "Net Empty Miles Saved", color = "purple", 
                 icon = icon("hand-holding-usd", lib = "font-awesome"), 
                 width = 4
        )
      ), 
      fluidRow(
        box(
          width = 12, footer = paste0("This data corresponds to the latest published plan" ), 
          plot_ly(data = dTerm, x = ~Terminal, y = ~DomicileCount, type ='bar', name = 'Sent Home', height = "700px", 
                  hovertext = paste0("Terminal: ", dTerm$Terminal, " - ", dTerm$TerminalName, "<br>", 
                                     "# Drivers Received: ", dTerm$DriversSent, "<br>", 
                                     "# Sent Home: ", dTerm$DomicileCount, "<br>", 
                                     "# BH Received: ", dTerm$BHreceived, "<br>", 
                                     "# BH Given: ", dTerm$BHgivenCount)
                  ) %>%
            add_trace(y = ~BHreceived, name = 'BH Received') %>%
            add_trace(y = ~BHgivenCount, name = 'BH Given') %>%
            layout(title = "Drivers by Terminal", yaxis = list(title = 'Driver Count'), barmode = 'group')
        )
      )
    )
  )
)