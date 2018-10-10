tabItem(
  "plot",
  fluidRow(
    box(
      width = 12,
        plot_ly(data = term, x = ~Ops.Div, y = ~Count, 
                type = "bar", name = 'Sent Home', 
                text = ~Count, textposition = 'auto', textfont = list(color = "white"), 
                outsidetextfont = list(color = 'black'),
                hovertext = paste0("Terminal: ", term$Ops.Div, " - ", term$Ops.Div.Name, "<br>", 
                                 "# Drivers Sent: ", term$Count 
                                 )
      ) %>%
        layout(title = "Drivers Sent Home by Terminal", 
               xaxis = list(title = 'Terminal'), 
               yaxis = list(title = 'Driver Count')
               )
    )
  )
)


