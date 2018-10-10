library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(shinycssloaders)
library(sqldf)
library(leaflet)
library(DT)
library(plotly)
library(xlsx)

home <- makeIcon(iconUrl = "www/img/home1.png",
                 iconWidth = 15, iconHeight = 20)
truck_org <- makeIcon(iconUrl = "www/img/transport.png",
                      iconWidth = 25, iconHeight = 30)

dat <- read.xlsx2(file = "data/EmptyLanes.xlsx", 
                  sheetIndex = 1, check.names = FALSE, colClasses = NA, stringsAsFactors = FALSE)

dTerm <- read.xlsx2(file = "data/EmptyLanes.xlsx", 
                    sheetIndex = 2, check.names = FALSE, colClasses = NA, stringsAsFactors = FALSE)

dat <- dat[!apply(is.na(dat) | dat == "", 1, "all"), ]
dTerm <- dTerm[!apply(is.na(dTerm) | dTerm == "", 1, "all"), ]

dat <- sqldf("Select a.*, b.[Terminal Latitude], b.[Terminal Longitude]
             From dat a INNER JOIN dTerm b
             ON a.[Ops Div] = b.Terminal")

dat <- dat %>% dplyr::filter(as.character.Date(`Dispatch Date-Time`) >= Sys.Date() - 10)

dat <- dat %>% dplyr::filter(`Sent To Inst` == `Return Division`)

term <- data.frame( dat %>%
                      group_by(`Ops Div`, `Ops Div Name`, `Terminal Latitude`, `Terminal Longitude`) %>% 
                      summarise(Count = n())
)



