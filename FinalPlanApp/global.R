library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(shinycssloaders)
library(htmlwidgets)
library(sqldf)
library(leaflet)
library(DT)
library(plotly)
library(shinydashboardPlus)
library(xlsx)



data <- read.xlsx2(file = "data/FinalPlanData.xlsx", sheetName = "data", check.names = FALSE, colClasses = NA, stringsAsFactors = FALSE)
dTerm <- read.xlsx2(file = "data/FinalPlanData.xlsx", sheetName = "dTerm", check.names = FALSE, colClasses = NA, stringsAsFactors = FALSE)
optsummary <- read.xlsx2(file = "data/FinalPlanData.xlsx", sheetName = "OptSummary", check.names = FALSE, colClasses = NA, stringsAsFactors = FALSE)
OptRun <- read.xlsx2(file = "data/FinalPlanData.xlsx", sheetName = "ModelSummary", check.names = FALSE, colClasses = NA, stringsAsFactors = FALSE)

home <- makeIcon(iconUrl = "www/img/home1.png",
                 iconWidth = 15, iconHeight = 20)
truck_org <- makeIcon(iconUrl = "www/img/transport.png",
                  iconWidth = 25, iconHeight = 30)
truck_black <- makeIcon(iconUrl = "www/img/truck.png",
                  iconWidth = 25, iconHeight = 30)
pin <- makeIcon(iconUrl = "www/img/purplepin.png",
                iconWidth = 13, iconHeight = 18)

data['ETA'] <- as.numeric(data$ETA)

content <- function(df, i) {
  return(paste0("Driver: ", df$DriverName[i], " - ", df$DriverNum[i], "<br>",
                "Home: ", df$Home[i], " - ", df$TerminalName[i], "<br>",
                "Trip #: ", df$TripNumber[i], "<br>",
                "Current Loc: ", df$CurSTCity[i], "<br>",
                "LD: ", df$LastDrop[i], "<br>",
                "Drops - ", ifelse(is.na(df$DM[i]), 0, df$DM[i]), " of ", df$SkidDrops[i], "<br>",
                "Truck: ", df$TruckNum[i], " - ", df$TruckType[i], "<br>",
                "Send to: ", df$STT_Review[i], " - ", df$STP_Review[i], "<br>",
                "Called In: ", ifelse(df$FlagCalled[i] == 1, paste(df$CallinDate[i], ", ", df$User[i]), "No"), "<br>",
                "Mi. to home - ", round(df$Dis2Home[i], 2), " miles", "<br>",
                "Mi. to STT - ", df$Miles2STT[i], " miles", "<br>",
                "ETA: ", round(as.numeric(df$ETA[i]), 2), " hrs" , "<br>", "System NAD: ", df$SystemNAD[i]
    )
  )
}

data %>% mutate_at(vars(Dis2LD, Dis2Home, ETA, Miles2STT), funs(round(., 2)))

data$LF_Review <- round( as.numeric(data$LF_Review), 1)

to_remove <- c('LDLat', 'LDLong', 'STTLat', 'STTLong')

to_round <- c('Dis2LD', 'Dis2Home', 'ETA', 'Miles2STT')

data <- data[1:100,]

# db <- odbcDriverConnect('driver={SQL Server}; Server=SRV-SCG\\LLAMASOFT2014; database=Gurobi_Prod; trusted_connection=TRUE')
# dTerm <- sqlQuery(db, as.is = TRUE, 'Select * from grb_dTerminal')
# DriverDetails <- sqlQuery(db, as.is = TRUE, 'Select DriverID, Lift_CityST, LDLat, LDLong, TruckType, Legs, LoadedMiles, STT, STP, CallIn_Status, 
#                           Prehook, TripNumber, TruckNumber from grb_DriverDetails')
# DriverOutput <- sqlQuery(db, as.is = TRUE, 'Select * from grb_OutputDriverResults')
# CurScenario = max(DriverOutput$OptID)
# DriverOutput <- filter(DriverOutput, DriverOutput$OptID == CurScenario)
# 
# acc <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=\\\\crpgroup\\Shared Files\\NetworkPlanning\\SCG\\BH Lift Review For Terminals_Data - 7-26-2016.accdb")
# plan <- sqlQuery(acc, as.is = TRUE, 'Select * from tbl_SCG_DriverNextStops')
# optsummary <- sqlQuery(acc, as.is = TRUE, "Select * from tbl_SCG_OptRunSummary")
# 
# odbcCloseAll()
# 
# #data <- merge(plan, DriverDetails, by.x = "DriverNum", by.y = "DriverID")
# data <- sqldf("Select x.TerminalName, y.* From dTerm x Inner Join 
#               (Select a.DriverNum, a.DriverName, a.TripNumber, a.STT_ALT, a.STT_eTYMS, a.STP_eTYMS, a.STT_Review, 
#               a.STP_Review, a.STT_RealSent2, a.Dis2LD, a.LM, a.Dis2Home, a.Home, a.CurSTCity, a.LastDrop, b.LDLat, 
#               b.LDLong, a.Legs, a.DM, a.DR, a.Prehook, a.TruckNum, a.TruckType, a.TruckDesp, a.LF_Review, a.ETA, 
#               a.Arrive, a.CallinDate, a.User, a.SystemETD, a.SystemNAD, a.Reviewed, a.`Use Alt Plan`, a.SentMessage
#               From plan a Inner Join DriverDetails b 
#               ON a.DriverNum = b.DriverID and a.TripNumber = b.TripNumber) y 
#               ON x.Terminal = y.Home")
# 
# data['FlagCalled'] <- if_else(is.na(data$CallinDate), 0, 1)
# data['SkidDrops'] <- data$DM + data$DR
# data['SentHome'] <- if_else(data$Home == data$STT_Review, 1, 0)
# 
# data <- sqldf("Select a.*, b.TerminalName as STT_TermName, b.Lat as STTLat, b.Long as STTLong 
#               From data a LEFT JOIN dTerm b
#               ON a.STT_Review = b.Terminal")
# 
# data['Called'] <- if_else(is.na(data$CallinDate), "Not Called-in", "Called-in")
# data['TripType'] <- if_else(data$Home == data$STT_Review, "Sent Home", "Backhaul")
# data['Miles2STT'] <- numeric()
# 
# optsummary <- optsummary[nrow(optsummary),]
# optsummary['UserName'] <- if(optsummary$User == 'ggoll') {
#   "Gerald Goll"  
# } else if(optsummary$User == 'jhill') {
#   "John Hill"
# } else if(optsummary$User == 'coleirwin') {
#   "Cole Irwin"
# }
# 
# get_miles <- function(idx, df) {
#   route <- osrmRoute(src = c(paste0("A", idx), as.numeric(df$LDLong[idx]), as.numeric(df$LDLat[idx])), 
#                      dst = c(paste0("B", idx), as.numeric(df$STTLong[idx]), as.numeric(df$STTLat[idx])), 
#                      sp = TRUE)
#   
#   return(as.numeric(round( route[["distance"]]/1.61, digits = 0)))
# }
# 
# #################################### Calculating HHG Miles ######################################
# cores <- detectCores()/2
# x<-list()
# cluster <- makeCluster(cores)
# registerDoParallel(cluster)
# 
# clusterExport(cluster, c("data"))
# clusterEvalQ(cluster, library(osrm) )
# 
# x<-foreach(i = 1:nrow(data), 
#            .combine = c, .packages = "osrm"  ) %dopar% {
#              get_miles(i, data)  
#            }
# 
# ##updating the miles into df
# start = 1
# 
# for(m in 1:nrow(data)){
#   for(k in start){
#     data$Miles2STT[m] <- x[k]
#     break;
#   }
#   start <- start + 1
#   if(start > nrow(data)) {
#     break;
#   }
# }
# 
# stopCluster(cluster)
# stopImplicitCluster()
# 
# ################################################################################################
# 
# 
# termsummary <- data %>% group_by(STT_Review) %>% summarise(Count = n())
# sh <- data %>% filter(SentHome == 1) %>% group_by(STT_Review, SentHome) %>% summarise(DomicileCount = n())
# bhgiven <- data %>% filter(SentHome == 0) %>% group_by(Home, SentHome) %>% summarise(BHgivenCount = n())
# #bh <- data %>% filter(SentHome == 0) %>% group_by(STT_Review, SentHome) %>% summarise(Count = n())
# 
# dTerm <- sqldf("Select a.*, b.Count as DriversSent 
#                From dTerm a INNER JOIN termsummary b 
#                ON a.Terminal = b.STT_Review")
# 
# dTerm <- sqldf("Select a.*, b.BHgivenCount 
#                From dTerm a LEFT JOIN bhgiven b 
#                ON a.Terminal = b.Home")
# 
# dTerm$BHgivenCount[is.na(dTerm$BHgivenCount)] <- 0
# 
# dTerm <- sqldf("Select a.*, b.DomicileCount 
#                From dTerm a LEFT JOIN sh b 
#                ON a.Terminal = b.STT_Review")
# 
# dTerm$DomicileCount[is.na(dTerm$DomicileCount)] <- 0
# 
# dTerm['BHreceived'] <- dTerm$DriversSent - dTerm$DomicileCount
