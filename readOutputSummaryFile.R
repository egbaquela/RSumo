setwd("D:/Compartido/Proyectos/RSumo")

#requiere("readSumoXML.R")

readOutputSummaryFile <- function(path){
  summaryOutput <- readSumoXML(path)  
  summaryOutput$time <- as.numeric(as.character(summaryOutput$time))
  summaryOutput$loaded <- as.numeric(as.character(summaryOutput$loaded)) 
  summaryOutput$emitted <- as.numeric(as.character(summaryOutput$emitted)) 
  summaryOutput$running <- as.numeric(as.character(summaryOutput$running))  
  summaryOutput$waiting <- as.numeric(as.character(summaryOutput$waiting))  
  summaryOutput$ended <- as.numeric(as.character(summaryOutput$ended))  
  summaryOutput$meanWaitingTime <- as.numeric(as.character(summaryOutput$meanWaitingTime))  
  summaryOutput$meanTravelTime <- as.numeric(as.character(summaryOutput$meanTravelTime))  
  summaryOutput$duration <- as.numeric(as.character(summaryOutput$duration))  
  summaryOutput
}


