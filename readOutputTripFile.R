requiere("readSumoXML.R")

readOutputTripFile <- function(path){
  tripFile <- readSumoXML(path)
  tripFile$depart <- as.numeric(as.character(tripFile$depart))
  tripFile$departPos <- as.numeric(as.character(tripFile$departPos))  
  tripFile$departSpeed <- as.numeric(as.character(tripFile$departSpeed))  
  tripFile$departDelay <- as.numeric(as.character(tripFile$departDelay))  
  tripFile$arrival <- as.numeric(as.character(tripFile$arrival)) 
  tripFile$arrivalPos <- as.numeric(as.character(tripFile$arrivalPos))
  tripFile$arrivalSpeed <- as.numeric(as.character(tripFile$arrivalSpeed))  
  tripFile$duration <- as.numeric(as.character(tripFile$duration)) 
  tripFile$routeLength <- as.numeric(as.character(tripFile$routeLength))  
  tripFile$waitSteps <- as.numeric(as.character(tripFile$waitSteps))  
  tripFile$rerouteNo <- as.numeric(as.character(tripFile$rerouteNo))  
  tripFile
}
