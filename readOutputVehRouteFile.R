#requiere("readSumoXML.R")

readOutputVehRouteFile <- function(path){
  vehRoute <- readSumoXML(path)  
  vehRoute$depart <- as.numeric(as.character(vehRoute$depart))
  vehRoute$arrival <- as.numeric(as.character(vehRoute$arrival)) 
  vehRoute
}