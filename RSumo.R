###################################################
# 
# Modulo para conectividad con SUMO
#
###################################################
 
require("XML")
require(stats)


############## Sumo Manager########################
setClass("adminRSumo",
  representation(
    name ="character", # Instance name 
    sumoBinPath = "character" # Path to sumo.exe and others    
  )
)

# Constructor
adminRSumo <- function(name, sumoBinPath){
  new("adminRSumo",  name = name, sumoBinPath = sumoBinPath)
}

setGeneric("openSumoGUI", function(object){})

setMethod("openSumoGUI", "adminRSumo", 
  function(object){
    shell(paste(object@sumoBinPath, "sumo-gui.exe", sep=""))  
  } 
)

setGeneric("runSimulationFromCfg", function(object, pathCFG, activeGUI=FALSE){})

setMethod("runSimulationFromCfg", "adminRSumo",
  function(object, pathCFG, activeGUI=FALSE){
    if (activeGUI){
      sumo<-"sumo-gui.exe"
    }
    else{
      sumo<-"sumo.exe"
    }
    shell(paste(object@sumoBinPath, sumo," -c ","\"",pathCFG,"\"",sep=""))    
  }
)

setGeneric("runSimulationFromFiles", function(object, pathNet, pathRoute, endTime, activeGUI=FALSE,
  pathOutputs=NA, reportTripInfo = FALSE, reportVehRoute = FALSE,
  reportSummary = FALSE){})

setMethod("runSimulationFromFiles", "adminRSumo",
  function(object, pathNet, pathRoute, endTime, activeGUI=FALSE, 
    pathOutputs=NA, reportTripInfo = FALSE, reportVehRoute = FALSE,
    reportSummary = FALSE){
    if (activeGUI){
      sumo<-"sumo-gui.exe"
    }
    else{
      sumo<-"sumo.exe"
    }
    command <- paste(object@sumoBinPath, sumo,sep="") 
    command <- paste(command," --net-file=\"",pathNet, "\"", sep="")
    command <- paste(command," --route-files=\"",pathRoute, "\"", sep="")
    command <- paste(command, " --time-to-teleport=\"-1\"", sep="")
    command <- paste(command, " --end=\"", endTime, "\"", sep="")
    if (!is.na(pathOutputs)){
      if (reportTripInfo){
        command <- paste(command, " --tripinfo-output=\"", pathOutputs, "trips.trip.xml\"", sep="")
      }
      if (reportVehRoute){
        command <- paste(command, " --vehroute-output=\"", pathOutputs, "vehRoute.vehr.xml\"", sep="")
      }    
      if (reportSummary){
        command <- paste(command, " --summary=\"", pathOutputs, "summary.summ.xml\"", sep="")
      }      
    }
   
    shell(command)    
  }        
)

setGeneric("generateNetFromCFG", function(object, pathCFG){})

#Cambiar netgen por netgenerate
setMethod("generateNetFromCFG", "adminRSumo", 
          function(object, pathCFG){
            shell(paste(object@sumoBinPath, "netgen.exe"," -c=\"", pathCFG,"\"", sep=""))  
          } 
)

setGeneric("generateRandomNet", function(object, iterations, pathOutput){})

#Cambiar netgen por netgenerate
setMethod("generateRandomNet", "adminRSumo", 
          function(object,  iterations, pathOutput){
            shell(paste(object@sumoBinPath, "netgen.exe"," --random-net --rand-iterations=", iterations, " -o=\"",pathOutput,"\"", sep=""))  
          } 
)

setGeneric("generateRandomTrips", 
           function(object, pathNet, pathOutput, generateRoute = TRUE, 
                    tripPrefix = "t", tripParams=NA, beginTime=0, endTime=3600, 
                    period=1,seed=NA, lengthWeigth=NA, lanesWeigth=NA,
                    speedWeigth=NA, fringeFactor=1, fringeThreshold=NA,
                    min-distance=0){})

setMethod("generateRandomTrips", "adminRSumo", 
          function(object, pathNet, pathOutput, generateRoute = TRUE, 
                   tripPrefix = "t", tripParams=NA, beginTime=0, endTime=3600, 
                   period=1,seed=NA, lengthWeigth=NA, lanesWeigth=NA,
                   speedWeigth=NA, fringeFactor=1, fringeThreshold=NA,
                   min-distance=0){
            # Versión anterior, llamaba a DUAROUTER, pero genera errores
            # shell(paste(object@sumoBinPath, "duarouter.exe"," -n=\"", pathNet,"\""," -o=\"", pathOutput,"\""," -b=", beginTime," -e=", endTime, " -R=0.1", sep="")) 
            if (!is.na(seed)){set.seed(seed)} 
            # Pendiente: DESARROLLAR LECTURA DE ARCHIVOS .net para asignar trips
          } 
)

############ Read of Sumo XML files #################

xmlNodesAttrToDataFrame <- function(xmlNode){
  myXmlAttr <- xmlApply(xmlNode, xmlAttrs)
  attrDataFrame <- t(as.data.frame(myXmlAttr))
  attrDataFrame <- as.data.frame(attrDataFrame)    
}

readSumoXML <- function(path){
  myXml <- newXMLDoc()
  myXml <-xmlRoot(xmlParse(path))
  xmlNodesAttrToDataFrame(myXml) 
}

############ Read of output files ###################

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

readOutputVehRouteFile <- function(path){
  vehRoute <- readSumoXML(path)  
  vehRoute$depart <- as.numeric(as.character(vehRoute$depart))
  vehRoute$arrival <- as.numeric(as.character(vehRoute$arrival)) 
  vehRoute
}

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


############ Read TSPLIB XML Models #################

readTSPLIBModel <- function(path){
  myXml <- newXMLDoc()
  myXml <-xmlRoot(xmlParse(path))
  listaVertices <- getNodeSet(myXml, "/travellingSalesmanProblemInstance/graph/vertex")
  graph <- data.frame()
  for (i in 1:length(listaVertices)){
    conections <-  xmlToDataFrame(xmlChildren(listaVertices[[i]])) 
    costs <- xmlNodesAttrToDataFrame(listaVertices[[1]])
    auxGraph <- cbind(i-1,conections, costs)
    names(auxGraph) <- c("node","to", "cost")
    graph <- rbind(graph, auxGraph)
  }
  rownames(graph) <- NULL
  graph
}


############ Class for models #######################

setClass("trafficNodes",
  representation(
    name = "character",
    path = "character",
    nodes = "data.frame"
  )
)

trafficNodes <- function(name, path, nodes){
  new("trafficNodes", name = name, path = path, nodes = nodes)  
}

setClass("trafficEdges",
  representation(
    name = "character",
    path = "character",
    edges = "data.frame"
  )
)

trafficEdges <- function(name, path, edges){
  new("trafficEdges", name = name, path = path, edges = edges)  
}

setClass("trafficNet",
  representation(
    name = "character",
    path = "character",
    net = "data.frame",
    nodes = "trafficNodes",
    edges = "trafficEdges"
  )
)

trafficNet <- function(name, path, net, nodes, edges){
  new("trafficEdges", name = name, path = path, net=net, nodes=nodes, edges = edges)  
}


setClass("vehicleTypes",
  representation(
    name = "character",
    path = "character",
    types = "data.frame"
  )
)

vehicleTypes <- function(name, path, types){
  new("vehicleTypes", name = name, path = path, types = types)  
}

setClass("routes",
  representation(
    name = "character",
    path = "character",
    routesDefinition = "data.frame"
  )
)

routes <- function(name, path, routesDefinition){
  new("routes", name = name, path = path, routesDefinition = routesDefinition)  
}

setClass("vehicles",
  representation(
    name = "character",
    path = "character",
    vehiclesDefinition = "data.frame"
  )
)

vehicles <- function(name, path, vehiclesDefinition){
  new("vehicles", name = name, path = path, vehiclesDefinition = vehiclesDefinition)  
}


setClass("trafficRoutes",
  representation(
    name = "character",
    path = "character",
    vehicleTypes = "vehicleTypes",
    routes = "routes",    
    vehicles = "vehicles"
  )
)

trafficNet <- function(name, path, vehicleTypes, routes, vehicles){
  new("trafficRoutes", name = name, path = path, vehicleTypes=vehicleTypes, routes=routes, vehicles = vehicles)  
}

setClass("trafficModels",
  representation(
    name = "character",
    path = "character",
    net = "trafficNet",
    routes = "trafficRoutes"   
  )
)

trafficModels <- function(name, path, net, routes){
  new("trafficModels", name = name, path = path, net=net, routes=routes)  
}