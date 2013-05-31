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

############ Class for models #######################

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

trafficRoutes <- function(name, path, vehicleTypes, routes, vehicles){
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