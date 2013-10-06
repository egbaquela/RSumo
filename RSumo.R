###################################################
# 
# Modulo para conectividad con SUMO
#
###################################################
 
require(stats)
#requiere(trafficNet.R)

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

setGeneric("runSimulationFromParams", function(object, params, activeGUI=FALSE){})

setMethod("runSimulationFromParams", "adminRSumo",
          function(object, params, activeGUI=FALSE){
            if (activeGUI){
              sumo<-"sumo-gui.exe"
            }
            else{
              sumo<-"sumo.exe"
            }
            shell(paste(object@sumoBinPath, sumo," ",params,sep=""))    
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

setGeneric("runSimulation", function(object, trafficNet, trafficRoute, endTime, activeGUI=FALSE,
                                              pathOutputs=NA, reportTripInfo = FALSE, reportVehRoute = FALSE,
                                              reportSummary = FALSE){})

setMethod("runSimulation", "adminRSumo",
          function(object, trafficNet, trafficRoute, endTime, activeGUI=FALSE, 
                   pathOutputs=NA, reportTripInfo = FALSE, reportVehRoute = FALSE,
                   reportSummary = FALSE){
            
            pathNet <- generateNet(object, trafficNet, paste(getwd(), "/", sep=""), 
                                   paste(trafficNet@id, ".net.xml", sep=""))
            pathRoute <- writeTrafficRouteToXML(trafficRoute, paste(getwd(), "/", trafficRoute@id, ".rou.xml", sep=""))
            runSimulationFromFiles(object, pathNet, pathRoute, endTime, activeGUI, 
                                   pathOutputs, reportTripInfo, reportVehRoute,
                                   reportSummary)
          }
)




setGeneric("generateNetFromCFG", function(object, pathCFG){})

#Cambiar netgen por netgenerate
setMethod("generateNetFromCFG", "adminRSumo", 
          function(object, pathCFG){
            shell(paste(object@sumoBinPath, "netgen.exe"," -c=\"", pathCFG,"\"", sep=""))  
          } 
)

setGeneric("generateNetFromFiles", function(object, pathNodes, pathEdges, pathOutput){})

setMethod("generateNetFromFiles", "adminRSumo", 
          function(object, pathNodes, pathEdges, pathOutput){
            shell(paste(object@sumoBinPath, "netconvert.exe",
                        " --node-files=\"", pathNodes,"\"",
                        " --edge-files=\"", pathEdges,"\"",
                        " --output-file=\"", pathOutput,"\"",
                        sep=""))  
          } 
)

setGeneric("generateNet", function(object, trafficNet, 
                                   pathOutputFolder, outputFileName){})

setMethod("generateNet", "adminRSumo", 
          function(object, trafficNet, pathOutputFolder, 
                   outputFileName){
            pathNodes <- paste(pathOutputFolder, trafficNet@id,
                               ".nod.xml", sep="")
            writeNodesToXML(trafficNet, pathNodes)
            pathEdges <- paste(pathOutputFolder, trafficNet@id,
                               ".edg.xml", sep="")
            writeEdgesToXML(trafficNet, pathEdges)            

            command <- paste(object@sumoBinPath, "netconvert.exe",
                             " --node-files=\"", pathNodes,"\"",
                             " --edge-files=\"", pathEdges,"\"",
                             sep="")
            if (!is.na(trafficNet@connections)) {
              pathConnections<- paste(pathOutputFolder, trafficNet@id,
                                      ".con.xml", sep="")
              writeConnectionsToXML(trafficNet, pathConnections)
              command <- paste(command, " --connection-files=\"", 
                               pathConnections,"\"", sep="")              
            }
            if (!is.na(trafficNet@edgeTypes)) {
              pathEdgeTypess<- paste(pathOutputFolder, trafficNet@id,
                                      ".typ.xml", sep="")
              writeEdgeTypesToXML(trafficNet, pathEdgeTypes)
              command <- paste(command, " --type-files=\"", 
                               pathEdgeTypes,"\"", sep="")              
            }           
            
            shell(paste(command, " --output-file=\"",
                        pathOutputFolder, outputFileName,
                        "\"", sep=""))  
          } 
)


setGeneric("generateRandomNet", function(object, iterations, pathOutputDir, name){})

setMethod("generateRandomNet", "adminRSumo", 
          function(object,  iterations, pathOutputDir, name=NULL){
            if (length(name)!=length(iterations)){
              name <- "net_"  
            }  
            name <- paste(name, 
                          as.character(1:max(length(iterations), length(name))),
                          ".net.xml",
                          sep="") 
            # Genero las redes, construyendo una función lambda que encapsula la llamada a
            # netgenerate. La vectorización se realiza a partir de mapply
            mapply(function(x,y,z){
              shell(paste(object@sumoBinPath, "netgenerate.exe"," --random-net --rand-iterations=", x, " -o=\"",y, z, "\"", sep=""))   
            }, iterations, pathOutputDir, name )            
            name
          }             
)

setGeneric("generateGridNet", function(object, gridNumber, gridLength, pathOutputDir, name){})

setMethod("generateGridNet", "adminRSumo", 
          function(object,  gridNumber, gridLength, pathOutputDir, name=NULL){
            if (length(name)!=max(length(gridNumber), length(gridLength))){
              name <- "net_"  
            }  
            name <- paste(name, 
                          as.character(1:max(length(gridNumber), length(gridLength), length(name))),
                          ".net.xml",
                          sep="") 
            # Genero las redes, construyendo una función lambda que encapsula la llamada a
            # netgenerate. La vectorización se realiza a partir de mapply
            mapply(function(w,x,y,z){
              shell(paste(object@sumoBinPath, "netgenerate.exe"," --grid-net --grid-number=", w, " --grid-length=", x,  " -o=\"",y, z, "\"", sep=""))   
            }, gridNumber, gridLength, pathOutputDir, name )
            name
          }             
)



setGeneric("generateRandomTrips", 
           function(object, pathNet, pathOutput, generateRoute = TRUE, 
                    tripPrefix = "t", tripParams=NA, beginTime=0, endTime=3600, 
                    period=1,seed=NA, lengthWeigth=NA, lanesWeigth=NA,
                    speedWeigth=NA, fringeFactor=1, fringeThreshold=NA,
                    minDistance=0){})

setMethod("generateRandomTrips", "adminRSumo", 
          function(object, pathNet, pathOutput, generateRoute = TRUE, 
                   tripPrefix = "t", tripParams=NA, beginTime=0, endTime=3600, 
                   period=1,seed=NA, lengthWeigth=NA, lanesWeigth=NA,
                   speedWeigth=NA, fringeFactor=1, fringeThreshold=NA,
                   minDistance=0){
            # Versión anterior, llamaba a DUAROUTER, pero genera errores
            # shell(paste(object@sumoBinPath, "duarouter.exe"," -n=\"", pathNet,"\""," -o=\"", pathOutput,"\""," -b=", beginTime," -e=", endTime, " -R=0.1", sep="")) 
            if (!is.na(seed)){set.seed(seed)} 
            # Pendiente: DESARROLLAR LECTURA DE ARCHIVOS .net para asignar trips
          } 
)

setGeneric("generateRoutesFromFlow", 
           function(object, pathNet, pathFlow, pathOutput){})
setMethod("generateRoutesFromFlow", "adminRSumo", 
          function(object, pathNet, pathFlow, pathOutput){
            # TODO: Unificar interface con los genradores de rutas, 
            #que me devuelva el nombre.
            mapply(function(x,y,z){
              shell(paste(object@sumoBinPath, "duarouter.exe"," -n=\"", x,"\"", " -f=\"", y,"\""," -o=\"", z,"\"", sep=""))}, 
              pathNet, pathFlow, pathOutput)
            pathOutput
          } 
)