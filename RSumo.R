###################################################
# Modulo para conectividad con SUMO
#
#
###################################################
 
# Sumo Manager
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

# Class for models

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