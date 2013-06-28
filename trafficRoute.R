setClass("trafficRoute",
         representation(
           id = "character",
           vehicleTypes = "data.frame",
           vehicles = "data.frame",
           routes = "data.frame"
         ),
         prototype = list(id=character(),
                          vehicleTypes = data.frame(),
                          vehicles = data.frame(),
                          routes = data.frame()
         )
)

trafficRoute <- function(id){
  object <- new("trafficRoute", id = id) 
  
  vehicleTypes <- data.frame(c("-"),c(0.0),c(0.0), c(0.0), 
                             c(0.0), c(0.0)) 
  names(vehicleTypes) <- c("id", "accel", "decel", "length", 
                           "maxSpeed", "sigma")
  object@vehicleTypes <- vehicleTypes
  
  vehicles <- data.frame(c("-"),c("-"),c("-"), c(0)) 
  names(vehicles) <- c("id", "type", "route", "depart")
  object@vehicles <- vehicles  
  
  routes <- data.frame(c("-"),c("-")) 
  names(routes) <- c("id", "edges")
  object@routes <- routes
  
  object  
}

setGeneric("appendVehicleType", function(object, id, accel, 
                                         decel, length, 
                                         maxSpeed, sigma){})
setMethod("appendVehicleType", "trafficRoute", 
          function(object, id, accel, 
                   decel, length, 
                   maxSpeed, sigma){
            vehicleType <- data.frame(id, accel, 
                                      decel, length, 
                                      maxSpeed, sigma)
            object@vehicleType <- rbind(object@vehicleType,
                                        vehicleType)
            object  
          }
)

setGeneric("appendRoute", function(object, id, edges){})
setMethod("appendRoute", "trafficRoute", 
          function(object, id, edges){
            # Asociarlo a una trafficNet
            route <- data.frame(id, edges)
            object@route <- rbind(object@route,route)
            object  
          }
)

setGeneric("appendVehicle", function(object, id, type, 
                                     route, depart){})
setMethod("appendVehicle", "trafficRoute", 
          function(object, id, type, 
                   route, depart){
            # Asociarlo a las routes de este objeto
            vehicle <- data.frame(id, type, route, depart)
            object@vehicle <- rbind(object@vehicle,vehicle)
            object  
          }
)