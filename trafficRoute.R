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
            object@vehicleTypes <- rbind(object@vehicleTypes,
                                        vehicleType)
            object  
          }
)

setGeneric("appendRoute", function(object, id, edges){})
setMethod("appendRoute", "trafficRoute", 
          function(object, id, edges){
            # Asociarlo a una trafficNet
            route <- data.frame(id, edges)
            object@routes <- rbind(object@routes,route)
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
            object@vehicles <- rbind(object@vehicles,vehicle)
            object  
          }
)

setGeneric("writeTrafficRouteToXML", function(object, path){})
setMethod("writeTrafficRouteToXML", "trafficRoute", 
          function(object, path){
            parentXMLRoute <- xmlNode("routes")
            for (i in 1:nrow(object@vehicleTypes)){
              childNode <- xmlNode("vType")
              childNode <- addAttributes(childNode, 
                                         id = object@vehicleTypes$id[i],
                                         accel = object@vehicleTypes$accel[i],
                                         decel = object@vehicleTypes$decel[i],
                                         length = object@vehicleTypes$length[i],
                                         maxSpeed = object@vehicleTypes$maxSpeed[i],
                                         sigma = object@vehicleTypes$sigma[i])
              parentXMLNode <- addChildren(parentXMLNode, 
                                           childNode) 
            }
            for (i in 1:nrow(object@routes)){
              childNode <- xmlNode("route")
              childNode <- addAttributes(childNode, 
                                         id = object@routes$id[i],
                                         edges = object@routes$edges[i])
              parentXMLNode <- addChildren(parentXMLNode, 
                                           childNode) 
            }  
            for (i in 1:nrow(object@vehicles)){
              childNode <- xmlNode("vehicle")
              childNode <- addAttributes(childNode, 
                                         id = object@vehicles$id[i],
                                         route = object@vehicles$route[i],  
                                         type = object@vehicles$type[i],                                         
                                         depart = object@vehicles$depart[i])
              parentXMLNode <- addChildren(parentXMLNode, 
                                           childNode) 
            }              
            saveXML(parentXMLNode, path)
          }
)