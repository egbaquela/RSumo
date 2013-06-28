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
  names(vehicles) <- c("id", "type", "route0", "depart")
  object@vehicles <- vehicles  
  
  routes <- data.frame(c("-"),c("-")) 
  names(routes) <- c("id", "edges")
  object@routes <- routes
  
  object
  
}