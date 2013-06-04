setClass("trafficNet",
         representation(
           id = "character",
           nodes = "data.frame",
           edges = "data.frame",
           lanes = "data.frame",
           edgeTypes = "data.frame",
           connections = "data.frame"
         ),
         prototype = list(id=character(),
                          nodes = data.frame(),
                          edges = data.frame(),
                          lanes = data.frame(),
                          edgeTypes = data.frame(),
                          connections = data.frame()
         )
)

trafficNet <- function(id){
  object <- new("trafficNet", id = id) 
  
  nodes <- data.frame(c("-"),c("normal"),c(0.0), c(0.0)) 
  names(nodes) <- c("id", "type", "x", "y")
  object@nodes <- nodes

  lanes <- data.frame(c("-"),c(0),c(0.0), c(0.0), c("-")) 
  names(lanes) <- c("id", "index", "speed", "length", "shape")
  object@lanes <- lanes  
  
  edgeTypes <- data.frame(c("-"),c(0),c(0), c(0.0)) 
  names(edgeTypes) <- c("id", "priority", "numLanes", "speed")
  object@edgeTypes <- edgeTypes  
  
  connections <- data.frame(c("-"),c("-"),c("-"), c("-"),c("-")) 
  names(connections) <- c("id", "fromEdge", "toEdge", "fromLane", "toLane")
  object@connections <- connections
  
  object
}

id = "character",
index = "numeric",
speed = "numeric",
length = "numeric",
shape = "character",