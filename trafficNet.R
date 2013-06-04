#requiere("trafficNodeList.R")

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

  edges <- data.frame(c("-"),c("-"),c("-"), c("-"),c(1),
                      c("-"),c(0.0), c(0.0)) 
  names(edges) <- c("id", "from", "to", "type", "priority", 
                    "edgeFunction", "speed", "length")
  object@edges <- edges  
  
  object
}

setGeneric("appendNode", function(object, id, type, x,y){})

setMethod("appendNode", "trafficNet", 
          function(object, id, type, x,y){
            node <- data.frame(id, type, x, y)
            object@nodes <- rbind(object@nodes, node)
            object  
          }
)

setGeneric("addNodesFromFile", function(object, path, append=FALSE){})

setMethod("addNodesFromFile", "trafficNet", 
          function(object, path){
            nodes <- readSumoXML(path) 
            nodes$id <- as.character(nodes$id)
            nodes$type <- as.character(nodes$type)
            nodes$x <- as.numeric(as.character(nodes$x))
            nodes$y <- as.numeric(as.character(nodes$y))
            if (append){
              object@nodes <- nodes             
            }
            else{
              object@nodes <- rbind(object@nodes, nodes)
            }
            object
          }
)