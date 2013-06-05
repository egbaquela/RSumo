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
          function(object, path, append=FALSE){
            nodes <- readSumoXML(path) 
            nodes$id <- as.character(nodes$id)
            # Check for type existence
            if (sum(names(nodes)=="type")==1){
              nodes$type <- as.character(nodes$type)
            }else{             
              nodes$type <- rep("normal", times=nrow(nodes))
              print("Warning: Some types are missing. This nodes was setting
                    to type=normal")
              # Armar una llamada a warnings global
            }
            
            nodes$x <- as.numeric(as.character(nodes$x))
            nodes$y <- as.numeric(as.character(nodes$y))
            if (append==FALSE){
              object@nodes <- nodes             
            }
            else{
              object@nodes <- rbind(object@nodes, nodes)
            }
            rownames(object@nodes) <- seq(1:nrow(object@nodes))
            object
          }
)

setGeneric("addEdgeTypesFromFile", function(object, path, append=FALSE){})
setMethod("addEdgeTypesFromFile", "trafficNet", 
          function(object, path){
            edgeTypes <- readSumoXML(path) 
            edgeTypes$id <- as.character(edgeTypes$id)
            edgeTypes$priority <- as.numeric(as.character(edgeTypes$priority))
            edgeTypes$numLanes <- as.numeric(as.character(edgeTypes$numLanes))
            edgeTypes$speed <- as.numeric(as.character(edgeTypes$speed))
            if (append){
              object@edgeTypes <- edgeTypes             
            }
            else{
              object@edgeTypes <- rbind(object@edgeTypes, edgeTypes)
            }
            object
          }
)

setGeneric("addConnectionsFromFile", function(object, path, append=FALSE){})
setMethod("addConnectionsFromFile", "trafficNet", 
          function(object, path){
            connections <- readSumoXML(path) 
            connections$id <- as.character(connections$id)            
            connections$fromEdge <- as.character(connections$fromEdge)
            connections$toEdge <- as.character(connections$toEdge)
            connections$fromLane <- as.character(connections$fromLane)
            connections$toLane <- as.character(connections$toLane)          
            if (append){
              object@connections <- connections             
            }
            else{
              object@connections <- rbind(object@connections, connections)
            }
            object
          }
)