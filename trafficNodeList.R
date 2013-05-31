#requiere("readSumoXML.R")
#requiere("trafficNode.R")

setClass("trafficNodeList",
         representation(
           id = "character",
           nodes = "vector"
         ),
         prototype = list(id=character(),
                          nodes=vector())
)

trafficNodeList <- function(id, nodes){
  new("trafficNodeList", id = id, nodes = nodes)  
}

setGeneric("addNodesFromFile", function(object,path){})

setMethod("addNodesFromFile", "trafficNodeList", 
          function(object,path){
            nodes <- readSumoXML(path) 
            nodes$x <- as.numeric(as.character(nodes$x))
            nodes$y <- as.numeric(as.character(nodes$y))
            object@nodes <- c(object@nodes,trafficNode(nodes$id,
                                       nodes$type,
                                        nodes$x,
                                        nodes$y))
          } 
)