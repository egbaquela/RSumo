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
                      c("-"),c(0.0), c(0.0), c(1)) 
  names(edges) <- c("id", "from", "to", "type", "priority", 
                    "edgeFunction", "speed", "length", "lanes")
  object@edges <- edges  
  
  object
}

setGeneric("appendNode", function(object, id, type, x,y){})
setMethod("appendNode", "trafficNet", 
          function(object, id, type, x,y){
            # node <- data.frame(id, type, x, y)
            # object@nodes <- rbind(object@nodes, node)
            object@nodes <- rbind(object@nodes, cbind(id, type, x, y))
            object  
          }
)

setGeneric("addNodesFromFile", function(object, path, append=FALSE, asText=FALSE){})
setMethod("addNodesFromFile", "trafficNet", 
          function(object, path, append=FALSE, asText=FALSE){
            nodes <- readSumoXML(path, asText) 
            nodes$id <- as.character(nodes$id)
            # Check for type existence
            if (sum(names(nodes)=="type")==1){
              nodes$type <- as.character(nodes$type)
            }else{             
              nodes$type <- rep("normal", times=nrow(nodes))
              cat("Warning: Some types are missing. This nodes was setting to type=normal")
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
          function(object, path, append=FALSE){
            edgeTypes <- readSumoXML(path) 
            edgeTypes$id <- as.character(edgeTypes$id)
            edgeTypes$priority <- as.numeric(as.character(edgeTypes$priority))
            edgeTypes$numLanes <- as.numeric(as.character(edgeTypes$numLanes))
            edgeTypes$speed <- as.numeric(as.character(edgeTypes$speed))
            if (append==FALSE){
              object@edgeTypes <- edgeTypes             
            }
            else{
              object@edgeTypes <- rbind(object@edgeTypes, edgeTypes)
            }
            rownames(object@edgeTypes) <- seq(1:nrow(object@edgeTypes))
            object
          }
)

setGeneric("addConnectionsFromFile", function(object, path, append=FALSE){})
setMethod("addConnectionsFromFile", "trafficNet", 
          function(object, path, append=FALSE){
            connections <- readSumoXML(path) 
            connections$id <- as.character(connections$id)            
            connections$fromEdge <- as.character(connections$fromEdge)
            connections$toEdge <- as.character(connections$toEdge)
            connections$fromLane <- as.character(connections$fromLane)
            connections$toLane <- as.character(connections$toLane)          
            if (append==FALSE){
              object@connections <- connections             
            }
            else{
              object@connections <- rbind(object@connections, connections)
            }
            rownames(object@connections) <- seq(1:nrow(object@connections))
            object
          }
)

setGeneric("addEdgesFromFile", function(object, path, append=FALSE, asText=False){})
setMethod("addEdgesFromFile", "trafficNet", 
          function(object, path, append=FALSE, asText=FALSE){
            edges <- readSumoXML(path, asText)
            edges$id <- as.character(edges$id)
            edges$from <- as.character(edges$from)
            edges$to <- as.character(edges$to)
            if (sum(names(edges)=="type")==1){
              edges$type <- as.character(edges$type) 
            }
            else{
              edges$type <- rep("none", times=nrow(edges))
            }

            if (sum(names(edges)=="priority")==1){
              edges$priority <- as.numeric(as.character(edges$priority))
            }
            else{
              edges$priority <- rep(1, times=nrow(edges))
            }            

            if (sum(names(edges)=="function")==1){
              # Change the name because any column can not has "function" as name.
              posFunction <- order((names(edges)=="function"),decreasing=TRUE)[1]
              names(edges)[posFunction] <- "edgeFunction"
              edges$edgeFunction <- as.character(edges$edgeFunction)
            }
            else{
              edges$edgeFunction <- rep("normal", times=nrow(edges))
            }            

            if (sum(names(edges)=="speed")==1){
              edges$speed <- as.numeric(as.character(edges$speed))
            }
            else{
              edges$speed <- rep(1.0, times=nrow(edges))
            }            

            if (sum(names(edges)=="lanes")==1){
              edges$lanes <- as.numeric(as.character(edges$lanes))
            }
            else{
              edges$lanes <- rep(1, times=nrow(edges))
            }            
            
            # edges$length <- as.numeric(as.character(edges$length))            
            if (append==FALSE){
              object@edges <- edges             
            }
            else{
              object@edges <- rbind(object@edges, edges)
            }
            rownames(object@edges) <- seq(1:nrow(object@edges))
            
            # Load the lanes XML subnodes
            lanes <- readXMLNodesFromFileAsDataFrame(path, "lane")
            if (sum(names(lanes)=="index")==1){
              lanes$index <- as.numeric(as.character(lanes$index))
            }
            else{
              lanes$index <- rep(1, times=nrow(lanes))
            }
            if (sum(names(lanes)=="speed")==1){
              lanes$speed <- as.numeric(as.character(lanes$speed))
            }
            else{
              lanes$speed <- rep(1, times=nrow(lanes))
            }
            if (sum(names(lanes)=="length")==1){
              lanes$length <- as.numeric(as.character(lanes$length))
            }
            else{
              lanes$length <- rep(1, times=nrow(lanes))
            }
            object@lanes <- lanes
            
            object
          }
)

setGeneric("writeNodesToXML", function(object, path){})
setMethod("writeNodesToXML", "trafficNet", 
          function(object, path){
            parentXMLNode <- xmlNode("nodes")
            for (i in 1:nrow(object@nodes)){
              childNode <- xmlNode("node")
              childNode <- addAttributes(childNode, 
                                         id = object@nodes$id[i],
                                         x = object@nodes$x[i],
                                         y = object@nodes$y[i],
                                         type = object@nodes$type[i])
              parentXMLNode <- addChildren(parentXMLNode, 
                                           childNode) 
            }
            saveXML(parentXMLNode, path)
          }
)

setGeneric("writeEdgeTypesToXML", function(object, path){})
setMethod("writeEdgeTypesToXML", "trafficNet", 
          function(object, path){
            parentXMLNode <- xmlNode("edgesTypes")
            for (i in 1:nrow(object@edgeTypes)){
              childNode <- xmlNode("edgeType")
              childNode <- addAttributes(childNode, 
                                         id = object@edgeTypes$id[i],
                                         priority = object@edgeTypes$priority[i],
                                         numLanes = object@edgeTypes$numLanes[i],
                                         speed = object@edgeTypes$speed[i])
              parentXMLNode <- addChildren(parentXMLNode, 
                                           childNode) 
            }
            saveXML(parentXMLNode, path)
          }
)

setGeneric("writeConnectionsToXML", function(object, path){})
setMethod("writeConnectionsToXML", "trafficNet", 
          function(object, path){
            parentXMLNode <- xmlNode("connections")
            for (i in 1:nrow(object@connections)){
              childNode <- xmlNode("connection")
              childNode <- addAttributes(childNode, 
                                         id = object@connections$id[i],
                                         fromEdge = object@connections$fromEdge[i],
                                         toEdge = object@connections$toEdge[i],
                                         fromLane = object@connections$fromLane[i],
                                         toLane = object@connections$toLane[i])
              parentXMLNode <- addChildren(parentXMLNode, 
                                           childNode) 
            }
            saveXML(parentXMLNode, path)
          }
)

setGeneric("writeEdgesToXML", function(object, path){})
setMethod("writeEdgesToXML", "trafficNet", 
          function(object, path){
            parentXMLNode <- xmlNode("edges")
            for (i in 1:nrow(object@edges)){
              childNode <- xmlNode("edge")
              childNode <- addAttributes(childNode, 
                                         id = object@edges$id[i],
                                         from = object@edges$from[i],
                                         to = object@edges$to[i]
                                         #type = object@edges$type[i],
                                         #priority = object@edges$priority[i],
                                         #edgeFunction = object@edges$edgeFunction[i],
                                         #Implementar el cambio de edgeFunction a function
                                         #speed = object@edges$speed[i],
                                         #length = object@edges$length[i],
                                         #lanes = object@edges$lanes[i]
                                         )
              parentXMLNode <- addChildren(parentXMLNode, 
                                           childNode) 
            }
            saveXML(parentXMLNode, path)
          }
)

setGeneric("numNodes", function(object){})
setMethod("numNodes", "trafficNet", 
          function(object){
            nrow(object@nodes)
          }
)

setGeneric("numEdges", function(object){})
setMethod("numEdges", "trafficNet", 
          function(object){
            nrow(object@edges)
          }
)

readTrafficNetFromFile <- function(path){
  myXml <- readXml(path)
  myTrafficNet <- trafficNet("-")  
  # Leo solamente los edges que son del tipo "normal".
  # SUMO declara edges internal en el atributo "function".
  # El valor por default es "normal", y cuando el edge
  # es "normal", "function" no se declara. Si el
  # edge es de otro tipo ("internal" por ejemplo),
  # se agrega este atributo al nodo. Por lo tanto,
  # si filtro los edges que no tienen el atributo
  # "function" declarado, estoy filtrando los edges
  # con "function = normal".
  xmlEdgesDoc <- newXMLDoc()
  xmlEdges <- newXMLNode("edges")  
  xmlEdges <- addChildren(xmlEdges,
                          getNodeSet(myXml, "edge[not(@function)]"))
  xmlEdgesDoc <-addChildren(xmlEdgesDoc, xmlEdges)
  myTrafficNet <- addEdgesFromFile(myTrafficNet,
                                   saveXML(xmlEdgesDoc),
                                  asText=TRUE)
  
  # Idem como con los edges, leo los nodos, que
  # en los archivos net.xml se denominan "junctions":
  xmlNodesDoc <- newXMLDoc()
  xmlNodes <- newXMLNode("nodes")  
  xmlNodes <- addChildren(xmlNodes,
                          getNodeSet(myXml, "junction[@type!=\"internal\"]"))
  xmlNodesDoc <-addChildren(xmlNodesDoc, xmlNodes)
  myTrafficNet <- addNodesFromFile(myTrafficNet,
                                   saveXML(xmlNodesDoc),
                                   asText=TRUE)  
  myTrafficNet
}
