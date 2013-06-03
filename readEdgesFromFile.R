#requiere("trafficNodeList.R")

readNodesFromFile <- function(path, id){
  
  # Hacer un if para ver si llamar a la función de lectura
  # con types o sin type.
}

readNodesFromFileWithTypeFile <- function(pathEdgeFile, pathEdgeType, id){
  
  edges <- readSumoXML(path) 
  edges$id <- as.character(edges$id)
  edges$from <- as.character(edges$from)
  edges$to <- as.character(edges$to)
  edges$type <- as.character(edges$type)
  edgeList = c()
  for (i in 1:length(nodes)){
    nodeList = c(nodeList, trafficNode(id=nodes$id[i], x=nodes$x[i], y=nodes$y[i]))
  }
  nodeList <- trafficNodeList(id=id, nodes=nodeList)
  nodeList
}