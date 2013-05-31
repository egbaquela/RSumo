#requiere("trafficNodeList.R")

readNodesFromFile <- function(path, id){
  
  nodes <- readSumoXML(path) 
  nodes$id <- as.character(nodes$id)
  nodes$x <- as.numeric(as.character(nodes$x))
  nodes$y <- as.numeric(as.character(nodes$y))
  nodeList = c()
  for (i in 1:length(nodes)){
    nodeList = c(nodeList, trafficNode(id=nodes$id[i], x=nodes$x[i], y=nodes$y[i]))
  }
  nodeList <- trafficNodeList(id=id, nodes=nodeList)
  nodeList
}