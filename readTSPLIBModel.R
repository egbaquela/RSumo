requiere("readSumoXML.R")

############ Read TSPLIB XML Models #################

readTSPLIBModel <- function(path){
  myXml <- newXMLDoc()
  myXml <-xmlRoot(xmlParse(path))
  listaVertices <- getNodeSet(myXml, "/travellingSalesmanProblemInstance/graph/vertex")
  graph <- data.frame()
  for (i in 1:length(listaVertices)){
    conections <-  xmlToDataFrame(xmlChildren(listaVertices[[i]])) 
    costs <- xmlNodesAttrToDataFrame(listaVertices[[1]])
    auxGraph <- cbind(i-1,conections, costs)
    names(auxGraph) <- c("node","to", "cost")
    graph <- rbind(graph, auxGraph)
  }
  rownames(graph) <- NULL
  graph
}