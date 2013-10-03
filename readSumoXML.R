############## Read of Sumo XML files #################
require("XML")

readXml <- function(path, asText=FALSE){
  myXml <- newXMLDoc()
  myXml <-xmlRoot(xmlParse(path, asText))
  myXml
}

xmlNodesAttrToDataFrame <- function(xmlNode){
  myXmlAttr <- xmlApply(xmlNode, xmlAttrs)
  attrDataFrame <- t(as.data.frame(myXmlAttr))
  attrDataFrame <- as.data.frame(attrDataFrame)   
  # Arreglar problema cuando la cantidad de atributos 
  # sea diferente entre nodos.
  attrDataFrame
}

readSumoXML <- function(path, readValue=FALSE, asText=FALSE){
  myXml <- readXml(path, asText)
  attrDataFrame <- xmlNodesAttrToDataFrame(myXml)
  if (!readValue){
    attrDataFrame
  }
  else{
    #Cambiar a devolución de valores
    attrDataFrame
  }
  
}

readXMLNodesAsDataFrame <- function(xmlNodes, xPath){
  xmlNodes<-xmlApply(getNodeSet(xmlNodes,xPath), xmlAttrs)
  myDataFrame <- as.data.frame(t(as.data.frame(xmlNodes)))  
  myDataFrame
}


readXMLNodesFromFileAsDataFrame <- function(path, xPath){
  myXml <- readXml(path)
  myDataFrame <- readXMLNodesAsDataFrame(myXml, xPath)  
  myDataFrame
}
