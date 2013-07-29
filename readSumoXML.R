############ Read of Sumo XML files #################
require("XML")

readXml <- function(path){
  myXml <- newXMLDoc()
  myXml <-xmlRoot(xmlParse(path))
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

readSumoXML <- function(path, readValue=FALSE){
  myXml <- readXml(path)
  attrDataFrame <- xmlNodesAttrToDataFrame(myXml)
  if (!readValue){
    attrDataFrame
  }
  else{
    #Cambiar a devolución de valores
    attrDataFrame
  }
  
}

readXMLNodesAsDataFrame <- function(path, xPath){
  myXml <- readXml(path)
  myXml<-xmlApply(getNodeSet(myXml,xPath), xmlAttrs)
  myDataFrame <- as.data.frame(t(as.data.frame(myXml)))  
  myDataFrame
}
