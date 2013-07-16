############ Read of Sumo XML files #################
require("XML")

xmlNodesAttrToDataFrame <- function(xmlNode){
  myXmlAttr <- xmlApply(xmlNode, xmlAttrs)
  attrDataFrame <- t(as.data.frame(myXmlAttr))
  attrDataFrame <- as.data.frame(attrDataFrame)   
  #Arreglar problema cuando la cantidad de artributos sea diferente
  #entre nodos
}

readSumoXML <- function(path, readValue=FALSE){
  myXml <- newXMLDoc()
  myXml <-xmlRoot(xmlParse(path))
  attrDataFrame <- xmlNodesAttrToDataFrame(myXml)
  if (!readValue){
    attrDataFrame
  }
  else{
    #Cambiar a devolución de valores
    attrDataFrame
  }
  
}

