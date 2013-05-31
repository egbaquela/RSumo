############ Read of Sumo XML files #################

xmlNodesAttrToDataFrame <- function(xmlNode){
  myXmlAttr <- xmlApply(xmlNode, xmlAttrs)
  attrDataFrame <- t(as.data.frame(myXmlAttr))
  attrDataFrame <- as.data.frame(attrDataFrame)    
}

readSumoXML <- function(path){
  myXml <- newXMLDoc()
  myXml <-xmlRoot(xmlParse(path))
  xmlNodesAttrToDataFrame(myXml) 
}

