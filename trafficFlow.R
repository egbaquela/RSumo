setClass("trafficFlow",
         representation(
           id = "character",
           #vehicleTypes = "data.frame",
           flows = "data.frame"
         ),
         prototype = list(id=character(),
                          #vehicleTypes = data.frame(),
                          flows = data.frame()
         )
)

trafficFlow <- function(id){
  object <- new("trafficFlow", id = id) 
  
  #vehicleTypes <- data.frame(c("-"),c(0.0),c(0.0), c(0.0), 
  #                           c(0.0), c(0.0)) 
  #names(vehicleTypes) <- c("id", "accel", "decel", "length", 
  #                         "maxSpeed", "sigma")
  #object@vehicleTypes <- vehicleTypes 
  
  flows <- data.frame(c("-"),c("-"), c("-"), c(0), c(0), c(0)) 
  names(flows) <- c("id", "from", "to", "begin", "end", "number")
  object@flows <- flows
  
  object  
}

setGeneric("appendFlow", function(object, id, from, to, 
                                  begin, end, number){})
setMethod("appendFlow", "trafficFlow", 
          function(object, id, from, to, begin, end, number){
            # Asociarlo a una trafficNet
            id<-as.character(id)
            from<-as.character(from)
            to<-as.character(to)
            flow <- data.frame(id, from,to, begin, end, number)
            object@flows <- rbind(object@flows,flow)
            object  
          }
)

setGeneric("removeFlow", function(object, flowIndex=NA, idFlow=NA){})
setMethod("removeFlow", "trafficFlow",
          function(object, flowIndex=NA, idFlow=NA){
            if(!is.na(flowIndex)){
              object@flows <- object@flows[-flowIndex] 
            }else{
              object@flows <- object@flows[!(object@flows$id==idFlow)]
            }
          }
)

setGeneric("writeTrafficFlowToXML", function(object, path){})
setMethod("writeTrafficFlowToXML", "trafficFlow", 
          function(object, path){
            parentXMLNode <- xmlNode("flows")
            #for (i in 1:nrow(object@flows)){
            #  childNode <- c(childNode, list(xmlNode("flow")))
            #  childNode <- addAttributes(childNode, 
            #                             id = object@flows$id[i],
            #                             from = object@flows$from[i],
            #                            to = object@flows$to[i],
            #                             begin = object@flows$begin[i],
            #                            end = object@flows$end[i],
            #                             number = object@flows$number[i])
            #  parentXMLNode <- addChildren(parentXMLNode, 
            #                               childNode) 
            #}
            childrenNodes <- rep(list(xmlNode("flow")), times=nrow(object@flows))
            childrenNodes <- mapply(addAttributes, childrenNodes, 
                                 id = object@flows$id, from = object@flows$from,
                                 to = object@flows$to[i],begin = object@flows$begin,
                                 end = object@flows$end[i],number = object@flows$number)
            parentXMLNode <- addChildren(node=parentXMLNode,kids=childrenNodes)
            saveXML(parentXMLNode, path)
          }
)

setGeneric("generateRandomFlow", function(object, trafficNet, number,
                                          begin, end){})
setMethod("generateRandomFlow", "trafficFlow", 
          function(object, trafficNet, number, begin, end){
            #Elimino todos los flows actuales.
            removeFlow(object, 1:length(object@flows))
            #Inicio la generación de los flows aleatorios.
            nodes <-numNodes(trafficNet)
            origin <- sample(nodes, number, replace=TRUE)
            destination <- sample(nodes, number, replace=TRUE)
            flows <- data.frame(origin, destination)

            # Tengo muchos flows identicos con un solo vehículo,
            # acá cuento las repeticiones y las consolido
            flows <- as.data.frame(table(flows))
            flows <- flows[which(flows$Freq>0),]
            names(flows)<-c("origin", "destination", "number")
            object <- appendFlow(object, 1:nrow(flows), flows$origin, 
                                 flows$destination, begin, end, 
                                 flows$number)
            object
          }
)
