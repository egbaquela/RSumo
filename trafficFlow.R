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

setGeneric("writeTrafficFlowToXML", function(object, path){})
setMethod("writeTrafficFlowToXML", "trafficFlow", 
          function(object, path){
            parentXMLNode <- xmlNode("flows")
            for (i in 1:nrow(object@flows)){
              childNode <- xmlNode("flow")
              childNode <- addAttributes(childNode, 
                                         id = object@flows$id[i],
                                         from = object@flows$from[i],
                                         to = object@flows$to[i],
                                         begin = object@flows$begin[i],
                                         end = object@flows$end[i],
                                         number = object@flows$number[i])
              parentXMLNode <- addChildren(parentXMLNode, 
                                           childNode) 
            }
                  
            saveXML(parentXMLNode, path)
          }
)

setGeneric("generateRandomFlow", function(object, trafficNet, number,
                                          begin, end){})
setMethod("generateRandomFlow", "trafficFlow", 
          function(object, trafficNet, number, begin, end){
            nodes <-numNodes(trafficNet)
            origin <- sample(nodes, number, replace=TRUE)
            destination <- sample(nodes, number, replace=TRUE)
            #Añadir chequeo por si el origen y destino coinciden
            #cuando añada ese chequeo, ojo que el segundo
            # parámetro de appendFlow deberia ser menor a 1:number
            object <- appendFlow(object, 1:number, origin, destination,
                                 begin, end, 1)
            object
          }
)
