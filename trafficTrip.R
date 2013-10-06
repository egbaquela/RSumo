setClass("trafficTrip",
         representation(
           id = "character",
           #vehicleTypes = "data.frame",
           trips = "data.frame"
         ),
         prototype = list(id=character(),
                          #vehicleTypes = data.frame(),
                          trips = data.frame()
         )
)

trafficTrip <- function(id){
  object <- new("trafficTrip", id = id) 
  
  #vehicleTypes <- data.frame(c("-"),c(0.0),c(0.0), c(0.0), 
  #                           c(0.0), c(0.0)) 
  #names(vehicleTypes) <- c("id", "accel", "decel", "length", 
  #                         "maxSpeed", "sigma")
  #object@vehicleTypes <- vehicleTypes 
  
  trips <- data.frame(c("-"),c(0.0), c("-"), c("-")) 
  names(trips) <- c("id", "depart", "from", "to")
  object@trips <- trips
  
  object  
}

setGeneric("appendTrip", function(object, id, depart, from, to){})
setMethod("appendTrip", "trafficTrip", 
          function(object, id, depart, from, to){
            # Asociarlo a una trafficNet
            id<-as.character(id)
            from<-as.character(from)
            to<-as.character(to)
            trip <- data.frame(id, depart, from, to)
            object@trips <- rbind(object@trips,trip)
            object  
          }
)

setGeneric("removeTrip", function(object, tripIndex=NA, idTrip=NA){})
setMethod("removeTrip", "trafficTrip",
          function(object, tripIndex=NA, idTrip=NA){
            if(!is.na(tripIndex)){
              object@trips <- object@trips[-tripIndex,] 
            }else{
              object@trips <- object@trips[!(object@trips$id==idTrip)]
            }
            object
          }
)

setGeneric("writeTrafficTripToXML", function(object, path){})
setMethod("writeTrafficTripToXML", "trafficTrip", 
          function(object, path){
            parentXMLNode <- xmlNode("trips")
            for (i in 1:nrow(object@trips)){
              # Nota: para escribir los "from" y "to" a XML es necesario
              # hacer un cast a character, ya que R los considera del tipo
              # factor.
              childNode <- xmlNode("trip")
              childNode <- addAttributes(childNode, 
                                         id = object@trips$id[i],
                                         depart = object@trips$depart[i],
                                         from = as.character(object@trips$from[i]),
                                         to = as.character(object@trips$to[i]))
              parentXMLNode <- addChildren(parentXMLNode, 
                                           childNode) 
            }
            
            saveXML(parentXMLNode, path)
          }
)

setGeneric("generateRandomTrip", function(object, trafficNet, number,
                                          depart){})
setMethod("generateRandomTrip", "trafficTrip", 
          function(object, trafficNet, number, depart){
            #Elimino todos los trips actuales.
            object<-removeTrip(object, 1:nrow(object@trip))
            #Inicio la generación de los trips aleatorios.
            namesOfEdges <-idEdges(trafficNet)
            origin <- sample(namesOfEdges, number, replace=TRUE)
            destination <- sample(namesOfEdges, number, replace=TRUE)
            object <- appendTrip(object, 1:number, depart, origin, 
                                 destination)
            object
          }
)