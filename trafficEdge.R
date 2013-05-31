#requiere("trafficLane.R")

setClass("trafficEdge",
         representation(
           id = "character",
           from = "character",
           to = "character"
           priority = "integer"
           edgeFunction = "character" # Cambiar a factor
           length = "double"
           lanes = "lanes"
         )
)

trafficEdge <- function(id, from, to, priority=1,edgeFunction="normal", 
                        length=NA, lanes=NA){
  new("trafficEdge", id=id, from=from, to=to, priority=priority,
      edgeFunction=edgeFunction, length=length, lanes=lanes)  
}