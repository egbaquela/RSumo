#requiere("trafficLane.R")

setClass("trafficEdge",
         representation(
           id = "character",
           from = "character",
           to = "character",
           type = "character",
           priority = "numeric",
           edgeFunction = "character", # Cambiar a factor
           speed = "numeric",
           length = "numeric",
           lanes = "vector"
         ),
         prototype = list(id=character(),
                          from = character(),
                          to = character(),
                          type = character(),
                          priority = numeric(),
                          edgeFunction = character(), # Cambiar a factor
                          speed = numeric(),
                          length = numeric(),
                          lanes = vector()
                          )
)

trafficEdge <- function(id, from, to, type=NA,priority=1,edgeFunction="normal", 
                        speed=1, length=NA, lanes=NA){
  new("trafficEdge", id=id, from=from, to=to, type=type,
      priority=priority,edgeFunction=edgeFunction, speed=speed, 
      length=length,lanes=lanes)  
}