setClass("trafficNet",
         representation(
           id = "character",
           nodes = "data.frame",
           edges = "data.frame",
           edgeTypes = "data.frame",
           connections = "data.frame"
         ),
         prototype = list(id=character(),
                          nodes = data.frame(),
                          edges = data.frame(),
                          edgeTypes = data.frame(),
                          connections = data.frame()
         )
)

trafficConnection <- function(id, fromEdge, toEdge, fromLane, toLane){
  new("trafficConnection", id = id, fromEdge=fromEdge, toEdge=toEdge, 
      fromLane=fromLane, toLane=toLane)  
}