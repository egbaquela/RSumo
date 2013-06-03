setClass("trafficConnection",
         representation(
           id = "character",
           fromEdge = "character",
           toEdge = "character",
           fromLane = "character",
           toLane = "character"
           #tls
           #tlLink
         ),
         prototype = list(id=character(),
                          fromEdge = character(),
                          toEdge = character(),
                          fromLane = character(),
                          toLane = character()
         )
)

trafficConnection <- function(id, fromEdge, toEdge, fromLane, toLane){
  new("trafficConnection", id = id, fromEdge=fromEdge, toEdge=toEdge, 
      fromLane=fromLane, toLane=toLane)  
}
