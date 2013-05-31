#requiere("trafficNode.R")
#requiere("trafficLane.R")
#requiere("trafficEdge.R")

setClass("trafficNet",
         representation(
           name = "character",
           path = "character",
           net = "data.frame",
           nodes = "trafficNodes",
           edges = "trafficEdges"
         )
)

trafficNet <- function(name, path, net, nodes, edges){
  new("trafficEdges", name = name, path = path, net=net, nodes=nodes, edges = edges)  
}