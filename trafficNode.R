setClass("trafficNode",
         representation(
           id = "character",
           x = "double",
           y = "double"
         )
)

trafficNode <- function(id, x, y){
  new("trafficNodes", id = id, x = x, y = y)  
}