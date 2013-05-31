setClass("trafficLane",
         representation(
           id = "character",
           index = "integer",
           speed = "double"
           length = "double"
           # ver como modelar el atributo shape
         )
)

trafficLane <- function(id, index, speed, length=NA){
  new("trafficLane", id=id, index=index, speed=speed, length=length)  
}