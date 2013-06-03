setClass("trafficLane",
         representation(
           id = "character",
           index = "numeric",
           speed = "numeric",
           length = "numeric",
           shape = "character",
           outgoing = "character",
           params = "character"
           # ver como modelar el atributo shape
         ),
         prototype = list(id=character(),
                          index = numeric(),
                          speed = numeric(),
                          length = numeric(),
                          shape = character(),
                          outgoing = character(),
                          params = character()
                          )
)

trafficLane <- function(id, index, speed, length=NA, 
                        shape=NA, outgoing=NA, params=NA){
  new("trafficLane", id=id, index=index, speed=speed, length=length,
      shape=shape, outgoing=outgoing, params=params)  
}