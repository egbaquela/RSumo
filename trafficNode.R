setClass("trafficNode",
         representation(
           id = "character",
           type = "character",
           x = "numeric",
           y = "numeric",
           incoming = "character",
           outgoing = "character",
           foes = "character",
           prohibits = "character",
           incLanes = "character"
           # Definir bien la implementación de foes y prohibits,
           # ver node.py
         ),
         prototype = list(id=character(),
                          type=character(),
                          x=numeric(), 
                          y=numeric(),
                          incoming = character(),
                          outgoing = character(),
                          foes = character(),
                          prohibits = character(),                          
                          incLanes=character())
)

trafficNode <- function(id, type, x, y){
  new("trafficNode", id = id, type = type, x = x, y = y)  
}

setGeneric("read.csv", function(object,file, header = TRUE, 
                                sep = ",", quote="\"", dec=".",
                                fill = TRUE, comment.char="", ...){})

setMethod("read.csv", "trafficNode", 
          function(object,file, header = TRUE, 
                   sep = ",", quote="\"", dec=".",
                   fill = TRUE, comment.char="", ...){
            shell(paste(object@sumoBinPath, "sumo-gui.exe", sep=""))  
          } 
)