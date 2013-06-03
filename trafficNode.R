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
           # El procedimiento getLinkIndex se puede utilizar filtrando incoming
           # Ver que hace el método forbids
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

trafficNode <- function(id, type="node", x, y){
  new("trafficNode", id = id, type = type, x = x, y = y)  
}
