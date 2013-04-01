###################################################
# Modulo para conectividad con SUMO
#
#
###################################################
 
setClass("adminRSumo",
  representation(
    sumoBinPath = "character" # Path to sumo.exe and others
    name ="character" # Instance name           
  )
)

# Constructor
adminRSumo <- function(sumoBinPath, name){
  new("adminRSumo", sumoBinPath = sumoBinPath, name = name)
}