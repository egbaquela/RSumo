############################################################
#
# Modulo para gestion de experimentos
#
############################################################

library("XML")

setwd("D:/Compartido/Proyectos/doctorado/papers/Experimentos_OvS_Densidad_Poblacional/Scripts")
ruta.Sumo <- "D:\\Appls\\SUMO\\sumo-0.15.0\\bin\\sumo.exe"
ruta.Nets <- "D:\\Compartido\\Proyectos\\doctorado\\papers\\Experimentos_OvS_Densidad_Poblacional\\Nets\\"
ruta.Routes <- "D:\\Compartido\\Proyectos\\doctorado\\papers\\Experimentos_OvS_Densidad_Poblacional\\Routes\\"
ruta.Outputs <- "D:\\Compartido\\Proyectos\\doctorado\\papers\\Experimentos_OvS_Densidad_Poblacional\\Outputs\\"
ruta.ROutputs <- "D:\\Compartido\\Proyectos\\doctorado\\papers\\Experimentos_OvS_Densidad_Poblacional\\ROutputs\\"
extensiones.entradasRutas <-".rou.xml"
extensiones.salidaVehiculos <-".veh.xml"
extensiones.salidaViajes <-".otrip.xml"
extensiones.salidaSummary <-".summ.xml"

realizarExperimentos <- function(pathListaExperimentos, generarReportes=TRUE, registrarTiemposComputos=TRUE){
  experimentos.lista <- read.csv(pathListaExperimentos)
  experimentos.tiempos <- c()
  comando <- c("")
  for (i in 1:nrow(experimentos.lista)){
    comando <- paste(ruta.Sumo, " --net-file=\"", ruta.Nets, experimentos.lista$net[i], "\"", sep="")
    comando <- paste(comando, " --route-files=\"", ruta.Routes, experimentos.lista$route[i], extensiones.entradasRutas,"\"", sep="")
    if (generarReportes){
      comando <- paste(comando, " --tripinfo-output=\"", ruta.Outputs, experimentos.lista$route[i], "-", experimentos.lista$end[i],extensiones.salidaViajes, "\"", sep="") 
     comando <- paste(comando, " --vehroute-output=\"", ruta.Outputs, experimentos.lista$route[i], "-", experimentos.lista$end[i],extensiones.salidaVehiculos,"\"", sep="")  
     comando <- paste(comando, " --summary=\"", ruta.Outputs, experimentos.lista$route[i], "-", experimentos.lista$end[i],extensiones.salidaSummary,"\"", sep="")  
     comando <- paste(comando, " --vehroute-output.exit-times=\"True\"", sep="")    
   }
    comando <- paste(comando, " --time-to-teleport=\"-1\"", sep="")
    comando <- paste(comando, " --end=\"", experimentos.lista$end[i], "\"", sep="")
  
    if (registrarTiemposComputos){
      tiempoInicial <- Sys.time()  
    }
   shell(print(comando))
   if (registrarTiemposComputos){
      tiempoFinal <-Sys.time()    
      experimentos.tiempos <- c(experimentos.tiempos, tiempoFinal - tiempoInicial)    
      write.csv(experimentos.tiempos, "tiemposExperimentos.csv")
    }
  }
}

consolidarResultados <- function(pathDirectorioResultados){
  currDir <- getwd()
  setwd(pathDirectorioResultados)
  vectorArchivos <- dir()  
  myXml <- newXMLDoc()
  addChildren(myXml,newXMLNode("Experimentos"))
  saveXML(myXml, paste(ruta.ROutputs, "resultadosConsolidados.xml", sep=""))
  for (i in 1:length(vectorArchivos)){
    auxXmlNode <- xmlRoot(xmlParse(vectorArchivos[i]))
    myXml <-xmlParse(paste(ruta.ROutputs, "resultadosConsolidados.xml", sep=""))
    addAttributes(auxXmlNode, simul=vectorArchivos[i])
    addChildren(xmlRoot(myXml),auxXmlNode ) 
    saveXML(myXml, paste(ruta.ROutputs, "resultadosConsolidados.xml", sep=""))    
    myXml <- newXMLDoc()
  }
  setwd(currDir)
  myXml
}

#realizarExperimentos("listaExperimentos.csv")
#misResultados <- consolidarResultados(ruta.Outputs)
#saveXML(misResultados, paste(ruta.ROutputs, "resultadosConsolidados.xml", sep=""))
consolidarResultados(ruta.Outputs)