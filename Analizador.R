setwd("D:\\Compartido\\Proyectos\\doctorado\\papers\\Experimentos_OvS_Densidad_Poblacional")

SummConsTotal <- read.csv("Summ_Consolidado_Total.csv", sep=";")
SummConsTotal$meanTravelTime_Numeric<-as.numeric(sub(pattern=",", x=SummConsTotal$meanTravelTime, replacement="."))
SummConsTotal$Net <- factor(substr(SummConsTotal$simulation,1,regexpr("-", SummConsTotal$simulation)[1]-1))
SummConsTotal$meanTravelTime_Numeric[SummConsTotal$meanTravelTime_Numeric==-1] <-NA
plot(SummConsTotal$time, SummConsTotal$meanTravelTime_Numeric)
boxplot(SummConsTotal$meanTravelTime_Numeric)

#Figure 7
par.original <- par(no.readonly = TRUE)
par(cex=0.75)
par(bg = "lightyellow")
par(bty = "n")
boxplot(SummConsTotal$meanTravelTime_Numeric~SummConsTotal$Net, col = "gray90", border = "grey",  notch=TRUE, boxwex=0.25)
par(par.original)

#Figure 6
par.original <- par(no.readonly = TRUE)

par(bg = "lightyellow")
plot(SummConsTotal$time, SummConsTotal$meanTravelTime_Numeric, xlab="Simulated Time", ylab="Mean Travel Time", col="blue",type="l")
par(par.original)

#Figure 5
setwd("D:\\Compartido\\Proyectos\\doctorado\\papers\\Experimentos_OvS_Densidad_Poblacional\\Scripts")
tposComputo <- read.csv("tiempos_corridas_3500.csv",sep=";")
tposComputo$Tiempo.Corrida_Numeric <- as.numeric(sub(pattern=",", x=tposComputo$Tiempo.Corrida, replacement="."))
par.original <- par(no.readonly = TRUE)
par(cex=0.75)
par(bg = "lightyellow")
par(bty = "n")
boxplot(tposComputo$Tiempo.Corrida_Numeric~tposComputo$Type, col = "gray90", border = "grey",  notch=TRUE, boxwex=0.25, ylab="Computational Time(seg)")
par(par.original)
