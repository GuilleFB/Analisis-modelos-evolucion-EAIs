rm(list=ls(all=TRUE)) # Borrar todo los objetos
invisible(capture.output(gc())) # Limpiar la memoria

library(readxl)
library(RColorBrewer)
library(readr)
library(tidyr)
library(dplyr)
library(rgdal)
library(raster)
library(knitr)
library(sp)
library(sf)
library(readxl)
library(gdistance)
library(rgeos)
library(ggplot2)
library(ggimage)
library(maptools)
library(sf)
library(smoothr)
library(worms)
library(stringi)
library(lubridate)
library(mapview)
library(stringr)
library(beepr)
library(httr)
library(tidyverse)
library(ggspatial)
library(tidygeocoder)
library(drc)
library(nlme)
library(aomisc)

Archivo=file.choose()
BD_EAI=read.csv2(Archivo,sep = ";", dec = ",")
Directorio=paste(strsplit(Archivo,"[\\]")[[1]][1:(length(strsplit(Archivo,"[\\]")[[1]])-1)], collapse ="\\")
setwd(Directorio)
Datos=BD_EAI

# Limpieza datos #############################
a=gsub("\\D+", "_", Datos$Year_First_record)
b=strsplit(a,"_")

for (i in 1:length(b)) {
  b[[i]]=b[[i]][which(as.numeric(b[[i]])>=1800)]
  b[[i]]=min(b[[i]])
}

Datos$Year_First_record=unlist(b)

Datos=Datos[which(!is.na(Datos$First_Reference)),]
Datos=Datos[-which(Datos$Archivo=="BD_registros completos_todas demarcaciones.xlsx"),]
Datos=Datos[-which(Datos$Archivo=="Marines al litoral catal\xe0.xlsx"),]

Tabla=table(Datos$phylum,Datos$Demarcacion)

library(RColorBrewer)
coul <- brewer.pal(5, "Set2")

Tabla2=as.data.frame(Tabla)

ggplot(Tabla2, aes(Var1,Freq,group=Var2)) + 
  #geom_line(aes(color=Var2)) + 
  geom_point(aes(color=Var2),size = 5)+
  theme_minimal()+
  labs(title="", 
       subtitle = "", 
       y="Number of ind.", 
       x="Phylum")+
  scale_colour_discrete("Marine Division")+
  theme(axis.text.x = element_text(angle=90))
  

# Obtener los datos solo de nuevas referencias por demarcacion #############################
paste("El año mas viejo y mas nuevo", min(Datos$Year_First_record, na.rm = T),max(Datos$Year_First_record, na.rm = T), sep=" ")
# Buscar por fecha 2004-2009; 2010-2015; 2016-2021
# 1998
Demarca=c("NOR","SUD","ESAL","LEBA","CAN")
Years=1950:2022
Data=data.frame(matrix(NA,length(Years),length(Demarca)))
colnames(Data)=Demarca

for (j in 1:length(Demarca)) {
  Especies=NULL
  for (i in 1:length(Years)) {
  temp1=which(Datos$Year_First_record==Years[i])
  temp=which(str_detect(Datos$Demarcacion[temp1],Demarca[j])&!is.na(Datos$First_Reference[temp1]))
  Especies=c(Especies,unique(Datos$scientificname[temp]))
  Data[i,j]=length(unique(Datos$scientificname[temp]))
  }
}

Data$Years=Years
Data$Año=1:length(Data$NOR)
Data$Ciclos=sort(rep(seq(1:(length(Years)/6)),6))[1:length(Data$NOR)]
Data$Ciclos[which(is.na(Data$Ciclos))]=max(as.numeric(Data$Ciclos),na.rm = T)+1

# NOR SUD ESAL LEBA CAN
Data$cumNOR=cumsum(Data$NOR)
Data$cumSUD=cumsum(Data$SUD)
Data$cumESAL=cumsum(Data$ESAL)
Data$cumLEBA=cumsum(Data$LEBA)
Data$cumCAN=cumsum(Data$CAN)
head(Data)

ggplot(Data,aes(x=Years,y=NOR))+geom_point()
ggplot(Data,aes(x=Years,y=SUD))+geom_point()
ggplot(Data,aes(x=Years,y=ESAL))+geom_point()
ggplot(Data,aes(x=Years,y=LEBA))+geom_point()
ggplot(Data,aes(x=Years,y=CAN))+geom_point()

ggplot(Data,aes(x=Years,y=cumNOR))+geom_point()
ggplot(Data,aes(x=Years,y=cumSUD))+geom_point()
ggplot(Data,aes(x=Years,y=cumESAL))+geom_point()
ggplot(Data,aes(x=Years,y=cumLEBA))+geom_point()
ggplot(Data,aes(x=Years,y=cumCAN))+geom_point()

# Modelos estadisticos por demarcacion #####################
log.model1 <- nls(cumCAN ~ SSlogis(Years, Asym, xmid, scal), data = Data)

summary(log.model1)
plot(Data$Years,Data$cumCAN,col=rgb(0.4,0.4,0.8,0.6), pch=16 , cex=2, main="Demarcacion Canaria",
     xlab="Years",
     ylab="Accumulated New Species")
myPredict <- predict( log.model1 , interval="predict" ) 
ix <- sort(Data$Years,index.return=T)$ix
lines(Data$Years[ix], myPredict[ix], col=2, lwd=2 )

# I add the features of the model to the plot
coeff <- coefficients(log.model1)
text(1990, 50 , paste("Model : ",round(coeff[1],2) , " / 1 + exp(-x - " , round(coeff[2],) , ")/" , round(coeff[3],2) ))

# Asym= representa el numero maximo o maxima carga de la poblacion
# xmid= el momento en el que se da el punto de inflexion
# scal= en el punto de inflexion el ratio
alpha <- coef(log.model1)  #extracting coefficients
plot(cumCAN ~ Years, data = Data, main = "Logistic NIS Population", 
     xlab = "Year", ylab = "Population")  # Census data
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue")
cor.test(Data$cumCAN, residuals(log.model1))

Data$pred_cumCAN=curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue", n = length(Data$cumCAN))$y
cor.test(Data$cumCAN, residuals(model2))
ggplot(Data,aes(x=Years,y=cumCAN))+
  geom_point()+
  geom_line(aes(x=Years,y=pred_cumCAN))+
  theme_minimal()


model2 <- lm(log(cumCAN+0.1) ~ log(Years+0.1), data=Data)
summary(model2)
confint(model2)
alpha <- coef(model2)
plot(Data$cumCAN ~ Data$Years, main = "Power curve")
a=curve(exp(alpha[1])*x^alpha[2], add = T, col = "blue", n = length(Data$Years))

Data$pred_cumCAN=curve(exp(alpha[1])*x^alpha[2], add = T, col = "blue", n = 172)$y
cor.test(Data$cumCAN, residuals(model2))
ggplot(Data,aes(x=Years,y=cumCAN))+
  geom_point()+
  geom_line(aes(x=Years,y=pred_cumCAN))+
  theme_minimal()

model3 <- lm(cumCAN ~ Years, data=Data)
summary(model3)
confint(model3)
alpha <- coef(model3)
plot(Data$cumCAN ~ Data$Years, main = "Power curve")
a=curve(alpha[1]+x*alpha[2], add = T, col = "blue", n = length(Data$Years))

AIC(log.model1, model2, model3)

# Atlantico #############################
Atlantico=which(str_detect(Datos$Demarcacion,"SUD")|str_detect(Datos$Demarcacion,"NOR"))

Datos_Atlantico=Datos[Atlantico,]

Esp_Atlantico=sort(unique(Datos_Atlantico[,"scientificname"]))
NAS=which(is.na(Datos_Atlantico$Year_First_record))
Datos_Atlantico=Datos_Atlantico[-NAS,]

temp=which(stri_duplicated(Datos_Atlantico$scientificname))
Duplicados=Datos_Atlantico$scientificname[temp]

for (i in 1:length(Duplicados)) {
  temp2=which(Datos_Atlantico$scientificname==Duplicados[i])
  temp3=max(as.numeric(Datos_Atlantico$Year_First_record[temp2]))
  temp4=which(Datos_Atlantico$Year_First_record[temp2]==as.character(temp3))
  Datos_Atlantico=Datos_Atlantico[-temp2[temp4],]
}

Atlantico=data.frame(matrix(NA,length(Years),1))
colnames(Atlantico)="New_Species"

for (i in 1:length(Years)) {
  temp1=which(Datos_Atlantico$Year_First_record==Years[i])
  Atlantico[i,1]=length(temp1)
}

Atlantico$Years=Years
Atlantico$Año=1:length(Atlantico$New_Species)
Atlantico$Ciclos=sort(rep(seq(1:(length(Years)/6)),6))[1:length(Atlantico$New_Species)]
Atlantico$Ciclos[which(is.na(Atlantico$Ciclos))]=max(as.numeric(Atlantico$Ciclos),na.rm = T)+1

Atlantico$cumAtlantico=cumsum(Atlantico$New_Species)


# Modelo estadistico
log.model1 <- nls(cumAtlantico ~ SSlogis(Years, Asym, xmid, scal), data = Atlantico)

summary(log.model1)
plot(Atlantico$Years,Atlantico$cumAtlantico,col=rgb(0.4,0.4,0.8,0.6), pch=16 , cex=2, main="Atlantico",
     xlab="Years",
     ylab="Accumulated New Species")
myPredict <- predict( log.model1 , interval="predict" ) 
ix <- sort(Atlantico$Years,index.return=T)$ix
lines(Atlantico$Years[ix], myPredict[ix], col=2, lwd=2 )
# Asym= representa el numero maximo o maxima carga de la poblacion
# xmid= el momento en el que se da el punto de inflexion
# scal= en el punto de inflexion el ratio
alpha <- coef(log.model1)  #extracting coefficients
plot(cumAtlantico ~ Years, data = Atlantico, main = "Logistic NIS Population", 
     xlab = "Year", ylab = "Population")  # Census data
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue")
cor.test(Atlantico$cumAtlantico, residuals(log.model1))

model2 <- lm(log(cumAtlantico+0.1) ~ log(Years+0.1), data=Atlantico)
summary(model2)
confint(model2)
alpha <- coef(model2)
plot(Atlantico$cumAtlantico ~ Atlantico$Years, main = "Power curve")
curve(exp(alpha[1])*x^alpha[2], add = T, col = "blue", n = 172)

Data$pred_cumCAN=curve(exp(alpha[1])*x^alpha[2], add = T, col = "blue", n = 172)$y
cor.test(Data$cumCAN, residuals(model2))
ggplot(Data,aes(x=Years,y=cumCAN))+
  geom_point()+
  geom_line(aes(x=Years,y=pred_cumCAN))+
  theme_minimal()


BIC(log.model1, model2)

# Mediterraneo #############################

Mediterraneo=which(str_detect(Datos$Demarcacion,"LEBA")|str_detect(Datos$Demarcacion,"ESAL"))

Datos_Mediterraneo=Datos[Mediterraneo,]

Esp_Mediterraneo=sort(unique(Datos_Mediterraneo[,"scientificname"]))
NAS=which(is.na(Datos_Mediterraneo$Year_First_record))
Datos_Mediterraneo=Datos_Mediterraneo[-NAS,]

temp=which(stri_duplicated(Datos_Mediterraneo$scientificname))
Duplicados=Datos_Mediterraneo$scientificname[temp]

for (i in 1:length(Duplicados)) {
  temp2=which(Datos_Mediterraneo$scientificname==Duplicados[i])
  temp3=max(as.numeric(Datos_Mediterraneo$Year_First_record[temp2]))
  temp4=which(Datos_Mediterraneo$Year_First_record[temp2]==as.character(temp3))
  Datos_Mediterraneo=Datos_Mediterraneo[-temp2[temp4],]
}

Mediterraneo=data.frame(matrix(NA,length(Years),1))
colnames(Mediterraneo)="New_Species"

for (i in 1:length(Years)) {
  temp1=which(Datos_Mediterraneo$Year_First_record==Years[i])
  Mediterraneo[i,1]=length(temp1)
}

Mediterraneo$Years=Years
Mediterraneo$Año=1:length(Mediterraneo$New_Species)
Mediterraneo$Ciclos=sort(rep(seq(1:(length(Years)/6)),6))[1:length(Mediterraneo$New_Species)]
Mediterraneo$Ciclos[which(is.na(Mediterraneo$Ciclos))]=max(as.numeric(Mediterraneo$Ciclos),na.rm = T)+1

Mediterraneo$cumMediterraneo=cumsum(Mediterraneo$New_Species)

# Modelo estadistico
log.model1 <- nls(cumMediterraneo ~ SSlogis(Years, Asym, xmid, scal), data = Mediterraneo)

summary(log.model1)
plot(Mediterraneo$Years,Mediterraneo$cumMediterraneo,col=rgb(0.4,0.4,0.8,0.6), pch=16 , cex=2, main="",
     xlab="Years",
     ylab="Accumulated New Species")

# Graficos a?adiendo atlantico y canarias
# points(Atlantico$Years,Atlantico$cumAtlantico,col=rgb(0.6,0.2,0.4,0.6), pch=16 , cex=2)
# points(Data$Years,Data$cumCAN,col=rgb(0.2,0.6,0.6,0.6), pch=16 , cex=2)
# 
# legend("topleft",inset=.01, legend=c("WMED", "ABI", "AMA"),
#        col=c(rgb(0.4,0.4,0.8,0.6),rgb(0.6,0.2,0.4,0.6),rgb(0.2,0.6,0.6,0.6)), pch=16, cex=0.9, box.lty=0)
# 
# plot(Mediterraneo$Years,Mediterraneo$New_Species,col=rgb(0.4,0.4,0.8,0.6), pch=15 , cex=2, main="",
#      xlab="Years",
#      ylab="New Species")
# 
# points(Atlantico$Years,Atlantico$New_Species,col=rgb(0.6,0.2,0.4,0.6), pch=16 , cex=2)
# points(Data$Years,Data$CAN,col=rgb(0.2,0.6,0.6,0.6), pch=17 , cex=2)
# 
# legend("topleft",inset=.01, legend=c("WMED", "ABI", "AMA"),
#        col=c(rgb(0.4,0.4,0.8,0.6),rgb(0.6,0.2,0.4,0.6),rgb(0.2,0.6,0.6,0.6)), pch=c(15,16,17), cex=0.9, box.lty=0)

myPredict <- predict( log.model1 , interval="predict" ) 
ix <- sort(Mediterraneo$Years,index.return=T)$ix
lines(Mediterraneo$Years[ix], myPredict[ix], col=2, lwd=2 )
# Asym= representa el numero maximo o maxima carga de la poblacion
# xmid= el momento en el que se da el punto de inflexion
# scal= en el punto de inflexion el ratio
alpha <- coef(log.model1)  #extracting coefficients
plot(cumMediterraneo ~ Years, data = Mediterraneo, main = "Logistic NIS Population", 
     xlab = "Year", ylab = "Population")  # Census data
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue")
cor.test(Mediterraneo$cumMediterraneo, residuals(log.model1))

model2 <- lm(log(cumMediterraneo+0.1) ~ log(Years+0.1), data=Mediterraneo)
summary(model2)
confint(model2)
alpha <- coef(model2)
plot(Mediterraneo$cumMediterraneo ~ Mediterraneo$Years, main = "Power curve")
a=curve(exp(alpha[1])*x^alpha[2], add = T, col = "blue", n = 172)

Data$pred_cumCAN=curve(exp(alpha[1])*x^alpha[2], add = T, col = "blue", n = 172)$y
cor.test(Data$cumCAN, residuals(model2))
ggplot(Data,aes(x=Years,y=cumCAN))+
  geom_point()+
  geom_line(aes(x=Years,y=pred_cumCAN))+
  theme_minimal()


BIC(log.model1, model2)

# Plots Atlantico, Mediterraneo y Canarias #########################
ggplot(Mediterraneo,aes(x=Years,y=New_Species))+geom_point()
ggplot(Mediterraneo,aes(x=Years,y=cumMediterraneo))+geom_point()

ggplot(Atlantico,aes(x=Years,y=New_Species))+geom_point()
ggplot(Atlantico,aes(x=Years,y=cumAtlantico))+geom_point()

ggplot(Data,aes(x=Years,y=CAN))+geom_point()
ggplot(Data,aes(x=Years,y=cumCAN))+geom_point()





temp=which(Suma$Years>=1950)

log.model1 <- nls(cumDemarca[temp] ~ SSlogis(Years[temp], Asym, xmid, scal), data = Suma)
summary(log.model1)
# Asym= representa el numero maximo o maxima carga de la poblacion
# xmid= el momento en el que se da el punto de inflexion
# scal= en el punto de inflexion el ratio
alpha <- coef(log.model1)  #extracting coefficients
plot(cumDemarca[temp] ~ Years[temp], data = Suma, main = "Logistic NIS Population", 
     xlab = "Year", ylab = "Population")  # Census data
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue")
cor.test(Suma$cumDemarca[temp], residuals(log.model1))

log.model2 <- nls(cumDemarca[temp] ~ SSfpl(Years[temp], A, B, xmid, scal), data = Suma)
summary(log.model2)
# Asym= representa el numero maximo o maxima carga de la poblacion
# xmid= el momento en el que se da el punto de inflexion
# scal= en el punto de inflexion el ratio
alpha <- coef(log.model2)  #extracting coefficients
plot(cumDemarca[temp] ~ Years[temp], data = Suma, main = "Logistic NIS Population", 
     xlab = "Year", ylab = "Population")  # Census data
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue")
cor.test(Suma$cumDemarca[temp], residuals(log.model2))

log.model3 <- nls(cumDemarca[temp] ~ SSgompertz(Years[temp], Asym, b2, b3), data = Suma)
summary(log.model3)
# Asym= representa el numero maximo o maxima carga de la poblacion
# xmid= el momento en el que se da el punto de inflexion
# scal= en el punto de inflexion el ratio
alpha <- coef(log.model3)  #extracting coefficients
plot(cumDemarca[temp] ~ Years[temp], data = Suma, main = "Logistic NIS Population", 
     xlab = "Year", ylab = "Population")  # Census data
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue")
cor.test(Suma$cumDemarca[temp], residuals(log.model3))

model1.1 <- drm(cumDemarca[temp] ~ Years[temp], fct = DRC.expoGrowth(), data=Suma)
summary(model1.1)
confint(model1.1)
plot(Suma$cumDemarca[temp] ~ Suma$Years[temp], main = "Exponential curve")
pred = predict(model1.1)
plot(model1.1, main = "Exponential curve")
cor.test(Suma$cumDemarca[temp], residuals(model1.1))


model2 <- lm(log(cumDemarca[temp]) ~ log(Years[temp]), data=Suma)
summary(model2)
confint(model2)
plot(Suma$cumDemarca[temp] ~ Suma$Years[temp], main = "Power curve")
alpha <- coef(model2)
curve(exp(alpha[1])*x^alpha[2], add = T, col = "blue")
cor.test(Suma$cumDemarca[temp], residuals(model2))

AIC(log.model, model1.1, model2)

# Plots de nuevas especies por demarcacion

for (i in 1:length(Demarca)) {
temp=ggplot(Data,aes(Years,Data[,i]))+
  geom_point()+
  geom_smooth(method=lm,aes(fill=Ciclos))+
  labs(title=paste("Demarcacion ",Demarca[i], sep=""),
       x ="Years", y = "Number of new species")+
  theme_minimal()
  print(temp)
  # ggsave(paste("Demarcacion ",Demarca[i], ".jpeg", sep=""),plot = last_plot(),dpi=600)
  # dev.off()
}

# Plots de nuevas especies por demarcacion acumuladas
for (i in 1:length(Demarca)) {
  temp=ggplot(Data,aes(Years,cumsum(Data[,i])))+
    geom_point()+
    geom_hline(yintercept=0)+
    geom_smooth(method=lm,aes(fill=Ciclos))+
    labs(title=paste("Demarcacion ",Demarca[i], sep=""),
          x ="Years", y = "Accumulated number of new species")+
    theme_minimal()+
    theme(legend.position="bottom")
  print(temp)
  # ggsave(paste("Demarcacion acumuladas ",Demarca[i], ".jpeg", sep=""),plot = last_plot(),dpi=600)
  # dev.off()
}

# Plots con loess
for (i in 1:length(Demarca)) {
  temp=ggplot(Data,aes(Years,cumsum(Data[,i])))+
    geom_point()+
    geom_smooth()+
    labs(title=paste("Demarcaci?n ",Demarca[i], sep=""),
         x ="Years", y = "Accumulated number of new species")+
    theme_minimal()+
    theme(legend.position="bottom")
    
    print(temp)
    # ggsave(paste("Demarcacion loess ",Demarca[i], ".jpeg", sep=""),plot = last_plot(),dpi=600)
    # dev.off()
}

plot(density(cumsum(Data$NOR)), main="Density Plot: NOR", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cumsum(Data$NOR)), 2)))  # density plot for 'speed'
polygon(density(cumsum(Data$NOR)), col="red")

plot(density(Data$NOR), main="Density Plot: NOR", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Data$NOR), 2)))  # density plot for 'speed'
polygon(density(Data$NOR), col="red")

cor(Data$Years, cumsum(Data$NOR))
cor(Data$Years, Data$NOR)

# Modelo lineal sin acumulado
model <- lm(Data$NOR ~ Data$Año)
summary(model)
confint(model)
plot(Data$NOR ~ Data$Año, main = "Lineal regression")
pred = predict(model)
lines(pred,col="red")
cor.test(cumsum(Data$NOR), model$residuals)

Pendiente=as.data.frame(matrix(NA,nrow =length(unique(Data$Ciclos)),ncol = length(Demarca)))
colnames(Pendiente)=Demarca
for (j in 1:length(Demarca)) {
  for (i in 1:length(unique(Data$Ciclos))) {
    model <- lm(Data[which(Data$Ciclos==i),j] ~ Data$Año[which(Data$Ciclos==i)])
    Pendiente[i,j]=model$coefficients[2]
  }
}
Pendiente

# Analisis acumulativos
model0 <- lm(cumsum(Data$NOR) ~ Data$Año)
summary(model0)
confint(model0)
plot(cumsum(Data$NOR) ~ Data$Año, main = "Lineal regression")
pred = predict(model0)
lines(pred,col="red")
cor.test(cumsum(Data$NOR), model0$residuals)

# Acumulativo por ciclo
Pendiente=as.data.frame(matrix(NA,nrow =3,ncol = length(Demarca)))
colnames(Pendiente)=Demarca
for (j in 1:length(Demarca)) {
  for (i in 1:length(unique(Data$Ciclos))) {
    model <- lm(cumsum(Data[which(Data$Ciclos==i),j]) ~ Data$Año[which(Data$Ciclos==i)])
    Pendiente[i,j]=model$coefficients[2]
  }
}
Pendiente

model1 <- drm(cumsum(Data$NOR) ~ Data$Año, fct = DRC.asymReg() )
summary(model1)
confint(model1)
plot(cumsum(Data$NOR) ~ Data$A?o, main = "Asymptotic regression")
pred = predict(model1)
plot(model1, main = "Asymptotic regression")
cor.test(cumsum(Data$NOR), residuals(model1))

temp=ggplot(Data,aes(Año,cumsum(NOR)))+
  geom_point()+
  geom_line(aes(y = pred), size = 1)+
  labs(title=paste("Demarcaci?n ","NOR", sep=""),
       x ="Years", y = "Accumulated number of new species")+
  theme_minimal()+
  theme(legend.position="bottom")
print(temp)

model2 <- drm(cumsum(Data$NOR) ~ Data$A?o, fct = DRC.logCurve() ) 
summary(model2)
confint(model2)
plot(cumsum(Data$NOR) ~ Data$A?o, main = "Logaritmic regression")
pred = predict(model2)
lines(pred, col="red")
plot(model2, main = "Logaritmic regression")
cor.test(cumsum(Data$NOR), residuals(model2))

model3 <- drm(cumsum(Data$NOR) ~ Data$A?o, fct = L.3() ) 
summary(model3)
confint(model3)
plot(cumsum(Data$NOR) ~ Data$A?o, main = "Logistic regression")
pred = predict(model3)
lines(pred, col="red")
plot(model3, main = "Logistic regression")
cor.test(cumsum(Data$NOR), residuals(model3))



model4 <- drm(cumsum(Data$NOR) ~ Data$A?o, fct = LL.3() ) 
summary(model4)
confint(model4)
plot(cumsum(Data$NOR) ~ Data$A?o, main = "Log-logistic regression")
pred = predict(model4)
lines(pred, col="red")
plot(model4, main = "Log-logistic regression")
cor.test(cumsum(Data$NOR), residuals(model4))

# the model with the lowest AIC and BIC score is preferred.
AIC(model0,model1,model2,model3,model4)

# Sin acumulado log-logistico
model4.1 <- drm(Data$NOR ~ Data$A?o, fct = LL.3() ) 
summary(model4.1)
confint(model4.1)
plot(Data$NOR ~ Data$A?o, main = "Log-logistic regression")
pred = predict(model4.1)
lines(pred, col="red")
plot(model4.1, main = "Log-logistic regression")
cor.test(Data$NOR, residuals(model4.1))



# Comparacion de modelos log-logisticos de acumulado
model0.1 <- drm(cumsum(Data$NOR) ~ Data$A?o, fct = LL.2() ) 
summary(model0.1)
confint(model0.1)
plot(cumsum(Data$NOR) ~ Data$A?o, main = "Log-logistic regression")
pred = predict(model0.1)
lines(pred, col="red")
plot(model0.1, main = "Log-logistic regression")
cor.test(cumsum(Data$NOR), residuals(model0.1))

model0.2 <- drm(cumsum(Data$NOR) ~ Data$A?o, fct = LL.3() ) 
summary(model0.2)
plot(cumsum(Data$NOR) ~ Data$A?o, main = "Log-logistic regression")
pred = predict(model0.2)
lines(pred, col="red")
plot(model0.2, main = "Log-logistic regression")
cor.test(cumsum(Data$NOR), residuals(model0.2))

model0.3 <- drm(cumsum(Data$NOR) ~ Data$A?o, fct = LL.4() ) 
summary(model0.3)
plot(cumsum(Data$NOR) ~ Data$A?o, main = "Log-logistic regression")
pred = predict(model0.3)
lines(pred, col="red")
plot(model0.3, main = "Log-logistic regression")
cor.test(cumsum(Data$NOR), residuals(model0.3))

model0.4 <- drm(cumsum(Data$NOR) ~ Data$A?o, fct = LL.5() ) 
summary(model0.4)
plot(cumsum(Data$NOR) ~ Data$A?o, main = "Log-logistic regression")
pred = predict(model0.4)
lines(pred, col="red")
plot(model0.4, main = "Log-logistic regression")
cor.test(cumsum(Data$NOR), residuals(model0.4))

AIC(model0.1,model0.2,model0.3,model0.4)

# Modelo con todos los datos de las demarcaciones
modelX= drm(rowSums(data.frame(cumsum(Data[,1:5])))~ Data$A?o, fct = LL.5() )
summary(modelX)
plot(rowSums(data.frame(cumsum(Data[,1:5]))) ~ Data$A?o, main = "Log-logistic regression")
pred = predict(modelX)
lines(pred, col="red")
plot(modelX, main = "Log-logistic regression")
cor.test(rowSums(data.frame(cumsum(Data[,1:5]))), residuals(modelX))

modelX.1 <- lm(rowSums(data.frame(cumsum(Data[,1:5]))) ~ Data$A?o)
summary(modelX.1)
confint(modelX.1)
plot(rowSums(data.frame(cumsum(Data[,1:5]))) ~ Data$A?o, main = "Lineal regression")
pred = predict(modelX.1)
lines(pred,col="red")
cor.test(rowSums(data.frame(cumsum(Data[,1:5]))), modelX.1$residuals)

# Acumulado por ciclos
Data2=Data
Data2[,1:5]=NA

for (i in 1:length(unique(Data$Ciclos))) {
  temp=cumsum(Data[which(Data$Ciclos==i),1:5])
  Data2[which(Data$Ciclos==i),1:5]=temp
}

for (i in 1:length(Demarca)) {
  temp=ggplot(Data2,aes(Years,Data2[,i], color=Ciclos, shape=Ciclos))+
    geom_point()+
    geom_smooth(method=lm,aes(fill=Ciclos))+
    labs(title=paste("Demarcaci?n ",Demarca[i], sep=""),
         x ="Years", y = "Number of new species")+
    theme_minimal()
  print(temp)
  #ggsave(paste("Demarcacion ",Demarca[i], ".jpeg", sep=""),plot = last_plot(),dpi=600)
  #dev.off()
}

modelY.NOR <- lm(Data2$NOR ~ Data2$A?o)
summary(modelY.NOR)
confint(modelY.NOR)
plot(Data2$NOR ~ Data2$A?o, main = "Lineal regression")
pred = predict(modelY.NOR)
lines(pred,col="red")
cor.test(Data2$NOR, modelY.NOR$residuals)

modelY.SUD <- lm(Data2$SUD ~ Data2$A?o)
summary(modelY.SUD)
confint(modelY.SUD)
plot(Data2$SUD ~ Data2$A?o, main = "Lineal regression")
pred = predict(modelY.SUD)
lines(pred,col="red")
cor.test(Data2$SUD, modelY.SUD$residuals)



# Modelo logistico en R ####

#par(mfrow = c(1, length(unique(Demarca))))
par(mfrow = c(2, 3))
for (j in 9:13) {
df.bar <- barplot(Data[,j-8],ylim=c(0,max(Data[,j])+50), names.arg = Data$Years, 
                  xlab="Years", 
                  ylab="Acc.number New NIS", 
                  main=paste("Demarcacion",colnames(Data)[j-8]))

points(x = df.bar, 
       Data[,j], 
       pch = 16, 
       cex = 1, 
       ylab = "Acc. New NIS", 
       xlab = "Years")

log.model1 <- glm(Data[,j-8] ~ Years, data=Data)
summary(log.model1)

lats = seq(min(Data$Years), max(Data$Years), 1)

probs = predict(log.model1, 
                newdata = data.frame(Years = lats), 
                type = "response", 
                se.fit = TRUE)

pm = probs$fit
pu = probs$fit + probs$se.fit * 1.96 # 95% confidence interval
pl = probs$fit - probs$se.fit * 1.96 # 95% confidence interval

lines(x = df.bar, pm, lwd = 2, lty =3)
lines(x = df.bar, pu, lwd = 2, col = "blue")
lines(x = df.bar, pl, lwd = 2, col = "blue")

cc <- log.model1$coefficients
eqn <- paste("Y =", paste(round(cc[-1],2), names(cc[-1]), sep=" * "), paste("+", round(cc[1],2), sep=" "), sep = " ")

text(19, max(Data[,j-8])+15, labels = eqn, cex=1)

# Log-logaritmico
log.model2 <- nls(Data[,j] ~ SSlogis(Years, Asym, xmid, scal), data = Data)
summary(log.model2)
# Asym= representa el numero maximo o maxima carga de la poblacion
# xmid= el momento en el que se da el punto de inflexion
# scal= en el punto de inflexion el ratio

lats = seq(min(Data$Years), max(Data$Years), 1)

probs2 = predict(log.model2, 
                newdata = data.frame(Years = lats), 
                type = "response", 
                se.fit = TRUE)

alpha <- coef(log.model2)
lines(x = df.bar, alpha[1]/(1 + exp(-(Data$Years - alpha[2])/alpha[3])), col = "red",lwd = 2)

for (i in 1:length(unique(Data$Ciclos))) {
  
  temp=which(Data$Ciclos==i)
  log.model <- glm(Data[,j][temp] ~ Data$Years[temp])
  summary(log.model)
  
  lats = seq(min(Data$Years[temp]), max(Data$Years[temp]), 1)
  
  probs = predict(log.model, 
                  newdata = data.frame(Years = lats), 
                  type = "response", 
                  se.fit = TRUE)
  
  pm = probs$fit
  pu = probs$fit + probs$se.fit * 1.96 # 95% confidence interval
  pl = probs$fit - probs$se.fit * 1.96 # 95% confidence interval
  
  #polygon(c(rev(lats),lats), c(rev(pl),pu),
  #       col = "grey90", border = NA)
  a=c(1,7,13,19,25)
  lines(df.bar[a[i]:(a[i+1]-1),1], pm, lwd = 2)
  lines(df.bar[a[i]:(a[i+1]-1),1], pu, lwd = 2, col = "green")
  lines(df.bar[a[i]:(a[i+1]-1),1], pl, lwd = 2, col = "green")
  
  text(df.bar[a[i+1]-1]-3, max(Data[,j][temp])+20, labels = round(log.model$coefficients[2],1), cex=1)
  
  if (i!=4) {
    abline(v=df.bar[a[i+1]-1]+0.5, lty=2)
    # abline(h=0.5, lty=2)
    # abline(h=0.9, lty=2)
  }
}

#ggsave("Demarcaciones.jpeg", sep="",dpi=600)
}
par(mfrow = c(1, 1))









pop.ss <- nls(cumCAN ~ SSlogis(Years, phi1, phi2, phi3), data=Data)
summary(pop.ss)

alpha <- coef(pop.ss)  #extracting coefficients
plot(cumCAN ~ Years, data = Data, main = "Logistic Growth Model of Australian Population", 
     xlab = "Year", ylab = "Population")  # Census data
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "blue")  # Fitted model


predict.pop.ss <- predict(pop.ss, data.frame(Years = c(2025:2050)))
predict.pop.ss

theta <- coef(pop.ss)  #extracting coefficients
plot(cumCAN ~ Years, data = Data, main = "Logistic Growth Model of Australian Population to 1925", 
     xlab = "Year", ylab = "Population", xlim=c(1998,2060), ylim=c(0,300))  # Census data
curve(theta[1]/(1 + exp(-(x - theta[2])/theta[3])), add = T, col = "green")  # Fitted model
points(c(2025:2050), round(predict.pop.ss[1:24],0))

