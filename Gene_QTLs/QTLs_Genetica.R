# Mediciones de traits poligenicos: QTLS 
# Tarea grupal Genetica

# Notas: Estado_Nat se refiere al estado de nacimiento
#        Estado_Dura se refiere al estado donde ha vivido mas la persona
#        Region 1 solo sn Norte, Central y Sur
#        Region 2 lo mismo pero con Central dividido en Occidente, Oriente, Central Norte y Central Sur
#        Altura esta medida en metros y Calzado en cm

rm(list=ls())
setwd("~/Documents/Genoblastulas/Gene_QTLs")
####################################
#    Internalizacion de datos      #
####################################
D = read.csv("MedicionesQTLs_Geneticacsv", header = FALSE)
D = as.data.frame(D)
D = D[1:22]
D[,23] = c("Norte",  "Central","Central","Central","Central","Central","Central","Central","Central","Central",
           "Central","Central","Sur",    "Central","Norte",  "Central","Central","Central","Central","Central",
           "Central","Central","Sur",    "Central","Central","Norte",  "Sur",    "Central","Central","Central","Central")
############  1              2             3            4               5               6            7                8           9             10
D[,24] = c("Norte",     "CentralSur",  "CentralSur","CentralSur",    "CentralSur", "CentralNor",  "CentralNor",  "Occidente", "Occidente",  "Oriente",
           "Occidente","CentralSur",  "Sur",       "Occidente",    "Norte",      "Occidente",  "CentralNor",  "CentralSur", "CentralSur",  "CentralSur",
           "CentralSur","CentralNor",  "Sur",       "CentralSur",    "CentralSur",  "CentralNor",   "Sur",      "CentralSur",   "Oriente",   "CentralNor",    "CentralSur")

colnames(D) = c("Nombre","Sexo","Edad","Estado_Nat","Estado_Dura","Altura","PigmFr1","PigmFr2","PigmFr3",
                "PigmFr4","PigmFr5","PigmFrAvg","PigmBr1","PigmBr2","PigmBr3","PigmBr4","PigmBr5","PigmBrAvg",
                "Calzado","PS_Sis","PS_Dia","Dia_med","Region1","Region2")

####################################
#    ...     #
####################################
#Graficar el nivel de melanina de acuerdo al sexo.
#Separación en hombres y mujeres.
num=seq(1,length(D[,1]))
M=num[D$Sexo=="M"]
H=num[D$Sexo=="H"]

#Datos de individuos no extranjeros, posteriormente se separa en hombres y mujeres no extranjeros.
D_mex=D[-2,]
num=seq(1,length(D_mex[,1]))
M_mex=num[D_mex$Sexo=="M"]
H_mex=num[D_mex$Sexo=="H"]

#Pigmentación en frente
h_M=hist(D$PigmFrAvg[M],freq=F,breaks=c(38,40,42,44,46,48,50,52,54,56,58))
h_H=hist(D$PigmFrAvg[H],freq=F,breaks=c(38,40,42,44,46,48,50,52,54,56,58))
h_M$density=(h_M$density)*(1/sum(h_M$density))
h_H$density=(h_H$density)*(1/sum(h_H$density))
barplot(h_M$density,col=rgb(0.9, 0.1, 0.1, 0.3),space=2,ylim=c(0,.35),xlab="Nivel de pigmentación",ylab="Proporción de individuos",
        main="Pigmentación en brazos por sexo", names.arg=c("38-40","40-42","42-44","44-46","46-48","48-50","50-52","52-54","54-56","56-58"))
barplot(h_H$density,space=c(3,2,2,2,2,2,2,2,2,2),col=rgb(0.1, 0.9, 0.1, 0.3),add=T)
legend("topright", c("Mujeres", "Hombres"), col=c(rgb(0.9, 0.1, 0.1, 0.3),rgb(0.1, 0.9, 0.1, 0.3)), lwd=10)

#Pigmentación en brazos
h_M=hist(D_mex$PigmBrAvg[M_mex],freq=F,breaks=c(34,36,38,40,42,44,46,48,50))
h_H=hist(D_mex$PigmBrAvg[H_mex],freq=F,breaks=c(34,36,38,40,42,44,46,48,50))
h_M$density=(h_M$density)*(1/sum(h_M$density))
h_H$density=(h_H$density)*(1/sum(h_H$density))
barplot(h_M$density,col=rgb(0.9, 0.1, 0.1, 0.3),space=2,xlab="Nivel de pigmentación",ylab="Proporción de individuos",
        main="Pigmentación en brazos por sexo", names.arg=c("34-36","36-38","38-40","40-42","42-44","44-46","46-48","48-50"))
barplot(h_H$density,space=c(3,2,2,2,2,2,2,2),col=rgb(0.1, 0.9, 0.1, 0.3),add=T)
legend("topright", c("Mujeres", "Hombres"), col=c(rgb(0.9, 0.1, 0.1, 0.3),rgb(0.1, 0.9, 0.1, 0.3)), lwd=10)

#Altura
h_M=hist(D_mex$Altura[M_mex],freq=F,breaks=c(1.5,1.55,1.6,1.65,1.7,1.75,1.8,1.85))
h_H=hist(D_mex$Altura[H_mex],freq=F,breaks=c(1.5,1.55,1.6,1.65,1.7,1.75,1.8,1.85))
h_M$density=(h_M$density)*(1/sum(h_M$density))
h_H$density=(h_H$density)*(1/sum(h_H$density))
barplot(h_M$density,col=rgb(0.9, 0.1, 0.1, 0.3),space=2,xlab="Altura",ylab="Proporción de individuos",
        main="Altura por sexo", names.arg=c("1.5-1.55","1.55-1.6","1.6-1.65","1.65-1.7","1.7-1.75","1.75-1.8","1.8-1.85"))
barplot(h_H$density,space=c(3,2,2,2,2,2,2),col=rgb(0.1, 0.9, 0.1, 0.3),add=T)
legend("topright", c("Mujeres", "Hombres"), col=c(rgb(0.9, 0.1, 0.1, 0.3),rgb(0.1, 0.9, 0.1, 0.3)), lwd=10)

