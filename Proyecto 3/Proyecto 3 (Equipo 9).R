# Administracion Integral de Riesgos
# Proyecto 3

library(TTR)
library(tidyquant)
library(quantmod)
library(knitr) #for kable()

#######################
#Nota

# El codigo solo esta hecho para mostrar los resultados de una emisora a la vez, para
# seleccionar otra emisora solo se debe cambiar manualmente el nombre de la emisora 
# deseada en las lineas de codigo con numero: 23 y 29

#######################
# Cargamos los datos

# cartera=c("WALMEX.MX","AMXL.MX","GFNORTEO.MX",
#          "GMEXICOB.MX","TLEVISACPO.MX","KIMBERA.MX",
#          "GCARSOA1.MX","GAPB.MX","PE&OLES.MX"."LABB.MX")

  cartera = c("GFNORTEO.MX")
  getSymbols(cartera,src = "yahoo",from="2019-01-01",to="2022-09-01")
  

#######################
# Precios de cierre
  precios<-GFNORTEO.MX$GFNORTEO.MX.Close
  colnames(precios)<-c("Precios de cierre")
  head(precios,5)


#######################
# Rendimientos
  rendimientos<-diff(precios)/precios
  rendimientos<-rendimientos[c(-1),]
  colnames(rendimientos)<-"Rendimiento"
  head(rendimientos,5)


#######################
# Ultimo precio
  ultimo_precio<-precios[length(precios),]
  ultimo_precio<-as.vector(ultimo_precio) # Lo convertimos a vector para poder hacer cuentas


#######################
# Revaluación
  revaluacion<-ultimo_precio*(1+rendimientos)
  colnames(revaluacion)<-"Revaluacion"
  head(revaluacion,5)
  
#######################
# P&L
  PL<-ultimo_precio-revaluacion
  colnames(PL)<-"PL"
  head(PL,5)

    
# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Metodo de Simulacion Historica
  
# Niveles de confianza
  alpha<-c(0.95,0.975,0.99)

  
#######################
# VaR No Parametrico (Simulacion Historica - 1 dia)
  VaR_SH1<-quantile(PL,probs=alpha)

#######################
# Resultados

  VaR_SH<-rbind(VaR_SH1,VaR_SH1*30,VaR_SH1*180,VaR_SH1*360)
  rownames(VaR_SH)<-c("1 dia","30 dias","180 dias", "360 dias")
  
  kable(VaR_SH,digits=4,caption = "Método de Simulación Histórica")
  

#######################
# Verificacion de resultados
  
  d <- density(PL) # returns the density data
  plot(d,main = "Densidad de P&L - Simulacion Historica")
  abline(v = VaR_SH1, col="red", lwd=3, lty=2)
  abline(v = quantile(PL,alpha), col="blue", lwd=3, lty=2)
  

  

# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Metodo de Simulacion Montecarlo
  
# Calculamos la media y los rendimientos de la emisora  
  mean<-mean(rendimientos)  
  sd<-sd(rendimientos)  

  
VaRSM95<-vector()  
VaRSM975<-vector()  
VaRSM99<-vector()  

for(i in 1:5000){

  # Simulacion de rendimientos (normal)
  rendimiento_simulado<-rnorm(length(rendimientos),mean = mean,sd = sd)
  
  # Revaluacion
  revaluacionSM<-ultimo_precio*(1+rendimiento_simulado)
  
  # P&L
  PL_SM<-ultimo_precio-revaluacionSM
  
  # Calculamos el VaR a diferentes niveles de confianza  
  VaR_SM95<-quantile(PL_SM,0.95)
  VaR_SM975<-quantile(PL_SM,0.975)
  VaR_SM99<-quantile(PL_SM,0.99)
  
  # Guardamos los VaR en vectores
  VaRSM95[i]<-VaR_SM95  
  VaRSM975[i]<-VaR_SM975  
  VaRSM99[i]<-VaR_SM99  

}


#######################
# VaR No Parametrico (Simulacion de Montecarlo - 1 dia)

VaR_Montecarlo95<-mean(VaRSM95)
VaR_Montecarlo975<-mean(VaRSM975)
VaR_Montecarlo99<-mean(VaRSM99)

  VaR_SM1<-cbind(VaR_Montecarlo95,VaR_Montecarlo975,VaR_Montecarlo99)
  colnames(VaR_SM1)<-c("95%","97.5%","99%")


#######################
# Resultados

  VaR_SM<-rbind(VaR_SM1,VaR_SM1*30,VaR_SM1*180,VaR_SM1*360)
  rownames(VaR_SM)<-c("1 dia", "30 dias", "180 dias", "360 dias")

  kable(VaR_SM,digist=4, caption="Método de Simulación MonteCarlo")
  
  
#######################
# Verificacion de resultados
  
  d <- density(PL_SM) # returns the density data
  plot(d,main = "Densidad de P&L - Montecarlo")
  abline(v = VaR_SM1, col="red", lwd=3, lty=2)
  abline(v = quantile(PL_SM,alpha), col="blue", lwd=3, lty=2)
  
  

# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Bootstrapping

# Funcion de P&L
head(PL,5)

VaRB95<-vector()  
VaRB975<-vector()  
VaRB99<-vector()  


  for(i in 1:5000){
  
    # Hacemos un remuestreo con la P&L
      remuestreoBoots<-sample(PL, size=length(PL), replace = TRUE)

    # Calculamos el VaR con diferentes niveles de confianza  
      VaR_B95<-quantile(remuestreoBoots,0.95)
      VaR_B975<-quantile(remuestreoBoots,0.975)
      VaR_B99<-quantile(remuestreoBoots,0.99)

    # Metemos los VaR calculados en un vector  
      VaRB95[i]<-VaR_B95  
      VaRB975[i]<-VaR_B975  
      VaRB99[i]<-VaR_B99  
    }


#######################
# VaR No Parametrico (Simulacion Bootstrapping - 1 dia)
  
VaRBoots95<-mean(VaRB95)
VaRBoots975<-mean(VaRB975)
VaRBoots99<-mean(VaRB99)

  VaR_B1<-cbind(VaRBoots95,VaRBoots975,VaRBoots99)
  colnames(VaR_B1)<-c("95%","97.5%","99%")

  
#######################
# Resultados

  VaR_B<-rbind(VaR_B1,VaR_B1*30,VaR_B1*180,VaR_B1*360)
  rownames(VaR_B)<-c("1 dia", "30 dias", "180 dias", "360 dias")

  kable(VaR_B,digist=4, caption="Método Bootstrapping")


#######################
# Verificacion de resultados
  
  d <- density(remuestreoBoots) # returns the density data
  plot(d,main = "Densidad de P&L - Bootstrapping")
  abline(v = VaR_B1, col="red", lwd=3, lty=2)
  abline(v = quantile(remuestreoBoots,alpha), col="blue", lwd=3, lty=2)
  
  
    

# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Alisado Exponencial

head(PL,5)  
  
# Definimos los coeficientes alpha y beta  
a<-0.95
b<-0.05


# Creamos la funcion de alisado exponencial

j<-length(PL)-1 # Indicador
Alisado<-vector()


  for(i in 0:j){

    Alisado[length(PL)-i]<-a^(i)*b
    
  }

head(Alisado,5) # Alisado es un vector


# Unimos en una tabla la P&L y el Alisado
  Tabla_Alisado<-cbind(PL,Alisado)
  colnames(Tabla_Alisado)<-c("PL","Alisado")
  head(Tabla_Alisado)

  
# Lo convertimos a dataframe
  Tabla_Alisado<-as.data.frame(Tabla_Alisado)

  
# Ordenamos tabla de forma descendente basandonos en la columna P&L
  Tabla_Alisado <- Tabla_Alisado[with(Tabla_Alisado, order(-Tabla_Alisado$PL)), ] 
  head(Tabla_Alisado)

  
# Creamos la columna de Fx
  
  Alisado2<-Tabla_Alisado$Alisado
  head(Alisado2)

  Fx<-vector()
  
  for(i in 1:length(Alisado2)){
    Fx[i]<-sum(Alisado2[i:length(Alisado2)])
  }
  
  head(Fx,30)

  
# Unimos la columna de Fx con la Tabla_Alisado
  Tabla_Alisado<-cbind(Tabla_Alisado,Fx)
  head(Tabla_Alisado)


#######################
# VaR No Parametrico (Alisado Exponencial - 1 dia)
  
# VaR al 95% (1 día)
  tabla1<-Tabla_Alisado[which(Tabla_Alisado$Fx>=0.95),]
  VaRAE_95<-tabla1[length(tabla1$PL),"PL"]

# VaR al 975% (1 día)
  tabla2<-Tabla_Alisado[which(Tabla_Alisado$Fx>=0.975),]
  VaRAE_975<-tabla2[length(tabla2$PL),"PL"]

# VaR al 99% (1 día)
  tabla3<-Tabla_Alisado[which(Tabla_Alisado$Fx>=0.99),]
  VaRAE_99<-tabla3[length(tabla3$PL),"PL"]


VaR_AE1<-cbind(VaRAE_95,VaRAE_975,VaRAE_99) 
colnames(VaR_AE1) <-c("95%","97.5%","99%")


#######################
# Resultados
  
  VaR_AE<-rbind(VaR_AE1,VaR_AE1*30,VaR_AE1*180,VaR_AE1*360)
  rownames(VaR_AE)<-c("1 día", "30 días", "180 días", "360 días")

  kable(VaR_AE,digits =4, caption="Método de Alisado Exponencial")

  
# Aqui ya no se pueden comprobar los resultados con una funcion de densidad pues
# las probabilidades no son uniformes  

# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# RECOPILACION DE RESULTADOS DE LA EMISORA  


  kable(VaR_SH,digits=4,caption = "Método de Simulación Histórica")
  kable(VaR_SM,digist=4, caption="Método de Simulación MonteCarlo")
  kable(VaR_B,digist=4, caption="Método Bootstrapping")
  kable(VaR_AE,digits =4, caption="Método de Alisado Exponencial")
  