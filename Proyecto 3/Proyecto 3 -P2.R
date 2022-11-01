# Administracion Integral de Riesgos
# Proyecto 3 - Parte 2

library(TTR)
library(tidyquant)
library(quantmod)
library(knitr) #for kable()


#######################
# Cargamos los datos
cartera = c("MSFT","TSLA","AMZN","AAPL","NVDA","BAC","WALMEX.MX","NFLX","BIMBOA.MX","ELEKTRA.MX")
getSymbols(cartera,src = "yahoo",from="2018-01-01",to="2022-09-01")


#######################
# Obtenemos los precios de cierre de las emisoras

precios1<-MSFT$MSFT.Close
precios2<-TSLA$TSLA.Close
precios3<-AMZN$AMZN.Close
precios4<-AAPL$AAPL.Close
# Hacemos el codigo solo para cuatro acciones

precios5<-NVDA$NVDA.Close
precios6<-BAC$BAC.Close
precios7<-WALMEX.MX$WALMEX.MX.Close
precios8<-NFLX$NFLX.Close
precios9<-BIMBOA.MX$BIMBOA.MX.Close
precios10<-ELEKTRA.MX$ELEKTRA.MX.Close

# Unimos todos los precios de cierre en una tabla
  tabla_precios<-cbind(precios1,precios2,precios3,precios4)

# Declaramos como dataframe a la tabla recien creada
  tabla_precios<-as.data.frame(tabla_precios)

  
#######################
# Rendimientos - Portafolio
  
tabla_rendimientos<-data.frame()
  for(i in 1:4){

      for (j in 1:length(tabla_precios$MSFT.Close)){
        tabla_rendimientos[j,i]<-tabla_precios[j+1,i]/tabla_precios[j,i]-1
      }  
  }

#######################
# Ultimo precio - Portafolio  
  ultimo_precio<-tabla_precios[c(length(tabla_precios$MSFT.Close)),]
  ultimo_precio<-as.data.frame(ultimo_precio)

#######################
# Revaluacion - Portafolio

tabla_revaluacion<-data.frame()
  for(i in 1:4){
  
      for( j in 1:length(tabla_rendimientos$V1)){
        tabla_revaluacion[j,i]<-ultimo_precio[,i]*(1+tabla_rendimientos[j,i])
      } 

  }

#######################
# P&L individual

  tabla_revaluacion<-tabla_revaluacion[c(-length(tabla_revaluacion$V1)),]
    #  debe haber 1174

  
PL1<-data.frame()
PL2<-data.frame()
PL3<-data.frame()
PL4<-data.frame()


  for (j in 1:length(tabla_revaluacion$V1)){
      PL1[j,1]<-ultimo_precio[1]-tabla_revaluacion[j,1]
      PL2[j,1]<-ultimo_precio[2]-tabla_revaluacion[j,2]
      PL3[j,1]<-ultimo_precio[3]-tabla_revaluacion[j,3]
      PL4[j,1]<-ultimo_precio[4]-tabla_revaluacion[j,4]

      PL_Portafolio<-cbind(PL1,PL2,PL3,PL4) # Los unimos en una tabla
  }

head(PL_Portafolio,5)

#######################
# P&L - Portafolio

# Creamos una nueva columna en PL_Portafolio donde sea la P&L del portafolio (la suma)
  
  for(i in 1:length(PL_Portafolio$MSFT.Close)){
    PL_Portafolio$PL[i]<-rowSums(PL_Portafolio[i,])
  }


head(PL_Portafolio,5)


# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Simulacion Historica - Portafolio


#######################
# VaR - Portafolio (Simulacion Historica - 1 día)
alpha<-c(0.95,0.975,0.99)
VaR_SH1<-quantile(PL_Portafolio$PL,probs = alpha)


#######################
#Resultados

VaR_SH<-rbind(VaR_SH1,VaR_SH1*30,VaR_SH1*180,VaR_SH1*360)
rownames(VaR_SH)<-c("1 dia","30 dias","180 dias", "360 dias")

kable(VaR_SH,digits=4,caption = "Método de Simulación Histórica - Portafolio")


# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Simulacion Montecarlo - Portafolio


# Calculamos las medias y sd de cada columna de los rendimientos de cada emisora
means<-colMeans(tabla_rendimientos) 

sd<-c(sd(tabla_rendimientos$V1),sd(tabla_rendimientos$V2),
      sd(tabla_rendimientos$V3),sd(tabla_rendimientos$V4))


#######################
# Simulacion de rendimientos (normal)
  tabla_rendimientos$R1<-rnorm(length(tabla_rendimientos$V1),mean = means[1],sd = sd[1])
  tabla_rendimientos$R2<-rnorm(length(tabla_rendimientos$V1),mean = means[2],sd = sd[2])
  tabla_rendimientos$R3<-rnorm(length(tabla_rendimientos$V1),mean = means[3],sd = sd[3])
  tabla_rendimientos$R4<-rnorm(length(tabla_rendimientos$V1),mean = means[4],sd = sd[4])

    
#######################
# Revaluacion
  ultimo_precio

tabla_revaluacionSM<-data.frame()
  for(i in 1:4){
  
      for( j in 1:length(tabla_rendimientos$V1)){
        tabla_revaluacionSM[j,i]<-ultimo_precio[,i]*(1+tabla_rendimientos[j,i+4])
      }
  }


#######################
# P&L indivual
  PL1_SM<-data.frame()
  PL2_SM<-data.frame()
  PL3_SM<-data.frame()
  PL4_SM<-data.frame()

  for (j in 1:length(tabla_revaluacionSM$V1)){
    PL1_SM[j,1]<-ultimo_precio[1]-tabla_revaluacionSM[j,1]
    PL2_SM[j,1]<-ultimo_precio[2]-tabla_revaluacionSM[j,2]
    PL3_SM[j,1]<-ultimo_precio[3]-tabla_revaluacionSM[j,3]
    PL4_SM[j,1]<-ultimo_precio[4]-tabla_revaluacionSM[j,4]
  
    PL_PortafolioSM<-cbind(PL1_SM,PL2_SM,PL3_SM,PL4_SM)
  }


#######################
# P&L del Portafolio

PL_SM<-data.frame()

for(i in 1:length(PL_PortafolioSM$MSFT.Close)){
  PL_SM[i,1]<-rowSums(PL_PortafolioSM[i,])
}

head(PL_SM,5)


#######################
# VaR - Portafolio (Simulacion Montecarlo - 1 día)
alpha<-c(0.95,0.975,0.99)
VaR_SM1<-quantile(PL_SM$V1,probs=alpha) 
  

#######################
# Resultados

VaR_SM<-rbind(VaR_SM1,VaR_SM1*30,VaR_SM1*180,VaR_SM1*360)
rownames(VaR_SM)<-c("1 dia","30 dias","180 dias", "360 dias")

kable(VaR_SM,digits=4,caption = "Método de Simulación de Montecarlo- Portafolio")


# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Simulacion Bootstrapping - Portafolio
        # Hacemos el remuestreo con la P&L del portafolio

head(PL_Portafolio,5)


VaRB95<-vector()  
VaRB975<-vector()  
VaRB99<-vector()  


for(i in 1:5000){
  
  # Remuestreo con reemplazo de la P&L del portafolio
  remuestreoBoots<-sample(PL_Portafolio$PL, size=length(PL_Portafolio$PL), replace = TRUE)
  
  # VaR a diferentes niveles de confianza  
  VaR_B95<-quantile(remuestreoBoots,0.95)
  VaR_B975<-quantile(remuestreoBoots,0.975)
  VaR_B99<-quantile(remuestreoBoots,0.99)
  
  # Guardamos los VaR obtenidos del remuestreo en un vector
  VaRB95[i]<-VaR_B95  
  VaRB975[i]<-VaR_B975  
  VaRB99[i]<-VaR_B99  
  
}

#######################
# VaR - Portafolio (Bootstrapping - 1 día)

# Sacamos la media
  VaRBoots95<-mean(VaRB95)
  VaRBoots975<-mean(VaRB975)
  VaRBoots99<-mean(VaRB99)

  VaR_B1<-cbind(VaRBoots95,VaRBoots975,VaRBoots99)
  colnames(VaR_B1)<-c("95%","97.5%","99%")
  
#######################
# Resultados  

  VaR_B<-rbind(VaR_B1,VaR_B1*30,VaR_B1*180,VaR_B1*360)
  rownames(VaR_B)<-c("1 dia","30 dias","180 dias", "360 dias")

  kable(VaR_B,digits=4,caption = "Método de Simulación Bootstrapping- Portafolio")


# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Simulacion con Alisado Exponencial - Portafolio
head(PL_Portafolio,5)


PL_AE<-PL_Portafolio$PL 

# Definimos los coeficientes alpha y beta
  a<-0.95
  b<-0.05


# Creamos la función de Alisado Exponencial de abajo hacia arriba
  
j<-length(PL_AE)-1 # Indicador
Alisado<-vector()

  for(i in 0:j){
  
    Alisado[length(PL_AE)-i]<-a^(i)*b
  
  }


# Unimos en una tabla la P&L y el Alisado
  Tabla_Alisado<-cbind(PL_AE,Alisado)
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

head(Fx,15)

# Unimos la columna de Fx con la Tabla_Alisado
Tabla_Alisado<-cbind(Tabla_Alisado,Fx)
head(Tabla_Alisado)


#######################
# VaR - Portafolio (Alisado Exponencial - 1 día)

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
  rownames(VaR_AE)<-c("1 dia","30 dias","180 dias", "360 dias")

  kable(VaR_AE,digits=4,caption = "Método con Alisado Exponencial- Portafolio")


# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  











