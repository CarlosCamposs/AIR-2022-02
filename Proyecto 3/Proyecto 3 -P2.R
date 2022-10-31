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


precios1<-MSFT$MSFT.Close
precios2<-TSLA$TSLA.Close
precios3<-AMZN$AMZN.Close
precios4<-AAPL$AAPL.Close
#
precios5<-NVDA$NVDA.Close
precios6<-BAC$BAC.Close
precios7<-WALMEX.MX$WALMEX.MX.Close
precios8<-NFLX$NFLX.Close
precios9<-BIMBOA.MX$BIMBOA.MX.Close
precios10<-ELEKTRA.MX$ELEKTRA.MX.Close

tabla_precios<-cbind(precios1,precios2,precios3,precios4)

tabla_precios<-as.data.frame(tabla_precios)


# Rendimientos - Portafolio
tabla_rendimientos<-data.frame()

for(i in 1:4){

  for (j in 1:length(tabla_precios$MSFT.Close)){
    tabla_rendimientos[j,i]<-tabla_precios[j+1,i]/tabla_precios[j,i]-1
  }  

  
}


# Ultimo precio - Portafolio
ultimo_precio<-tabla_precios[c(length(tabla_precios$MSFT.Close)),]
ultimo_precio<-as.data.frame(ultimo_precio)


# Revaluacion - Portafolio
tabla_revaluacion<-data.frame()
for(i in 1:4){
  
  for( j in 1:length(tabla_rendimientos$V1)){
    tabla_revaluacion[j,i]<-ultimo_precio[,i]*(1+tabla_rendimientos[j,i])
    }

}

# P&L individual
tabla_revaluacion<-tabla_revaluacion[c(-length(tabla_revaluacion$V1)),]


# P&L individual
PL1<-data.frame()
PL2<-data.frame()
PL3<-data.frame()
PL4<-data.frame()


for (j in 1:length(tabla_revaluacion$V1)){
  PL1[j,1]<-ultimo_precio[1]-tabla_revaluacion[j,1]
  PL2[j,1]<-ultimo_precio[2]-tabla_revaluacion[j,2]
  PL3[j,1]<-ultimo_precio[3]-tabla_revaluacion[j,3]
  PL4[j,1]<-ultimo_precio[4]-tabla_revaluacion[j,4]
  
  
  PL_Portafolio<-cbind(PL1,PL2,PL3,PL4)
    }

PL_Portafolio


# P&L - Portafolio

for(i in 1:length(PL_Portafolio$MSFT.Close)){
  PL_Portafolio$PL[i]<-rowSums(PL_Portafolio[i,])
}

head(PL_Portafolio,5)


# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Simulacion Historica - Portafolio


# VaR - Portafolio (Simulacion Historica - 1 día)
alpha<-c(0.95,0.975,0.99)

VaR_SH1<-quantile(PL_Portafolio$PL,probs = alpha)

VaR_SH<-rbind(VaR_SH1,VaR_SH1*30,VaR_SH1*180,VaR_SH1*360)
rownames(VaR_SH)<-c("1 dia","30 dias","180 dias", "360 dias")

kable(VaR_SH,digits=4,caption = "Método de Simulación Histórica - Portafolio")


# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Simulacion Montecarlo - Portafolio

means<-colMeans(tabla_rendimientos) 

sd<-c(sd(tabla_rendimientos$V1),sd(tabla_rendimientos$V2),
      sd(tabla_rendimientos$V3),sd(tabla_rendimientos$V4))
sd

# Simulacion de rendimientos (normal)
  tabla_rendimientos$R1<-rnorm(length(tabla_rendimientos$V1),mean = means[1],sd = sd[1])
  tabla_rendimientos$R2<-rnorm(length(tabla_rendimientos$V1),mean = means[2],sd = sd[2])
  tabla_rendimientos$R3<-rnorm(length(tabla_rendimientos$V1),mean = means[3],sd = sd[3])
  tabla_rendimientos$R4<-rnorm(length(tabla_rendimientos$V1),mean = means[4],sd = sd[4])
  

# Revaluacion
ultimo_precio

tabla_revaluacionSM<-data.frame()
for(i in 1:4){
  
  for( j in 1:length(tabla_rendimientos$V1)){
    tabla_revaluacionSM[j,i]<-ultimo_precio[,i]*(1+tabla_rendimientos[j,i+4])
  }
  
}

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

# P&L de cada accion
PL_PortafolioSM

# P&L del Portafolio

PL_SM<-data.frame()

for(i in 1:length(PL_PortafolioSM$MSFT.Close)){
  PL_SM[i,1]<-rowSums(PL_PortafolioSM[i,])
}


alpha<-c(0.95,0.975,0.99)

# VaR por Simulacion de Montecarlo 
VaR_SM1<-quantile(PL_SM$V1,probs=alpha) # 1 dia
  

VaR_SM<-rbind(VaR_SM1,VaR_SM1*30,VaR_SM1*180,VaR_SM1*360)
rownames(VaR_SM)<-c("1 dia","30 dias","180 dias", "360 dias")

kable(VaR_SM,digits=4,caption = "Método de Simulación de Montecarlo- Portafolio")


# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Simulacion Bootstrapping - Portafolio






