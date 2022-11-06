# Administracion Integral de Riesgos
# Proyecto 3 - Parte 2

library(TTR)
library(tidyquant)
library(quantmod)
library(knitr) #for kable()
library (dplyr)


#######################
# Cargamos los datos
cartera=c("WALMEX.MX","AMXL.MX","GFNORTEO.MX",
          "GMEXICOB.MX","TLEVISACPO.MX","KIMBERA.MX",
          "GCARSOA1.MX","GAPB.MX","PE&OLES.MX","LABB.MX")

getSymbols(cartera,src = "yahoo",from="2019-01-01",to="2022-09-30")


#######################
# Obtenemos los precios de cierre de las emisoras

precios1<-WALMEX.MX$WALMEX.MX.Close
precios2<-AMXL.MX$AMXL.MX.Close
precios3<-GFNORTEO.MX$GFNORTEO.MX.Close
precios4<-GMEXICOB.MX$GMEXICOB.MX.Close
precios5<-TLEVISACPO.MX$TLEVISACPO.MX.Close
precios6<-KIMBERA.MX$KIMBERA.MX.Close
precios7<-GCARSOA1.MX$GCARSOA1.MX.Close
precios8<-GAPB.MX$GAPB.MX.Close
precios9<-`PE&OLES.MX`$`PE&OLES.MX.Close`
precios10<-LABB.MX$LABB.MX.Close


# Unimos todos los precios de cierre en una tabla
tabla_precios<-cbind(precios1,precios2,precios3,precios4,
                     precios5,precios6,precios7,precios8,
                     precios9,precios10)

# Declaramos como dataframe a la tabla recien creada
tabla_precios<-as.data.frame(tabla_precios)


#######################
# Rendimientos - Portafolio

tabla_rendimientos<-data.frame()
for(i in 1:10){
  
  for (j in 1:length(tabla_precios$WALMEX.MX.Close)){
    tabla_rendimientos[j,i]<-tabla_precios[j+1,i]/tabla_precios[j,i]-1
  }  
} 

# Le quitamos el ultimo renglon porque nos genero un NA en ese renglon
tabla_rendimientos<-tabla_rendimientos[c(-length(tabla_rendimientos$V1)),]  

#######################
# Ultimo precio - Portafolio  
ultimo_precio<-tabla_precios[c(length(tabla_precios$WALMEX.MX.Close)),]
ultimo_precio<-as.data.frame(ultimo_precio)

#######################
# Revaluacion - Portafolio

tabla_revaluacion<-data.frame()
for(i in 1:10){
  
  for( j in 1:length(tabla_rendimientos$V1)){
    tabla_revaluacion[j,i]<-ultimo_precio[,i]*(1+tabla_rendimientos[j,i])
  } 
  
}



#######################
# P&L individual
# Creamos la funcion P&L de cada emisora y las juntamos en un dataframe llamado PL_Portafolio


PL_Portafolio<-data.frame()  


for (i in 1:length(tabla_revaluacion)){
  
  for (j in 1:length(tabla_revaluacion$V1)){
    PL_Portafolio[j,i]<-ultimo_precio[i]-tabla_revaluacion[j,i]
    
  }
  
}  



############################################################################################
# Simulacion Historica

# Obtenemos el VaR-1 día con diferentes niveles de confianza
alpha<-c(0.95,0.975,0.99)
VaR_SH1<-data.frame()


    for(i in 1:10){
      for(j in 1:length(alpha)){
        VaR_SH1[i,j]<-quantile(PL_Portafolio[,i],alpha)[j]
      }
    }

# Le damos nombres a columnas y filas
colnames(VaR_SH1)<-c("95%","97.5%","99%")
rownames(VaR_SH1)<-cartera 


# -------------------------------------------------------------------------------
# Resultados

# Hacemos la suma por columnas para obtener el VaR No Diversificado - 1 dia
VaR_SH1["VaR No Diversificado",]<-colSums(VaR_SH1)
View(VaR_SH1)


# Obtenemos el VaR No Diversificado 30 - dias
VaR_SH30<-VaR_SH1*30
View(VaR_SH30)

# Obtenemos el VaR No Diversificado 180 - dias
VaR_SH180<-VaR_SH1*180
View(VaR_SH180)

# Obtenemos el VaR No Diversificado 360 - dias
VaR_SH360<-VaR_SH1*360
View(VaR_SH360)


# -------------------------------------------------------------------------------

############################################################################################
# Metodo de Simulacion Montecarlo

# Calculamos las medias y sd de cada columna de los rendimientos de cada emisora

means<-colMeans(tabla_rendimientos) 

sd<-c(sd(tabla_rendimientos$V1),sd(tabla_rendimientos$V2),sd(tabla_rendimientos$V3),
      sd(tabla_rendimientos$V4),sd(tabla_rendimientos$V5),sd(tabla_rendimientos$V6),
      sd(tabla_rendimientos$V7),sd(tabla_rendimientos$V8),sd(tabla_rendimientos$V9),
      sd(tabla_rendimientos$V10))


##################
# Simulaciones

VaRSM95<-data.frame()  
VaRSM975<-data.frame()
VaRSM99<-data.frame()

for(i in 1:10){
  
  rendimientos_sim<-data.frame()
  
  for(k in 1:10){
    for(j in 1:length(tabla_rendimientos$V1)){
      rendimientos_sim[j,k]<-rnorm(length(tabla_rendimientos$V1),mean = means[k],sd = sd[k])[j]
    }
    
  } 
  
  
  
  #######################
  # Revaluacion
  # Una vez obtenido los rendimientos simulados, procedemos a calcular la revaluacion para cada emisora
  
  tabla_revaluacionSM<-data.frame()
  for(k in 1:10){
    
    for( j in 1:length(rendimientos_sim$V1)){
      tabla_revaluacionSM[j,k]<-ultimo_precio[,k]*(1+rendimientos_sim[j,k])
    }
    
  }
  
  #######################
  # P&L indivual
  # Construimos la P&L de cada emisora
  PL_EmisorasSM<-data.frame()
  
  for (k in 1:length(tabla_revaluacionSM)){
    
    for(j in 1:length(tabla_revaluacionSM$V1)){
      
      PL_EmisorasSM[j,k]<-ultimo_precio[k]-tabla_revaluacionSM[j,k]
    }
  }


#######################
# VaR - Portafolio (Simulacion Montecarlo - 1 día)
# Para cada emisora, calculamos el VaR y los guardamos en un dataframe, de modo que
# tendremos 10 valores de VaR al 95% de confianza y se guardan todos en una fila
# del dataframe VARSM95 (para los otros dos es analogo)
  
  for(j in 1:10){
    VaRSM95[i,j]<-quantile(PL_EmisorasSM[,j],probs=0.95) 
    VaRSM975[i,j]<-quantile(PL_EmisorasSM[,j],probs=0.975)
    VaRSM99[i,j]<-quantile(PL_EmisorasSM[,j],probs=0.99) 
  }
  
}

# El VaR de MonteCarlo de cada emisora es el promedio de cada columna

# En la primera columna colocamos los VaR de cada emisora al 95%
# En la segunda columna colocamos los VaR de cada emisora al 97.5%
# En la tecera columna colocamos los VaR de cada emisora al 99%

VaR_SM1<-data.frame()

  for (j in 1:10){
    VaR_SM1[j,1]<- colMeans(VaRSM95)[j]
    VaR_SM1[j,2]<- colMeans(VaRSM975)[j]
    VaR_SM1[j,3]<- colMeans(VaRSM99)[j]
    
  }
colnames(VaR_SM1)<-c("95%","97.5%","99%")
rownames(VaR_SM1)<-cartera 

View(VaR_SM1)




# -------------------------------------------------------------------------------
# Resultados

# Hacemos la suma por columnas para obtener el VaR No Diversificado - 1 dia
VaR_SM1["VaR No Diversificado",]<-colSums(VaR_SM1)
View(VaR_SM1)


# Obtenemos el VaR No Diversificado 30 - dias
VaR_SM30<-VaR_SM1*30
View(VaR_SM30)

# Obtenemos el VaR No Diversificado 180 - dias
VaR_SM180<-VaR_SM1*180
View(VaR_SM180)

# Obtenemos el VaR No Diversificado 360 - dias
VaR_SM360<-VaR_SM1*360
View(VaR_SM360)
# -------------------------------------------------------------------------------

############################################################################################
# Metodo de Simulacion Bootstrap




