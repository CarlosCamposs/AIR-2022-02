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


VaRBoots95<-data.frame()  
VaRBoots975<-data.frame()
VaRBoots99<-data.frame()
PL_EmisorasBoots<-data.frame()


for (i in 1:10){
  
  # Hacemos un remuestreo del P&L de cada emisora y lo metemos en un dataframe llamado PL_EmisorasBoots  
  # el remuestro de cada emisora se mete en una columna del de dataframe "PL_EmisorasBoots"
  for(k in 1:10){
      for (j in 1:length(PL_Portafolio$WALMEX.MX.Close)){
      
        PL_EmisorasBoots[j,k]<-sample(PL_Portafolio[,k], size=length(PL_Portafolio$WALMEX.MX.Close), replace = TRUE)[j]
      
      }
  }
  
  
  # Calculamos el VaR a diferentes niveles de confianza para cada emisora  
  
  for (j in 1:10){
    VaRBoots95[i,j]<-quantile(PL_EmisorasBoots[,j],0.95)
    VaRBoots975[i,j]<-quantile(PL_EmisorasBoots[,j],0.975)
    VaRBoots99[i,j]<-quantile(PL_EmisorasBoots[,j],0.99)
  }

  
}


# El VaR por simulacion Bootstrap de cada emisora es el promedio de cada columna

# En la primera columna colocamos los VaR de cada emisora al 95%
# En la segunda columna colocamos los VaR de cada emisora al 97.5%
# En la tecera columna colocamos los VaR de cada emisora al 99%

VaR_Boots1<-data.frame()

for (j in 1:10){
  VaR_Boots1[j,1]<- colMeans(VaRBoots95)[j]
  VaR_Boots1[j,2]<- colMeans(VaRBoots975)[j]
  VaR_Boots1[j,3]<- colMeans(VaRBoots99)[j]
  
}
colnames(VaR_Boots1)<-c("95%","97.5%","99%")
rownames(VaR_Boots1)<-cartera 


# -------------------------------------------------------------------------------
# Resultados

# Hacemos la suma por columnas para obtener el VaR No Diversificado - 1 dia
VaR_Boots1["VaR No Diversificado",]<-colSums(VaR_Boots1)
View(VaR_Boots1)


# Obtenemos el VaR No Diversificado 30 - dias
VaR_Boots30<-VaR_Boots1*30
View(VaR_Boots30)

# Obtenemos el VaR No Diversificado 180 - dias
VaR_Boots180<-VaR_Boots1*180
View(VaR_Boots180)

# Obtenemos el VaR No Diversificado 360 - dias
VaR_Boots360<-VaR_Boots1*360
View(VaR_Boots360)
# -------------------------------------------------------------------------------

############################################################################################
# Simulacion con Alisado Exponencial

View(PL_Portafolio)



# Definimos los coeficientes alpha y beta
  a<-0.95
  b<-0.05

  
# Creamos la función de Alisado Exponencial de abajo hacia arriba
  
  limite<-length(PL_Portafolio$WALMEX.MX.Close)-1 # Indicador
  Alisado<-vector()
  
  for(k in 0:limite){
    
    Alisado[length(PL_Portafolio$WALMEX.MX.Close)-k]<-a^(k)*b
    
  }

  
# Unimos en una tabla la P&L y el Alisado
  Tabla_Alisado<-cbind(PL_Portafolio,Alisado)
  colnames(Tabla_Alisado)<-c(cartera,"Alisado")
  head(Tabla_Alisado)
  
  # Exportar Tabla_Alisado
  #  library(openxlsx)
  #  mydata <- write.xlsx(Tabla_Alisado,".xlsx")
  #  saveWorkbook(mydata, file = "C:/Users/Carlos Campos/Desktop/GitHub/AIR-2022-02/Proyecto 3/Tabla_Alisado.xlsx", overwrite = TRUE)
  

# Creamos una tabla para cada emisora donde se ordenen los valores de su P&L

# ---------------------------------------------------------------------------------
  # WALMEX
  Tabla_Alisado1<-Tabla_Alisado[,c(1,11)]
  Tabla_Alisado1 <- Tabla_Alisado1[with(Tabla_Alisado1, order(-Tabla_Alisado1$WALMEX.MX)), ] 
  
  # Creamos la columna de Fx
  Alisado1<-Tabla_Alisado1$Alisado
  
  Fx<-vector()
  
  for(i in 1:length(Alisado1)){
    Fx[i]<-sum(Alisado1[i:length(Alisado1)])
  }
  
  head(Fx,30)
  
# Unimos la columna de Fx con la Tabla_Alisado
  Tabla_Alisado1<-cbind(Tabla_Alisado1,Fx)

  # VaR al 95% (1 día)
  tabla_emisora1_95<-Tabla_Alisado1[which(Tabla_Alisado1$Fx>=0.95),] # Guarda todos los valores mayores a 0.95
  VaRAE_95EMISORA1<-tabla_emisora1_95[length(tabla_emisora1_95$Fx),1] # Se toma el primer valor que sea mayor a 0.95
  
  # VaR al 975% (1 día)
  tabla_emisora1_975<-Tabla_Alisado1[which(Tabla_Alisado1$Fx>=0.975),]
  VaRAE_975EMISORA1<-tabla_emisora1_975[length(tabla_emisora1_975$Fx),1]
  
  
  # VaR al 99% (1 día)
  tabla_emisora1_99<-Tabla_Alisado1[which(Tabla_Alisado1$Fx>=0.99),]
  VaRAE_99EMISORA1<-tabla_emisora1_99[length(tabla_emisora1_99$Fx),1]
  
# Resultados - VaR 1 dia de WalMex  
  VaRAE_emisora1<-cbind(VaRAE_95EMISORA1,VaRAE_975EMISORA1,VaRAE_99EMISORA1) 
  colnames(VaRAE_emisora1) <-c("95%","97.5%","99%")  
  VaRAE_emisora1
  
# ---------------------------------------------------------------------------------  
  
# ---------------------------------------------------------------------------------
  # AMXL
  Tabla_Alisado2<-Tabla_Alisado[,c(2,11)]
  Tabla_Alisado2 <- Tabla_Alisado2[with(Tabla_Alisado2, order(-Tabla_Alisado2$AMXL.MX)), ] 
  
  # Creamos la columna de Fx
  Alisado2<-Tabla_Alisado2$Alisado
  
  Fx<-vector()
  
  for(i in 1:length(Alisado2)){
    Fx[i]<-sum(Alisado2[i:length(Alisado2)])
  }
  
  head(Fx,30)
  
  # Unimos la columna de Fx con la Tabla_Alisado
  Tabla_Alisado2<-cbind(Tabla_Alisado2,Fx)
  
  # VaR al 95% (1 día)
  tabla_emisora2_95<-Tabla_Alisado2[which(Tabla_Alisado2$Fx>=0.95),] # Guarda todos los valores mayores a 0.95
  VaRAE_95EMISORA2<-tabla_emisora2_95[length(tabla_emisora2_95$Fx),1] # Se toma el primer valor que sea mayor a 0.95
  
  # VaR al 975% (1 día)
  tabla_emisora2_975<-Tabla_Alisado2[which(Tabla_Alisado2$Fx>=0.975),]
  VaRAE_975EMISORA2<-tabla_emisora2_975[length(tabla_emisora2_975$Fx),1]
  
  
  # VaR al 99% (1 día)
  tabla_emisora2_99<-Tabla_Alisado2[which(Tabla_Alisado2$Fx>=0.99),]
  VaRAE_99EMISORA2<-tabla_emisora2_99[length(tabla_emisora2_99$Fx),1]
  
  # Resultados - VaR 1 dia de WalMex  
  VaRAE_emisora2<-cbind(VaRAE_95EMISORA2,VaRAE_975EMISORA2,VaRAE_99EMISORA2) 
  colnames(VaRAE_emisora2) <-c("95%","97.5%","99%")  
  VaRAE_emisora2
  
# ---------------------------------------------------------------------------------    