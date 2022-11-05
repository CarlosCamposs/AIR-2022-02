# Administracion Integral de Riesgos
# Proyecto 3 - Parte 2

library(TTR)
library(tidyquant)
library(quantmod)
library(knitr) #for kable()


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

ultimo_precio
  
  for (i in 1:length(tabla_revaluacion)){
      
      for (j in 1:length(tabla_revaluacion$V1)){
        PL_Portafolio[j,i]<-ultimo_precio[i]-tabla_revaluacion[j,i]
                
      }

  }  

head(PL_Portafolio,5)  
  
  
#######################
# P&L - Portafolio

# Creamos una nueva columna en PL_Portafolio donde sea la P&L del portafolio (es la suma de las P&L de cada emisora)

PL_Port<-data.frame()
  for(i in 1:length(PL_Portafolio$WALMEX.MX.Close)){
    PL_Port[i,1]<-rowSums(PL_Portafolio[i,])
  }

colnames(PL_Port)<-c("PL")


PL_Portafolio<-cbind(PL_Portafolio,PL_Port)

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

sd<-c(sd(tabla_rendimientos$V1),sd(tabla_rendimientos$V2),sd(tabla_rendimientos$V3),
      sd(tabla_rendimientos$V4),sd(tabla_rendimientos$V5),sd(tabla_rendimientos$V6),
      sd(tabla_rendimientos$V7),sd(tabla_rendimientos$V8),sd(tabla_rendimientos$V9),
      sd(tabla_rendimientos$V10))


#######################
# Simulacion de rendimientos (normal)

# Definimos los vectores donde se guardaran los valores del VaR en cada simulacion que hagamos
VaRSM95<-vector()  
VaRSM975<-vector()  
VaRSM99<-vector() 

  for(i in 1:10){
  
        # Creamos las simulaciones de los rendimientos de cada emisora en nuevas columnas Ri
        tabla_rendimientos$R1<-rnorm(length(tabla_rendimientos$V1),mean = means[1],sd = sd[1])
        tabla_rendimientos$R2<-rnorm(length(tabla_rendimientos$V1),mean = means[2],sd = sd[2])
        tabla_rendimientos$R3<-rnorm(length(tabla_rendimientos$V1),mean = means[3],sd = sd[3])
        tabla_rendimientos$R4<-rnorm(length(tabla_rendimientos$V1),mean = means[4],sd = sd[4])
        tabla_rendimientos$R5<-rnorm(length(tabla_rendimientos$V1),mean = means[5],sd = sd[5])
        tabla_rendimientos$R6<-rnorm(length(tabla_rendimientos$V1),mean = means[6],sd = sd[6])
        tabla_rendimientos$R7<-rnorm(length(tabla_rendimientos$V1),mean = means[7],sd = sd[7])
        tabla_rendimientos$R8<-rnorm(length(tabla_rendimientos$V1),mean = means[8],sd = sd[8])
        tabla_rendimientos$R9<-rnorm(length(tabla_rendimientos$V1),mean = means[9],sd = sd[9])
        tabla_rendimientos$R10<-rnorm(length(tabla_rendimientos$V1),mean = means[10],sd = sd[10])

        #######################
        # Revaluacion
        # Una vez obtenido los rendimientos simulados, procedemos a calcular la revaluacion para cada emisora

        tabla_revaluacionSM<-data.frame()
        for(k in 1:10){
    
          for( j in 1:length(tabla_rendimientos$V1)){
              tabla_revaluacionSM[j,k]<-ultimo_precio[,k]*(1+tabla_rendimientos[j,k+10])
            }
        
        }

        #######################
        # P&L indivual
        # Construimos la P&L de cada emisora
        PL_PortafolioSM<-data.frame()
  
        for (k in 1:length(tabla_revaluacionSM)){
    
          for(j in 1:length(tabla_revaluacionSM$V1)){
      
            PL_PortafolioSM[j,k]<-ultimo_precio[k]-tabla_revaluacionSM[j,k]
          }
        }
  
        #######################
        # P&L del Portafolio
        
        PL_SM<-data.frame()
        
        for(k in 1:length(PL_PortafolioSM$WALMEX.MX.Close)){
          PL_SM[k,1]<-rowSums(PL_PortafolioSM[k,])
        }
        
        head(PL_SM,5)
        
        #######################
        # VaR - Portafolio (Simulacion Montecarlo - 1 día)
        # Para cada tabltia de P&L obtenida, calculamos los VaR y los guardamos en vectores
        
        VaRSM95[i]<-quantile(PL_SM$V1,probs=0.95) 
        VaRSM975[i]<-quantile(PL_SM$V1,probs=0.975)
        VaRSM99[i]<-quantile(PL_SM$V1,probs=0.99) 
        
  
}
  # Calculamos el promedio de cada vector
  VaR_Montecarlo95<-mean(VaRSM95)
  VaR_Montecarlo975<-mean(VaRSM975)
  VaR_Montecarlo99<-mean(VaRSM99)

  # En el vector VaR_SM1 metemos los valores del VaR por metodo de Montecarlo 1 día
  VaR_SM1<-cbind(VaR_Montecarlo95,VaR_Montecarlo975,VaR_Montecarlo99)
  colnames(VaR_SM1)<-c("95%","97.5%","99%")
  

    

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

  VaR_B1<-cbind(mean(VaRB95),mean(VaRB975),mean(VaRB99))
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

# Nos quedamos nada mas con la columna PL
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

# Exportar Tabla_Alisado
#  library(openxlsx)
#  mydata <- write.xlsx(Tabla_Alisado,".xlsx")
#  saveWorkbook(mydata, file = "C:/Users/Carlos Campos/Desktop/GitHub/AIR-2022-02/Proyecto 3/Tabla_Alisado.xlsx", overwrite = TRUE)
  
  
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
# RECOPILACION DE RESULTADOS DEL PORTAFOLIO  
  
  
  kable(VaR_SH,digits=4,caption = "Método de Simulación Histórica - Portafolio")
  kable(VaR_SM,digits=4,caption = "Método de Simulación de Montecarlo- Portafolio")
  kable(VaR_B,digits=4,caption = "Método de Simulación Bootstrapping- Portafolio")
  kable(VaR_AE,digits=4,caption = "Método con Alisado Exponencial- Portafolio")
  








