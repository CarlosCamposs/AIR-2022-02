# Administracion Integral de Riesgos
# Proyecto 3 - Parte 3

library(TTR)
library(tidyquant)
library(quantmod)
library(knitr) #for kable()
library(corrplot)

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


# Obtenemos los rendimientos para cada emisora
  tabla_rendimientos<-data.frame()
  for(i in 1:10){
  
    for (j in 1:length(tabla_precios$WALMEX.MX.Close)){
      tabla_rendimientos[j,i]<-tabla_precios[j+1,i]/tabla_precios[j,i]-1
    }  
  }


# Corregimos tabla_rendimientos
  tabla_rendimientos<-tabla_rendimientos[c(-length(tabla_rendimientos$V1)),]
    # Nos quedamos con 924 observaciones
  

#######################
# Ultimo precio - Portafolio  
  ultimo_precio<-tabla_precios[c(length(tabla_precios$WALMEX.MX.Close)),]
  ultimo_precio<-as.data.frame(ultimo_precio)

  # Exportar Tabla_Alisado
  #  library(openxlsx)
  #  mydata <- write.xlsx(ultimo_precio,".xlsx")
  #  saveWorkbook(mydata, file = "C:/Users/Carlos Campos/Desktop/GitHub/AIR-2022-02/Proyecto 3/ultimo_precio.xlsx", overwrite = TRUE)
  
  
#######################
# Hallamos las medias y sd de los rendimientos de cada activo
  rendimiento_esperado<-colMeans(tabla_rendimientos) 

  volatilidad_d<-c(sd(tabla_rendimientos$V1),sd(tabla_rendimientos$V2),sd(tabla_rendimientos$V3),
        sd(tabla_rendimientos$V4),sd(tabla_rendimientos$V5),sd(tabla_rendimientos$V6),
        sd(tabla_rendimientos$V7),sd(tabla_rendimientos$V8),sd(tabla_rendimientos$V9),
        sd(tabla_rendimientos$V10))
  
  
#######################
# Numero de acciones de cada emisora
  no_acciones<-c(450,2000,300,450,1400,1200,450,150,200,2500)

  
#######################
# Monto de cada emisora en un vector S
  S<-ultimo_precio*no_acciones

#######################
# Participacion
  participacion<-S/sum(S)
  participacion<-as.numeric(participacion)

  
# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# VaR parametrico - No diversificado

alpha<-0.95
F<-qnorm(alpha,mean=0,sd=1)
tiempo1<-sqrt(1)

# VaR - 1 dia al 95% (por cada acción) 
  VaR<-F*S*volatilidad_d*tiempo1
  
  VaR_NoDiversificado<-sum(VaR)
  VaR_NoDiversificado # 1 dia al 95%

  
# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# VaR parametrico - Diversificado

alpha<-0.95
F<-qnorm(alpha,mean=0,sd=1)
tiempo1<-sqrt(1)
  
# Creamos la matriz q y la matriz de varianzas y covarianzas
  matriz<-cov(tabla_rendimientos)
  q<-matrix(participacion,nrow=1,ncol=10)

# Calculamos la varianza y desviacion estandar
  varianza<-q%*%matriz%*%t(q)  
  sd_diver<-sqrt(varianza)

# Definimos que sd_diver sea un vector para poder hacer cuentas
    sd_diver<-as.vector(sd_diver)

# Hallamos el VaR diversificado al 95%  
VaR_Diversificado<-F*sum(S)*sd_diver*tiempo1

# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# RESULTADOS

VaR_Diversificado
VaR_NoDiversificado




