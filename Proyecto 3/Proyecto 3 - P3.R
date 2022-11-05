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


# Cuantiles
F95<-qnorm(0.95,mean=0,sd=1)
F975<-qnorm(0.975,mean=0,sd=1)
F99<-qnorm(0.99,mean=0,sd=1)

# Tiempo
tiempo1<-sqrt(1)

# VaR - 1 dia al 95% (por cada acción) 
  VaR_95<-F95*S*volatilidad_d*tiempo1
  VaR_NoDiversificado95<-sum(VaR_95)
  VaR_NoDiversificado95 # 1 dia al 95%

# VaR - 1 dia al 97.5% (por cada acción) 
  VaR_975<-F975*S*volatilidad_d*tiempo1
  VaR_NoDiversificado975<-sum(VaR_975)
  VaR_NoDiversificado975 # 1 dia al 97.5%

# VaR - 1 dia al 99% (por cada acción) 
  VaR_99<-F99*S*volatilidad_d*tiempo1
  VaR_NoDiversificado99<-sum(VaR_99)
  VaR_NoDiversificado99 # 1 dia al 99%

  
# ------------------------------------------------------------------------------
# Un resumen de los resultados se muestra mas abajo, con un "kable", a continuación
# se hace el cálculo del VaR No Diversificado del portafolio para diferentes
# horizontes de tiempo
  
# VaR No Diversificado 1 dia con niveles de confianza del 95%, 97.5% y 99%  
    VaR_NoDiv1<-cbind(VaR_NoDiversificado95,VaR_NoDiversificado975,VaR_NoDiversificado99)  
    colnames(VaR_NoDiv1)<-c("95%", "97.5%","99%")
    rownames(VaR_NoDiv1)<-c("1 día")

# VaR No Diversificado 30 dias con niveles de confianza del 95%, 97.5% y 99%  
    VaR_NoDiv30<-cbind(VaR_NoDiversificado95*sqrt(30),VaR_NoDiversificado975*sqrt(30),VaR_NoDiversificado99*sqrt(30))
    rownames(VaR_NoDiv30)<-c("30 días")

# VaR No Diversificado 180 dias con niveles de confianza del 95%, 97.5% y 99%  
    VaR_NoDiv180<-cbind(VaR_NoDiversificado95*sqrt(180),VaR_NoDiversificado975*sqrt(180),VaR_NoDiversificado99*sqrt(180))
    rownames(VaR_NoDiv180)<-c("180 días")

# VaR No Diversificado 360 dias con niveles de confianza del 95%, 97.5% y 99%  
    VaR_NoDiv360<-cbind(VaR_NoDiversificado95*sqrt(360),VaR_NoDiversificado975*sqrt(360),VaR_NoDiversificado99*sqrt(360))
    rownames(VaR_NoDiv360)<-c("360 días")
# ------------------------------------------------------------------------------  
    
    

###################################################################################  
# Resultados

  VaR_NoDiversificado<-rbind(VaR_NoDiv1,VaR_NoDiv30,VaR_NoDiv180,VaR_NoDiv360)
  kable(VaR_NoDiversificado,digits=4, caption = "VaR No Diversificado")
    
    
# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# VaR parametrico - Diversificado

# Cuantiles
F95<-qnorm(0.95,mean=0,sd=1)
F975<-qnorm(0.975,mean=0,sd=1)
F99<-qnorm(0.99,mean=0,sd=1)
tiempo1<-sqrt(1)
  
# Creamos la matriz q y la matriz de varianzas y covarianzas
  matriz<-cov(tabla_rendimientos)
  q<-matrix(participacion,nrow=1,ncol=10)

# Calculamos la varianza y desviacion estandar
  varianza<-q%*%matriz%*%t(q)  
  sd_diver<-sqrt(varianza)

# Definimos que sd_diver sea un vector para poder hacer cuentas
    sd_diver<-as.vector(sd_diver)

# VaR diversificado al 95%  
VaR_Diversificado95<-F95*sum(S)*sd_diver*tiempo1

# VaR diversificado al 97.5%  
VaR_Diversificado975<-F975*sum(S)*sd_diver*tiempo1

# VaR diversificado al 99%  
VaR_Diversificado99<-F99*sum(S)*sd_diver*tiempo1


# ------------------------------------------------------------------------------
# Un resumen de los resultados se muestra mas abajo, con un "kable", a continuación
# se hace el cálculo del VaR Diversificado del portafolio para diferentes
# horizontes de tiempo

# VaR Diversificado 1 dia con niveles de confianza del 95%, 97.5% y 99%  
VaR_Div1<-cbind(VaR_Diversificado95,VaR_Diversificado975,VaR_Diversificado99)
colnames(VaR_Div1)<-c("95%", "97.5%","99%")
rownames(VaR_Div1)<-c("1 día")

# VaR Diversificado 30 dia con niveles de confianza del 95%, 97.5% y 99%  
VaR_Div30<-cbind(VaR_Diversificado95*sqrt(30),VaR_Diversificado975*sqrt(30),VaR_Diversificado99*sqrt(30))
rownames(VaR_Div30)<-c("30 días")

# VaR Diversificado 180 dia con niveles de confianza del 95%, 97.5% y 99%  
VaR_Div180<-cbind(VaR_Diversificado95*sqrt(180),VaR_Diversificado975*sqrt(180),VaR_Diversificado99*sqrt(180))
rownames(VaR_Div180)<-c("180 días")

# VaR Diversificado 360 dia con niveles de confianza del 95%, 97.5% y 99%  
VaR_Div360<-cbind(VaR_Diversificado95*sqrt(360),VaR_Diversificado975*sqrt(360),VaR_Diversificado99*sqrt(360))
rownames(VaR_Div360)<-c("360 días")
# ------------------------------------------------------------------------------  

###################################################################################  
# Resultados

  VaR_Diversificado<-rbind(VaR_Div1,VaR_Div30,VaR_Div180,VaR_Div360)
  kable(VaR_Diversificado,digits=4, caption = "VaR No Diversificado")

  
# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# RESULTADOS

  kable(VaR_NoDiversificado,digits=4, caption = "VaR No Diversificado")
  kable(VaR_Diversificado,digits=4, caption = "VaR Diversificado")




