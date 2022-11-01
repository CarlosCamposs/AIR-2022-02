# Administracion Integral de Riesgos
# Proyecto 3

library(TTR)
library(tidyquant)
library(quantmod)
library(knitr) #for kable()
library(corrplot)

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


# Unimos todos los precios de cierre en una tabla
tabla_precios<-cbind(precios1,precios2,precios3,precios4)

# Declaramos como dataframe a la tabla recien creada
tabla_precios<-as.data.frame(tabla_precios)


#######################
# Rendimientos - Portafolio


# Obtenemos los rendimientos para cada emisora
  tabla_rendimientos<-data.frame()
  for(i in 1:4){
  
    for (j in 1:length(tabla_precios$MSFT.Close)){
      tabla_rendimientos[j,i]<-tabla_precios[j+1,i]/tabla_precios[j,i]-1
    }  
  }


# Corregimos tabla_rendimientos
  tabla_rendimientos<-tabla_rendimientos[c(-length(tabla_rendimientos$V1)),]
    # Nos quedamos con 1154 observaciones
  

#######################
# Ultimo precio - Portafolio  
  ultimo_precio<-tabla_precios[c(length(tabla_precios$MSFT.Close)),]
  ultimo_precio<-as.data.frame(ultimo_precio)


#######################
# Hallamos las medias y sd de los rendimientos de cada activo
  rendimiento_esperado<-colMeans(tabla_rendimientos) 

  volatilidad_d<-c(sd(tabla_rendimientos$V1),sd(tabla_rendimientos$V2),
        sd(tabla_rendimientos$V3),sd(tabla_rendimientos$V4))

  
#######################
# Numero de acciones de cada emisora
  no_acciones<-c(1500,1200,3500,2500)

  
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
  q<-matrix(participacion,nrow=1,ncol=4)

# Calculamos la varianza y desviacion estandar
  varianza<-q%*%matriz%*%t(q)  
  sd_diver<-sqrt(varianza)

# Definimos que sd_diver sea un vector para poder hacer cuentas
    sd_diver<-as.vector(sd_diver)

# Hallamos el VaR diversificado al 95%  
VaR_Diversificado<-F*sum(S)*sd_diver*tiempo1


VaR_Diversificado
VaR_NoDiversificado




