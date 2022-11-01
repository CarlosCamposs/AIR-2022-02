# Administracion Integral de Riesgos
# Proyecto 3

library(TTR)
library(tidyquant)
library(quantmod)
library(knitr) #for kable()


#######################
# Cargamos los datos
  cartera = c("BAC")
  getSymbols(cartera,src = "yahoo",from="2018-01-01",to="2022-09-01")
  head(BAC,n=3)


#######################
# Precios de cierre
  precios<-BAC$BAC.Close
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
# Simulacion Historica
  
# Niveles de confianza
  alpha<-c(0.95,0.975,0.99)
  
# VaR no parametrico (1 dia)
  VaR_SH1<-quantile(PL,probs=alpha)


# Resultado
VaR_SH<-rbind(VaR_SH1,VaR_SH1*30,VaR_SH1*180,VaR_SH1*360)
rownames(VaR_SH)<-c("1 dia","30 dias","180 dias", "360 dias")
  
  kable(VaR_SH,digits=4,caption = "Método de Simulación Histórica")




# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Montecarlo

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
  
  # VaR  
  VaR_SM95<-quantile(PL_SM,0.95)
  VaR_SM975<-quantile(PL_SM,0.975)
  VaR_SM99<-quantile(PL_SM,0.99)
  
VaRSM95[i]<-VaR_SM95  
VaRSM975[i]<-VaR_SM975  
VaRSM99[i]<-VaR_SM99  

}

# VaR Montecarlo (1 dia)
VaR_Montecarlo95<-mean(VaRSM95)
VaR_Montecarlo975<-mean(VaRSM975)
VaR_Montecarlo99<-mean(VaRSM99)

VaR_1<-cbind(VaR_Montecarlo95,VaR_Montecarlo975,VaR_Montecarlo99)
colnames(VaR_1)<-c("95%","97.5%","99%")

# VaR Montecarlo (30 dia)
VaR_30<-VaR_1*30

# VaR Montecarlo (180 dia)
VaR_180<-VaR_1*180

# VaR Montecarlo (360 dia)
VaR_360<-VaR_1*360


# Resultados
VaR_SM<-rbind(VaR_1,VaR_30,VaR_180,VaR_360)
rownames(VaR_SM)<-c("1 dia", "30 dias", "180 dias", "360 dias")

  kable(VaR_SM,digist=4, caption="Método de Simulación MonteCarlo")
  

# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Bootstrapping

# Funcion de P&L
PL<-ultimo_precio-revaluacion


VaRB95<-vector()  
VaRB975<-vector()  
VaRB99<-vector()  


  for(i in 1:5000){
  
    # Sample con replace=T
      remuestreoBoots<-sample(PL, size=length(PL), replace = TRUE)

    # VaR  
      VaR_B95<-quantile(remuestreoBoots,0.95)
      VaR_B975<-quantile(remuestreoBoots,0.975)
      VaR_B99<-quantile(remuestreoBoots,0.99)
  
      VaRB95[i]<-VaR_B95  
      VaRB975[i]<-VaR_B975  
      VaRB99[i]<-VaR_B99  
    }


# VaR Boostrapping (1 dia)
  VaRBoots95<-mean(VaRB95)
  VaRBoots975<-mean(VaRB975)
  VaRBoots99<-mean(VaRB99)

  VaR_1<-cbind(VaRBoots95,VaRBoots975,VaRBoots99)
  colnames(VaR_1)<-c("95%","97.5%","99%")

# VaR Boostrapping (30 dia)
  VaR_30<-VaR_1*30

# VaR Boostrapping (180 dia)
  VaR_180<-VaR_1*180

# VaR Boostrapping (360 dia)
  VaR_360<-VaR_1*360


# Resultados
VaR_B<-rbind(VaR_1,VaR_30,VaR_180,VaR_360)
rownames(VaR_B)<-c("1 dia", "30 dias", "180 dias", "360 dias")

  kable(VaR_B,digist=4, caption="Método Bootstrapping")
  

# //////////////////////////////////////////////////////////////////////////////////  
# //////////////////////////////////////////////////////////////////////////////////  
# Alisado Exponencial

a<-0.95
b<-0.05
PL<-ultimo_precio-revaluacion

# Indicador
j<-length(PL)-1
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
  
  head(Fx,15)

# Unimos la columna de Fx con la Tabla_Alisado
  Tabla_Alisado<-cbind(Tabla_Alisado,Fx)
  head(Tabla_Alisado)


# VaR Alisado Exponencial (1 dia)
  
# VaR al 95% (1 día)
  tabla1<-Tabla_Alisado[which(Tabla_Alisado$Fx>=0.95),]
  VaRAE_95<-tabla1[length(tabla1$PL),"PL"]

# VaR al 975% (1 día)
  tabla2<-Tabla_Alisado[which(Tabla_Alisado$Fx>=0.975),]
  VaRAE_975<-tabla2[length(tabla2$PL),"PL"]

# VaR al 99% (1 día)
  tabla3<-Tabla_Alisado[which(Tabla_Alisado$Fx>=0.99),]
  VaRAE_99<-tabla3[length(tabla3$PL),"PL"]


VaR_1<-cbind(VaRAE_95,VaRAE_975,VaRAE_99) 
colnames(VaR_1) <-c("95%","97.5%","99%")

# VaR Alisado Exponencial (30 dia)
VaR_30<-VaR_1*30

# VaR Alisado Exponencial (180 dia)
VaR_180<-VaR_1*180

# VaR Alisado Exponencial (360 dia)
VaR_360<-VaR_1*360


# Resultados
VaR_AE<-rbind(VaR_1,VaR_30,VaR_180,VaR_360)
rownames(VaR_AE)<-c("1 día", "30 días", "180 días", "360 días")

  kable(VaR_AE,digits =4, caption="Método de Alisado Exponencial")


# //////////////////////////////////////////////////////////////////////////////////  



