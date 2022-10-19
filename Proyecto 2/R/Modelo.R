# ////////////////////////////////////////////////////////////////////
# Proyecto 2 - Equipo 9
# Administración Integral de Riesgos
 
# Cargamos bibliotecas
library(knitr)
library(xtable)
library(printr)
library(stargazer)
library(rmarkdown)
library(effects)
library(car)
library(AER)
library(broom)
library(stats)
library(lmtest)
library(sandwich)
library(tinytex)
library(openxlsx)
library(readxl)
library(nlWaldTest)
library(forecast)
library(tables)
library(haven)
library(nnet)
library(MCMCpack)
library(sampleSelection)
library(corrplot)


# Cargamos los datos
setwd("C:/Users/Carlos Campos/Desktop/GitHub/AIR-2022-02/Proyecto 2/R")
database<-read.csv("database.csv",header=TRUE)

# A partir de la fila 50 empiezan los registros de TD
database$td[50]


# ////////////////////////////////////////////////////////////////////
# PRIMER MODELO (TBM)

# Restringimos el modelo a partir de 2005, esto se hace para considerar los valores
# de la TD  
datos<-database[50:nrow(database),-c(1)]        


# La columna de td la convertimos en 'numeric', ya que los valores NULL hicieron
# que R leyera la columna como character
datos$td<-as.numeric(datos$td)
datos


# Creamos un gráfico de correlaciones
correlacion<-round(cor(datos), 2)
corrplot(correlacion, method="number", type="upper")  
        

# Otro tipo de gráfico de correlación
#install.packages("PerformanceAnalytics")
#library("PerformanceAnalytics")
#chart.Correlation(datos, histogram=F, pch=19)
# Otro gráfico
#pairs(datos)



# Cargamos los datos de la proxy de PD
pd_tbm <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "TBM")

# Tomamos solo las observaciones correspondientes a 2005 en adelante
pd_tbm<-pd_tbm[50:nrow(database),]

#pd_santdr<-pd_santdr[50:nrow(database),]



#####################
### TBM (Empresas)

tbm_empresas<-pd_tbm$Empresas

score_empresasTBM<-vector()
for (i in 1:nrow(datos)){
  score_empresasTBM[i]<-log(tbm_empresas[i]/(1-tbm_empresas[i]))
}

modelo1<-lm(score_empresasTBM~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc,data=datos)
summary(modelo1)


b0<-summary(modelo1)$coefficients[1]
b1<-summary(modelo1)$coefficients[2]
b2<-summary(modelo1)$coefficients[3]
b3<-summary(modelo1)$coefficients[4]
b4<-summary(modelo1)$coefficients[5]
b5<-summary(modelo1)$coefficients[6]
b6<-summary(modelo1)$coefficients[7]


fitted_scores<-b0+b1*tasa_cetes+b2*inflacion+b3*pib+b4*ln_tc+b5*td+b6*ln_ipc
fitted_scores



#####################
### TBM (Consumo)

tbm_consumo<-pd_tbm$Consumo

score_consumoTBM<-vector()
for (i in 1:nrow(datos)){
  score_consumoTBM[i]<-log(tbm_consumo[i]/(1-tbm_consumo[i]))
}

modelo2<-lm(score_consumoTBM~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc,data=datos)
summary(modelo2)



#####################
### TBM (Vivienda)

tbm_vivienda<-pd_tbm$Vivienda

score_viviendaTBM<-vector()
for (i in 1:nrow(datos)){
  score_viviendaTBM[i]<-log(tbm_vivienda[i]/(1-tbm_vivienda[i]))
}

modelo3<-lm(score_viviendaTBM~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc,data=datos)
summary(modelo3)

#####################
### Santander (Empresas)
pd_santdr <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "Santander")
pd_santdr<-pd_santdr[50:nrow(database),]


santdr_empresas<-pd_santdr$Empresas

score_empresasSANTDR<-vector()
for (i in 1:nrow(datos)){
  score_empresasSANTDR[i]<-log(santdr_empresas[i]/(1-santdr_empresas[i]))
}

modelo4<-lm(score_empresasSANTDR~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc,data=datos)
summary(modelo4)



#####################
### Santander (Consumo)

santdr_consumo<-pd_santdr$Consumo
  # Existen valores NA y 1

# Los valores de "1" los cambiamos por NA
santdr_consumo[which(santdr_consumo==1)] <- NA
santdr_consumo


score_consumoSANTDR<-vector()
for (i in 1:nrow(datos)){
  score_consumoSANTDR[i]<-log(santdr_consumo[i]/(1-santdr_consumo[i]))
}
modelo5<-lm(score_consumoSANTDR~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc,data=datos)
summary(modelo5)
# Elimina 40 observaciones



# Otra alternativa para esos datos 
# Por si quisieramos convertirlos a 1
santdr_consumo[!is.finite(santdr_consumo)] <- 1
santdr_consumo


# Los valores Inf les asignamos valor NA
score_consumoSANTDR[!is.finite(score_consumoSANTDR)] <- NA
score_consumoSANTDR


#####################
### Santander (Vivienda)

santdr_vivienda<-pd_santdr$Vivienda

score_viviendaSANTDR<-vector()
for (i in 1:nrow(datos)){
  score_viviendaSANTDR[i]<-log(santdr_vivienda[i]/(1-santdr_vivienda[i]))
}

modelo6<-lm(score_viviendaSANTDR~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc,data=datos)
summary(modelo6)




summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)
summary(modelo6)




# ////////////////////////////////////////////////////////////////////
# SEGUNDO MODELO (sin TD)
