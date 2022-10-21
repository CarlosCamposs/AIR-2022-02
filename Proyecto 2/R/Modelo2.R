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

# ////////////////////////////////////////////////////////////////////////////////
# Este script considera una nueva base de datos con 12 variables macroeconómicas
# ////////////////////////////////////////////////////////////////////////////////


# Cargamos los datos
setwd("C:/Users/Carlos Campos/Desktop/GitHub/AIR-2022-02/Proyecto 2/R")
database<-read.csv("database (2).csv",header=TRUE)


# Eliminamos la columna de fecha y a partir del registro 50 empiezan TODAS las variables
# macro a tomar valores
datos<-database[50:nrow(database),c(-1)]

# Verificamos que todas las variables sean numéricas
attach(datos)
class(td)
class(ln_inpp)

# Las variables anteriores las convertimos en "numeric" para poder realizar el modelo
datos$td<-as.numeric(datos$td)
datos$ln_inpp<-as.numeric(datos$ln_inpp)


# ////////////////////////////////////////////////////////////////////
# GRAFICO DE CORRELACION

correlacion<-round(cor(datos), 2)
corrplot(correlacion, method="number", type="upper")  


library("PerformanceAnalytics")
chart.Correlation(datos, histogram=F, pch=19)



# ////////////////////////////////////////////////////////////////////
# TBM
pd_tbm <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "TBM")
pd_tbm<-pd_tbm[50:nrow(database),]


############################################################# EMPRESAS
tbm_empresas<-pd_tbm$Empresas

score_empresasTBM<-vector()
score_empresasTBM<-log(tbm_empresas/(1-tbm_empresas))

modelo1<-lm(score_empresasTBM~.,data=datos)
summary(modelo1)

#////////////////////////////////////
attach(datos)
prueba1<-lm(score_empresasTBM~., data=datos)
step(prueba1,direction="both",trace=0)

# Modelo propuesto
prueba1_mod<-lm(score_empresasTBM~tasa_cetes+pib+td+ln_ipc+ln_actvindustrial+ln_inpp, data=datos)
summary(prueba1_mod)
#////////////////////////////////////


############################################################# CONSUMO
tbm_consumo<-pd_tbm$Consumo

score_consumoTBM<-vector()
score_consumoTBM<-log(tbm_consumo/(1-tbm_consumo))


modelo2<-lm(score_consumoTBM~.,data=datos)
summary(modelo2)

#////////////////////////////////////
prueba2<-lm(score_consumoTBM~., data=datos)
step(prueba2,direction="both",trace=0)

# El mejor modelo:
prueba2_mod<-lm(score_consumoTBM~inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp+ahorro, data=datos)
summary(prueba2_mod)
#////////////////////////////////////


############################################################# VIVIENDA
tbm_vivienda<-pd_tbm$Vivienda

score_viviendaTBM<-vector()
score_viviendaTBM<-log(tbm_vivienda/(1-tbm_vivienda))

modelo3<-lm(score_viviendaTBM~.,data=datos)
summary(modelo3)

#////////////////////////////////////
attach(datos)
prueba3<-lm(score_viviendaTBM~., data=datos)
step(prueba3,direction="both",trace=0)

# Modelo propuesto
prueba3_mod<-lm(score_viviendaTBM~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_inpp, data=datos)
summary(prueba3_mod)
#////////////////////////////////////



# ////////////////////////////////////////////////////////////////////
# SANTANDER
pd_santdr <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "Santander")
pd_santdr<-pd_santdr[50:nrow(database),]


############################################################# EMPRESAS
santdr_empresas<-pd_santdr$Empresas

score_empresasSANTDR<-vector()
score_empresasSANTDR<-log(santdr_empresas/(1-santdr_empresas))

modelo4<-lm(score_empresasSANTDR~.,data=datos)
summary(modelo4)


# ////////////////////////////////////
attach(datos)
prueba4<-lm(score_empresasSANTDR~., data=datos)
step(prueba4,direction="both",trace=0)

# Modelo propuesto
prueba4_mod<-lm(score_empresasSANTDR~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp+ahorro+ln_export+ln_import+inversion, data=datos)
summary(prueba4_mod)
# ////////////////////////////////////



############################################################# CONSUMO
santdr_consumo<-pd_santdr$Consumo

score_consumoSANTDR<-vector()
score_consumoSANTDR<-log(santdr_consumo/(1-santdr_consumo))

modelo5<-lm(score_consumoSANTDR~.,data=datos)
summary(modelo5)

#////////////////////////////////////
attach(datos)
prueba5<-lm(score_consumoSANTDR~., data=datos)
step(prueba5,direction="both",trace=0)

# Modelo propuesto
prueba5_mod<-lm(score_consumoSANTDR~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp+ln_import+inversion, data=datos)
summary(prueba5_mod)
#////////////////////////////////////


############################################################# VIVIENDA
santdr_vivienda<-pd_santdr$Vivienda

score_viviendaSANTDR<-vector()
score_viviendaSANTDR<-log(santdr_vivienda/(1-santdr_vivienda))

modelo6<-lm(score_viviendaSANTDR~.,data=datos)
summary(modelo6)


#////////////////////////////////////
attach(datos)
prueba6<-lm(score_viviendaSANTDR~., data=datos)
step(prueba6,direction="both",trace=0)

# Modelo propuesto
prueba6_mod<-lm(score_viviendaSANTDR~tasa_cetes+pib+ln_tc+td+ln_ipc+ahorro, data=datos)
summary(prueba6_mod)
#////////////////////////////////////











