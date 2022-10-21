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
datos<-database[,-c(1)]        

datos<-database[50:nrow(database),-c(1)]        
attach(datos)

ln_actvindustrial[181] # Tiene valor NULL

# La columna de td la convertimos en 'numeric', ya que los valores NULL hicieron
# que R leyera la columna como character
datos$td<-as.numeric(datos$td)
datos$ln_actvindustrial<-as.numeric(datos$ln_actvindustrial)
datos$ln_inpp<-as.numeric(datos$ln_inpp)


class(datos$ln_inpp)
# Creamos un gráfico de correlaciones
correlacion<-round(cor(datos), 2)
corrplot(correlacion, method="number", type="upper")  
        

# Otro tipo de gráfico de correlación
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(datos, histogram=F, pch=19)



# Cargamos los datos de la proxy de PD
pd_tbm <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "TBM")


# Tomamos solo las observaciones correspondientes a 2005 en adelante
pd_tbm<-pd_tbm[50:nrow(database),]



#####################
### TBM (Empresas)

tbm_empresas<-pd_tbm$Empresas

score_empresasTBM<-vector()
score_empresasTBM<-log(tbm_empresas/(1-tbm_empresas))

modelo1<-lm(score_empresasTBM~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp,data=datos)
summary(modelo1)

#////////////////////////////////////
attach(datos)
prueba1<-lm(score_empresasTBM~., data=datos)
step(prueba1,direction="both",trace=0)

# Modelo propuesto
prueba1_mod<-lm(score_empresasTBM~tasa_cetes+pib+td+ln_ipc+ln_actvindustrial+ln_inpp+ahorro+ln_export+inversion, data=datos)
summary(prueba1_mod)
#////////////////////////////////////





b0<-summary(modelo1)$coefficients[1]
b1<-summary(modelo1)$coefficients[2]
b2<-summary(modelo1)$coefficients[3]
b3<-summary(modelo1)$coefficients[4]
b4<-summary(modelo1)$coefficients[5]
b5<-summary(modelo1)$coefficients[6]
b6<-summary(modelo1)$coefficients[7]


fitted_scores<-b0+b1*tasa_cetes+b2*inflacion+b3*pib+b4*ln_tc+b5*td+b6*ln_ipc
fitted_scores
# Existe un comando para encontrar los fitted values


#####################
### TBM (Consumo)

tbm_consumo<-pd_tbm$Consumo

score_consumoTBM<-vector()
score_consumoTBM<-log(tbm_consumo/(1-tbm_consumo))


modelo2<-lm(score_consumoTBM~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp,data=datos)
summary(modelo2)

#////////////////////////////////////
prueba2<-lm(score_consumoTBM~., data=datos)
step(prueba2,direction="both",trace=0)

# El mejor modelo:
prueba2_mod<-lm(score_consumoTBM~inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp, data=datos)
summary(prueba2_mod)
#////////////////////////////////////




#####################
### TBM (Vivienda)

tbm_vivienda<-pd_tbm$Vivienda

score_viviendaTBM<-vector()
score_viviendaTBM<-log(tbm_vivienda/(1-tbm_vivienda))

modelo3<-lm(score_viviendaTBM~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp,data=datos)
summary(modelo3)

#////////////////////////////////////
attach(datos)
prueba3<-lm(score_viviendaTBM~., data=datos)
step(prueba3,direction="both",trace=0)

# Modelo propuesto
prueba3_mod<-lm(score_viviendaTBM~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_inpp, data=datos)
summary(prueba3_mod)
#////////////////////////////////////



#####################
### Santander (Empresas)
pd_santdr <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "Santander")
pd_santdr<-pd_santdr[50:nrow(database),]


santdr_empresas<-pd_santdr$Empresas

score_empresasSANTDR<-vector()
score_empresasSANTDR<-log(santdr_empresas/(1-santdr_empresas))

modelo4<-lm(score_empresasSANTDR~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp,data=datos)
summary(modelo4)


# ////////////////////////////////////
attach(datos)
prueba4<-lm(score_empresasSANTDR~., data=datos)
step(prueba4,direction="both",trace=0)

# Modelo propuesto
prueba4_mod<-lm(score_empresasSANTDR~tasa_cetes+inflacion+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp, data=datos)
summary(prueba4_mod)
# ////////////////////////////////////



#####################
### Santander (Consumo)

santdr_consumo<-pd_santdr$Consumo

score_consumoSANTDR<-vector()
score_consumoSANTDR<-log(santdr_consumo/(1-santdr_consumo))

modelo5<-lm(score_consumoSANTDR~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp,data=datos)
summary(modelo5)

#////////////////////////////////////
attach(datos)
prueba5<-lm(score_consumoSANTDR~., data=datos)
step(prueba5,direction="both",trace=0)

# Modelo propuesto
prueba5_mod<-lm(score_consumoSANTDR~tasa_cetes+inflacion+td+ln_ipc+ln_actvindustrial, data=datos)
summary(prueba5_mod)
#////////////////////////////////////




#####################
### Santander (Vivienda)

santdr_vivienda<-pd_santdr$Vivienda

score_viviendaSANTDR<-vector()
score_viviendaSANTDR<-log(santdr_vivienda/(1-santdr_vivienda))

modelo6<-lm(score_viviendaSANTDR~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp,data=datos)
summary(modelo6)


#////////////////////////////////////
attach(datos)
prueba6<-lm(score_viviendaSANTDR~., data=datos)
step(prueba6,direction="both",trace=0)

# Modelo propuesto
prueba6_mod<-lm(score_viviendaSANTDR~tasa_cetes+inflacion+pib+ln_ipc+ln_actvindustrial+ln_inpp, data=datos)
summary(prueba6_mod)
#////////////////////////////////////




summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)
summary(modelo5)
summary(modelo6)




# ////////////////////////////////////////////////////////////////////
# SEGUNDO MODELO (sin TD)
