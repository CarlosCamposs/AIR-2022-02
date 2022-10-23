# Modelos con step y nls

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
library(PerformanceAnalytics)
library(tidyverse)
library(penalized)



# Cargamos la base de datos
setwd("C:/Users/Carlos Campos/Desktop/GitHub/AIR-2022-02/Proyecto 2/R")
database<-read.csv("database (2).csv",header=TRUE)

# Nos quedamos solo con las observaciones donde todas las variables macroeconómicas empiezan
# a tomar valores
datos<-database[50:nrow(database),c(-1)]
attach(datos)

# Convertimos estas dos columnas a tipo numeric para poder correr el modelo
datos$td<-as.numeric(datos$td)
datos$ln_inpp<-as.numeric(datos$ln_inpp)


# ////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////
# TBM
pd_tbm <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "TBM")
pd_tbm<-pd_tbm[50:nrow(database),]


####################################################################### TBM EMPRESAS
tbm_empresas<-pd_tbm$Empresas

# Sacamos el score
score_empresasTBM<-vector()
score_empresasTBM<-log(tbm_empresas/(1-tbm_empresas))

# Modelo lm
modelo1<-lm(score_empresasTBM~.,data=datos)

# Con la función step obtenemos cuáles son las mejores variables para el score de empresas
step(modelo1,direction="both",trace=0)

prueba1_mod<-lm(score_empresasTBM~tasa_cetes+pib+td+ln_ipc+ln_actvindustrial+ln_inpp+ahorro+ln_export+inversion, data=datos)
summary(prueba1_mod) # R2=0.5275

# Modelo con NLS 
modeloNLS_1 <- nls(formula =  score_empresasTBM ~ a+b1*tasa_cetes+b2*pib+b3*td+b4*ln_ipc+b5*ln_actvindustrial+b6*ln_inpp+b7*ahorro+b8*ln_export+b9*inversion,
                 data = datos, 
                 start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7 = 0,b8 = 0,b9 = 0),
                 lower = c(a = -Inf,b1 = 0,b2 = -Inf, b3 = 0,b4 = -Inf,b5 = -Inf,b6 = 0,b7=-Inf,b8=-Inf, b9=-Inf),
                 upper = c(a = Inf,b1 = Inf,b2 = 0, b3 = Inf,b4 = 0,b5=0,b6=Inf,b7=0,b8=0,b9=0),
                 algorithm = "port") 
summary(modeloNLS_1)


# Visualizamos los resultados de los coeficientes
c.modeloNLS_1 <- data.frame(t(coef(modeloNLS_1))) 
colnames(c.modeloNLS_1) <- c("(Intercept)",colnames(datos)[c(1,3,5,6,7,8,9,10,12)])
knitr::kable(c.modeloNLS_1, caption = "Forced  Coefficients")


# Gráfico
modeloNLS_1$m$fitted()  #Con este obtenemos los valores estimados
score_empresasTBM

# Creamos las PD
fitted_curve1<-1/(1+exp(-modeloNLS_1$m$fitted()))
sample_curve<-1/(1+exp(-score_empresasTBM))
x<-1:204

# Creamos el gráfico
plot(x,sample_curve,type="l",col="red",main="NLS")
lines(x,fitted_curve1,col="green")


# Para calcular el R2
RSS.p<-sum(residuals(modeloNLS_1 )^2)
TSS<-sum((score_empresasTBM-mean(score_empresasTBM))^2)
1-(RSS.p/TSS) # Es el R2, 0.1106


####################################################################### TBM CONSUMO
tbm_consumo<-pd_tbm$Consumo

score_consumoTBM<-vector()
score_consumoTBM<-log(tbm_consumo/(1-tbm_consumo))


modelo2<-lm(score_consumoTBM~.,data=datos)
summary(modelo2) #R2=0.5489

# Con la función step obtenemos cuáles son las mejores variables para el score de empresas
step(modelo2,direction="both",trace=0)

prueba2_mod<-lm(score_consumoTBM~inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp+ahorro, data=datos)
summary(prueba2_mod) # R2=0.5541 

# Modelo con NLS 
modeloNLS_2 <- nls(formula =  score_consumoTBM ~ a+b1*inflacion+b2*pib+b3*ln_tc+b4*td+b5*ln_ipc+b6*ln_actvindustrial+b7*ln_inpp+b8*ahorro,
                   data = datos, 
                   start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7 = 0,b8 = 0),
                   lower = c(a = -Inf,b1 = 0,b2 = -Inf, b3 = 0,b4 = 0,b5 = -Inf,b6 = -Inf,b7=0,b8=-Inf),
                   upper = c(a = Inf,b1 = Inf,b2 = 0, b3 = Inf,b4 = Inf,b5=0,b6=0,b7=Inf,b8=0),
                   algorithm = "port") 

# Visualizamos los resultados de los coeficientes
c.modeloNLS_2 <- data.frame(t(coef(modeloNLS_2))) 
colnames(c.modeloNLS_2) <- c("(Intercept)",colnames(datos)[c(2,3,4,5,6,7,8,9)])
knitr::kable(c.modeloNLS_2, caption = "Forced Coefficients")


# Gráfico
modeloNLS_2$m$fitted()  #Con este obtenemos los valores estimados
score_consumoTBM

# Creamos las PD
fitted_curve2<-1/(1+exp(-modeloNLS_2$m$fitted()))
sample_curve<-1/(1+exp(-score_consumoTBM))
x<-1:204

# Creamos el gráfico
plot(x,sample_curve,type="l",col="red",main="NLS")
lines(x,fitted_curve2,col="green")


# Para calcular el R2
RSS.p<-sum(residuals(modeloNLS_2 )^2)
TSS<-sum((score_consumoTBM-mean(score_consumoTBM))^2)
1-(RSS.p/TSS) # Es el R2, 0.4471


####################################################################### TBM VIVIENDA
tbm_vivienda<-pd_tbm$Vivienda

score_viviendaTBM<-vector()
score_viviendaTBM<-log(tbm_vivienda/(1-tbm_vivienda))

modelo3<-lm(score_viviendaTBM~.,data=datos)
summary(modelo3) # R2 0.8567

step(modelo3,direction="both",trace=0)

prueba3_mod<-lm(score_viviendaTBM~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_inpp, data=datos)
summary(prueba3_mod) # R2=0.858


# Modelo con NLS 
modeloNLS_3 <- nls(formula =  score_viviendaTBM ~ a+b1*tasa_cetes+b2*inflacion+b3*pib+b4*ln_tc+b5*td+b6*ln_ipc+b7*ln_inpp,
                   data = datos, 
                   start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7 = 0),
                   lower = c(a = -Inf,b1 = 0,b2 = 0, b3 = -Inf,b4 = 0,b5 = 0,b6 = -Inf,b7=0),
                   upper = c(a = Inf,b1 = Inf,b2 = Inf, b3 = 0,b4 = Inf,b5=Inf,b6=0,b7=Inf),
                   algorithm = "port") 

# Visualizamos los resultados de los coeficientes
c.modeloNLS_3 <- data.frame(t(coef(modeloNLS_3))) 
colnames(c.modeloNLS_3) <- c("(Intercept)",colnames(datos)[c(1,2,3,4,5,6,8)])
knitr::kable(c.modeloNLS_3, caption = "Forced Coefficients")


### Gráfico

# Creamos las PD
fitted_curve3<-1/(1+exp(-modeloNLS_3$m$fitted()))
sample_curve<-1/(1+exp(-score_viviendaTBM))
x<-1:204

# Creamos el gráfico
plot(x,sample_curve,type="l",col="red",main="NLS")
lines(x,fitted_curve3,col="green")


# Para calcular el R2
RSS.p<-sum(residuals(modeloNLS_3 )^2)
TSS<-sum((score_viviendaTBM-mean(score_viviendaTBM))^2)
1-(RSS.p/TSS) #R2=0.7271


# ////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////
# SANTANDER

pd_santdr <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "Santander")
pd_santdr<-pd_santdr[50:nrow(database),]


############################################################# EMPRESAS
santdr_empresas<-pd_santdr$Empresas

score_empresasSANTDR<-vector()
score_empresasSANTDR<-log(santdr_empresas/(1-santdr_empresas))

modelo4<-lm(score_empresasSANTDR~.,data=datos)

step(modelo4,direction="both",trace=0)

prueba4_mod<-lm(score_empresasSANTDR~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp+ahorro+ln_export+ln_import+inversion, data=datos)
summary(prueba4_mod) # R2=0.8096

# Modelo con NLS 
modeloNLS_4 <- nls(formula =  score_empresasSANTDR ~ a+b1*tasa_cetes+b2*inflacion+b3*pib+b4*ln_tc+b5*td+b6*ln_ipc+b7*ln_actvindustrial+b8*ln_inpp+b9*ahorro+b10*ln_export+b11*ln_import+b12*inversion,
                   data = datos, 
                   start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7 = 0,b8=0,b9=0,b10=0,b11=0,b12=0),
                   lower = c(a = -Inf,b1 = 0,b2 = 0, b3 = -Inf,b4 = 0,b5 = 0,b6 = -Inf,b7=-Inf,b8=0,b9=-Inf,b10=-Inf,b11=-Inf,b12=-Inf),
                   upper = c(a = Inf,b1 = Inf,b2 = Inf, b3 = 0,b4 = Inf,b5=Inf,b6=0,b7=0,b8=Inf,b9=0,b10=0,b11=Inf,b12=0),
                   algorithm = "port") 

# Visualizamos los resultados de los coeficientes
c.modeloNLS_4 <- data.frame(t(coef(modeloNLS_4))) 
colnames(c.modeloNLS_4) <- c("(Intercept)",colnames(datos))
knitr::kable(c.modeloNLS_4, caption = "Forced Coefficients")


### Gráfico

# Creamos las PD
fitted_curve4<-1/(1+exp(-modeloNLS_4$m$fitted()))
sample_curve<-1/(1+exp(-score_empresasSANTDR))
x<-1:204

# Creamos el gráfico
plot(x,sample_curve,type="l",col="red",main="NLS")
lines(x,fitted_curve4,col="green")


# Para calcular el R2
RSS.p<-sum(residuals(modeloNLS_4 )^2)
TSS<-sum((score_empresasSANTDR-mean(score_empresasSANTDR))^2)
1-(RSS.p/TSS) #R2=0.6679


############################################################# CONSUMO

santdr_consumo<-pd_santdr$Consumo

score_consumoSANTDR<-vector()
score_consumoSANTDR<-log(santdr_consumo/(1-santdr_consumo))

modelo5<-lm(score_consumoSANTDR~.,data=datos)
step(modelo5,direction="both",trace=0)

# Modelo propuesto
prueba5_mod<-lm(score_consumoSANTDR~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc+ln_actvindustrial+ln_inpp+ln_import+inversion, data=datos)
summary(prueba5_mod) #R2=0.5182

# Modelo con NLS 
modeloNLS_5 <- nls(formula =  score_consumoSANTDR ~ a+b1*tasa_cetes+b2*inflacion+b3*pib+b4*ln_tc+b5*td+b6*ln_ipc+b7*ln_actvindustrial+b8*ln_inpp+b9*ln_import+b10*inversion,
                   data = datos, 
                   start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7 = 0,b8=0,b9=0,b10=0),
                   lower = c(a = -Inf,b1 = 0,b2 = 0, b3 = -Inf,b4 = 0,b5 = 0,b6 = -Inf,b7=-Inf,b8=0,b9=-Inf,b10=-Inf),
                   upper = c(a = Inf,b1 = Inf,b2 = Inf, b3 = 0,b4 = Inf,b5=Inf,b6=0,b7=0,b8=Inf,b9=Inf,b10=0),
                   algorithm = "port") 

# Visualizamos los resultados de los coeficientes
c.modeloNLS_5 <- data.frame(t(coef(modeloNLS_5))) 
colnames(c.modeloNLS_5) <- c("(Intercept)",colnames(datos)[c(1,2,3,4,5,6,7,8,11,12)])
knitr::kable(c.modeloNLS_5, caption = "Forced Coefficients")


### Gráfico

# Creamos las PD
fitted_curve5<-1/(1+exp(-modeloNLS_5$m$fitted()))
sample_curve<-1/(1+exp(-score_consumoSANTDR))
x<-1:204

# Creamos el gráfico
plot(x,sample_curve,type="l",col="red",main="NLS")
lines(x,fitted_curve5,col="green")


# Para calcular el R2
RSS.p<-sum(residuals(modeloNLS_5 )^2)
TSS<-sum((score_consumoSANTDR-mean(score_consumoSANTDR))^2)
1-(RSS.p/TSS) #R2=0.4291

############################################################# VIVIENDA
santdr_vivienda<-pd_santdr$Vivienda

score_viviendaSANTDR<-vector()
score_viviendaSANTDR<-log(santdr_vivienda/(1-santdr_vivienda))

modelo6<-lm(score_viviendaSANTDR~.,data=datos)
step(modelo6,direction="both",trace=0)

# Creamos las PD
fitted_curve6<-1/(1+exp(-modelo6$fitted.values))
sample_curve<-1/(1+exp(-score_viviendaSANTDR))
x<-1:204

# Creamos el gráfico
plot(x,sample_curve,type="l",col="red",main="LM")
lines(x,fitted_curve6,col="green")



# Modelo con las variables de STEP()
modeloNLS_6 <- nls(formula =  score_viviendaSANTDR ~ a + b1*tasa_cetes +  b2*pib + b3*ln_tc+b4*td+b5*ln_ipc+b6*ahorro,
                 data = datos, 
                 start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0),
                 lower = c(a = -Inf,b1 = 0,b2 = -Inf, b3 = 0,b4 = 0,b5 = -Inf,b6 = -Inf),
                 upper = c(a = Inf,b1 = Inf,b2 = 0, b3 = Inf,b4 = Inf,b5=0,b6=0),
                 algorithm = "port") 

# Modleo con todas las variables
modeloNLS_7 <- nls(formula =  score_viviendaSANTDR ~ a+b1*tasa_cetes+b2*inflacion+b3*pib+b4*ln_tc+b5*td+b6*ln_ipc+b7*ln_actvindustrial+b8*ln_inpp+b9*ahorro+b10*ln_export+b11*ln_import+b12*inversion,
                   data = datos, 
                   start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7 = 0,b8=0,b9=0,b10=0,b11=0,b12=0),
                   lower = c(a = -Inf,b1 = 0,b2 = 0, b3 = -Inf,b4 = 0,b5 = 0,b6 = -Inf,b7=-Inf,b8=0,b9=-Inf,b10=-Inf,b11=-Inf,b12=-Inf),
                   upper = c(a = Inf,b1 = Inf,b2 = Inf, b3 = 0,b4 = Inf,b5=Inf,b6=0,b7=0,b8=Inf,b9=0,b10=0,b11=Inf,b12=0),
                   algorithm = "port") 

# MODELO NLS_6
# Visualizamos los resultados de los coeficientes
c.modeloNLS_6 <- data.frame(t(coef(modeloNLS_6))) 
colnames(c.modeloNLS_6) <- c("(Intercept)",colnames(datos)[c(1,3,4,5,6,9)])
knitr::kable(c.modeloNLS_6, caption = "Forced Coefficients")

# Gráfico

# Creamos las PD
fitted_curve6<-1/(1+exp(-modeloNLS_6$m$fitted()))
sample_curve<-1/(1+exp(-score_viviendaSANTDR))
x<-1:204

# Creamos el gráfico
plot(x,sample_curve,type="l",col="red",main="NLS_6")
lines(x,fitted_curve6,col="green")

######

# MODELO NLS_7
# Visualizamos los resultados de los coeficientes
c.modeloNLS_7 <- data.frame(t(coef(modeloNLS_7))) 
colnames(c.modeloNLS_7) <- c("(Intercept)",colnames(datos))
knitr::kable(c.modeloNLS_7, caption = "Forced Coefficients")

summary(modeloNLS_7)
summary(modeloNLS_6)

RSS.p6<-sum(residuals(modeloNLS_6 )^2)
RSS.p6
RSS.p7<-sum(residuals(modeloNLS_7 )^2)
RSS.p7


# Gráfico

# Creamos las PD
fitted_curve7<-1/(1+exp(-modeloNLS_7$m$fitted()))
sample_curve<-1/(1+exp(-score_viviendaSANTDR))
x<-1:204

# Creamos el gráfico
plot(x,sample_curve,type="l",col="red",main="NLS_7")
lines(x,fitted_curve7,col="green")


################3
# Para calcular el R2
RSS.p<-sum(residuals(modeloNLS_6 )^2)
TSS<-sum((score_viviendaSANTDR-mean(score_viviendaSANTDR))^2)
1-(RSS.p/TSS) # R2=0.79994


