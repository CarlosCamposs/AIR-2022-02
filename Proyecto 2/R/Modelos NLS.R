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

prueba1_mod<-lm(score_empresasTBM~tasa_cetes+pib+td+ln_ipc+ln_actvindustrial+ln_inpp, data=datos)
summary(prueba1_mod)

# Modelo con NLS
modeloNLS_1 <- nls(formula =  score_empresasTBMR ~ a+b1*tasa_cetes+b2*pib+b3*td+b4*ln_ipc+b5*ln_actvindustrial+b6*ln_inpp,
                 data = datos, 
                 start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0),
                 lower = c(a = -Inf,b1 = 0,b2 = -Inf, b3 = 0,b4 = -Inf,b5 = -Inf,b6 = 0),
                 upper = c(a = Inf,b1 = Inf,b2 = 0, b3 = Inf,b4 = 0,b5=0,b6=Inf),
                 algorithm = "port") 

# Visualizamos los resultados de los coeficientes
c.modeloNLS <- data.frame(t(coef(modeloNLS))) 
colnames(c.modeloNLS) <- c("(Intercept)",colnames(datos)[c(1,3,4,5,6,9)])
knitr::kable(c.modeloNLS, caption = "Forced Positive Coefficients")



