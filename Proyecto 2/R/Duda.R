
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



# SANTANDER

# Cargamos la base de datos del proxy de la PD de Santander
pd_santdr <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "Santander")

# Nos quedamos solo con las observaciones a partir del renglon 50
pd_santdr<-pd_santdr[50:nrow(database),]

# SANTANDER Vivienda
santdr_vivienda<-pd_santdr$Vivienda

# //////////////////////////////////////////////////////////////////////// MODELO LM

# Sacamos el score
score_viviendaSANTDR<-vector()
score_viviendaSANTDR<-log(santdr_vivienda/(1-santdr_vivienda))


# En el modelo "prueba6" metemos todas las variables que tiene la base de datos (12)
prueba6<-lm(score_viviendaSANTDR~., data=datos)

# Con la función step obtenemos cuáles son las mejores variables para el score de vivienda
step(prueba6,direction="both",trace=0)

# Creamos el modelo que nos arrojó la función step
mejor_modelo<-lm(score_viviendaSANTDR~tasa_cetes+pib+ln_tc+td+ln_ipc+ahorro, data=datos)
summary(mejor_modelo)


# ////////////////////////////////////////////////////////////////////
# Modelo con NLS

# Una vez que hemos determiando qué variables tomar, el siguiente paso es crear el modelo
# considerando las restricciones en los coeficientes, para esto usaremos un modelo no lineal
# NLS = Nonlinear Least Squares


modeloNLS <- nls(formula =  score_viviendaSANTDR ~ a + b1*tasa_cetes +  b2*pib + b3*ln_tc+b4*td+b5*ln_ipc+b6*ahorro,
            data = datos, 
            start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0),
            lower = c(a = -Inf,b1 = 0,b2 = -Inf, b3 = 0,b4 = 0,b5 = -Inf,b6 = -Inf),
            upper = c(a = Inf,b1 = Inf,b2 = 0, b3 = Inf,b4 = Inf,b5=0,b6=0),
            algorithm = "port") 

# Visualizamos los resultados de los coeficientes
c.modeloNLS <- data.frame(t(coef(modeloNLS))) 
colnames(c.modeloNLS) <- c("(Intercept)",colnames(datos)[c(1,3,4,5,6,9)])
knitr::kable(c.modeloNLS, caption = "Forced Positive Coefficients")

summary(modeloNLS)


# Gráfico
  # Para crear el gráfico ponemos: la PD de la cartera de santander y la PD estimada con el modelo NLS

modeloNLS$m$fitted()  #Con este obtenemos los valores estimados
score_viviendaSANTDR

# Creamos las PD
fitted_curve1<-1/(1+exp(-modeloNLS$m$fitted()))
sample_curve<-1/(1+exp(-score_viviendaSANTDR))
x<-1:204

# Creamos el gráfico
plot(x,sample_curve,type="l",col="red",main="NLS")
lines(x,fitted_curve1,col="green")


# Para calcular el R2
RSS.p<-sum(residuals(modeloNLS )^2)
TSS<-sum((score_viviendaSANTDR-mean(score_viviendaSANTDR))^2)
1-(RSS.p/TSS) # Es el R2



# A esto que está abajo ya no le hagas caso we

# ////////////////////////////////////////////////////////////////////
# Modelo con PENALIZED
library(penalized)

modelo_constr <- penalized(score_viviendaSANTDR, ~ tasa_cetes+pib+ln_tc+td+ln_ipc+ahorro, ~1, 
                  lambda1=0, lambda2=0, positive = c(T, F, T,T,F,F), data=datos)

coef(modelo_constr)
        # No respeta una de las constrains impuestas


# Gráfico
modelo_constr@fitted
score_viviendaSANTDR


fitted_curve2<-1/(1+exp(-modelo_constr@fitted))
sample_curve<-1/(1+exp(-score_viviendaSANTDR))
x<-1:204


plot(x,sample_curve,type="l",col="red",main="PENALIZED")
lines(x,fitted_curve2,col="green")



