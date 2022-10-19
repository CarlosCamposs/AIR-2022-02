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
# PRIMER MODELO

# Restringimos el modelo a partir de 2005, esto se hace para considerar los valores
# de la TD  
datos<-database[50:nrow(database),-c(1)]        
class(datos)        



# La columna de td la convertimos en 'numeric', ya que los valores NULL hicieron
# que R leyera la columna como character
datos$td<-as.numeric(datos$td)
datos

class(datos$tasa_cetes)
class(datos$inflacion)
class(datos$pib)
class(datos$ln_tc)
class(datos$td)
class(datos$ln_ipc)


# Creamos un gráfico de correlaciones
correlacion<-round(cor(datos), 2)
corrplot(correlacion, method="number", type="upper")  
        

# Otro tipo de gráfico de correlación
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(datos, histogram=F, pch=19)
# Otro gráfico
pairs(datos)



# Cargamos los datos de la proxy de PD
pd_tbm <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "TBM")
pd_santdr <- read_excel("Proyecto 2 Stress Testing (E9).xlsx", sheet = "Santander")


class(pd_tbm)
class(pd_santdr)

# Tomamos solo las observaciones correspondientes a 2005 en adelante
pd_tbm<-pd_tbm[50:nrow(database),]
pd_santdr<-pd_santdr[50:nrow(database),]
datos<-database[50:nrow(database),-c(1)]        
datos$td<-as.numeric(datos$td)


tbm_empresas<-pd_tbm$Empresas
tbm_empresas[1]


score<-vector()

for (i in 1:nrow(datos)){
  score[i]<-log(tbm_empresas[i]/(1-tbm_empresas[i]))
}

score[1]

attach(datos)
modelo1<-lm(score~tasa_cetes+inflacion+pib+ln_tc+td+ln_ipc,data=datos)
modelo1
anova(modelo1)
b0<-summary(modelo1)$coefficients[1]
b1<-summary(modelo1)$coefficients[2]
b2<-summary(modelo1)$coefficients[3]
b3<-summary(modelo1)$coefficients[4]
b4<-summary(modelo1)$coefficients[5]
b5<-summary(modelo1)$coefficients[6]
b6<-summary(modelo1)$coefficients[7]

b0+b1*tasa_cetes+b2*inflacion+b3*pib+b4*ln_tc+b5*td+b6*ln_ipc
score
# ////////////////////////////////////////////////////////////////////
# SEGUNDO MODELO (sin TD)
