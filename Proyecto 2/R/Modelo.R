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






# ////////////////////////////////////////////////////////////////////
# SEGUNDO MODELO (sin TD)
