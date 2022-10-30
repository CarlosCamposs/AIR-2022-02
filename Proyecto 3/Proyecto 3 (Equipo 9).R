# Administracion Integral de Riesgos
# Proyecto 3

library(TTR)
library(tidyquant)
library(quantmod)


cartera = c("BAC")
getSymbols(cartera,src = "yahoo",from="2019-01-01",to="2022-09-01")
head(BAC,n=3)

