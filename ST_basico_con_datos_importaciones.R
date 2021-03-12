#IMPORTACIONES FOB SEGUN INTENSIDAD TECNOLOGICA INCORPORADA ECUADOR
#SEGUN EL AÑO 2018,2019,2020
install.packages("tseries")
install.packages("astsa")
install.packages("forecast")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("foreign")
install.packages("quantmod")

library(tseries)
library(astsa)
library(forecast)
library(tidyverse)
library(lubridate)
library(foreign)
library(quantmod)

#Series de tiempo
importaciones.ts = ts(importaciones,start = c(2018,1),frequency=12)
importaciones.ts
plot(importaciones.ts, type="o", lty = "dashed",col ="blue",ylab= "total de importaciones", main = "Serie de Tiempo de importaciones del ecuador(millones de dolares)")
d <- decompose(importaciones.ts)
#tendencia
lines(d$trend, lwd = 2, col = 'red')
grid()

#serie logaritmica
serieslog = log(importaciones.ts)
serieslog
plot(serieslog)

#prueba de Dickey-Fuller
adf.test(serieslog,alternative ="stationary")

#Primera diferencia
seriesdif = diff(importaciones.ts)
seriesdif
plot(seriesdif)

#Para probar estacionariedad
adf.test(seriesdif)

#Segunda diferencia
seriesdif2=diff(importaciones.ts,differences=2)
plot(seriesdif2)
adf.test(seriesdif2,alternative = "stationary")

#Para saber cuantas diferencias necesitamos
ndiffs(importaciones.ts)

#
plot(seriesdif2, type="o", lty = "dashed",col ="red", main = "Serie de Tiempo de importaciones")
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)

acf(seriesdif2)
pacf(seriesdif2)
acf(ts(seriesdif2,frequency = 1))
pacf(ts(seriesdif2,frequency =1))

modelo1 = arima(importaciones.ts,order = c(0,1,1))
modelo1
tsdiag(modelo1)

#Para comprobar ruido blanco >0.05

Box.test(residuals(modelo1), type="Ljung-Box")

#errores residuales                
error = residuals(modelo1)
plot(error)

#Nos permite comprender cual se ajusta mejor a nuestra ST
auto.arima(importaciones.ts)

