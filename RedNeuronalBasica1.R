#Installando paquetes
install.packages("neuralnet")
install.packages("readxl")

#Paquetes
library(readxl)
library(neuralnet)

#Establecer el escritorio de trabajo
setwd("C:\Users\Personal\Documents\SeriesTiempo")

#importar base de datos
entrenamiento = read_excel("redn.xlsx",sheet = "Hoja1")
prueba =read_excel("redn.xlsx",sheet = "Hoja2")

#Generar Red Neuronal
rn= neuralnet(infarto~edad+T, data = entrenamiento, hidden = 20, act.fct = "logistic", linear.output = F)

#Mirar red neuronal
plot(rn)

#Prediccion
rnprediccion = compute(rn,prueba)
a = rnprediccion$net.result
sino = ifelse(a>0.5,1,0)
sino
