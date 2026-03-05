#Ejemplo
##datos  dchicken en la libreria astsa
library(astsa)
library(tseries)
library(car)
library(forecast)

#### paso 1: 
#gráfico de la serie
ts.plot(chicken)


#prueba de estacionariedad en media
#Ho=La serie no es estacionaria
#Ha= serie es estacionaria
#Rechazo Ho si vp<alpha
adf.test(chicken)

## operador diff para estabilizar la media
ts.plot((diff(chicken)))
#prueba de estacionariedad
adf.test(diff(chicken))

#gráficos con posibles transformaciones
par(mfrow=c(3,1))
ts.plot(diff(log(chicken)))
ts.plot(diff((chicken)^(1/3)))
ts.plot(diff(sqrt(chicken)))

#definición de la transfomaci?n, en caso de ser necesario 
dato=log(chicken)

#funciones de autocorrelaci?n
acf2(diff(dato))
acf2(diff(dato),221)

#modelo candidato
auto.arima(dato)

#### paso 2: estimaci?n del modelo ####
#modelo arima(1,1,0)=AR(1)
mod1=sarima(dato,1,1,0)
mod1

#modelo arima(0,1,2)=MA(2)
mod2=sarima(dato, 0,1,2)
mod2

#modelo arma(1,2)
mod3=sarima(dato, 1,1,2)
mod3

#modelo arma(1,1)
mod4=sarima(dato, 1,1,1)
mod4

####paso 3: chequeo ####
# para el modelo 1 
#residuales del modelo
res1=mod1$fit$residuals

#gr?fico de normalidad
qqPlot(res1)

#Ho:hay normalidad
#Ha: no hay normalidad
shapiro.test(res1)

#acf para independencia
acf(res1, 221)

# test de rachas
#Ho:aleatoriedad
x<-factor(sign(res2))
runs.test(x)

#otra prueba de aleatoriedad
Box.test(res2, type='Ljung-Box')


### paso 4:pron?stico###
sarima.for(gnp, 10, 1,1,0)


#prueba 
a=gnp[1:210]
b=gnp[211:223]
sarima.for(a, 13, 1,1,0)

