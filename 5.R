############################
###Regesión lineal simple###
############################

##Ejemplo usando los datos del ejercicio tiempo vs # de cajas

#para leer los datos#

#Si se no se conoce el nombre y se desea buscar entre los archivos 
datos=read.table(file.choose(), header =T)

#para asociar cada nombre a cada columan de datos
attach(datos)

# para ver los datos con los que va a trabajar
datos

#para ver los nombres de las variables en los datos 
names(datos)

#gráfico de dispersión
plot(Cajas,Tiempo, main="Dispersión", las=1, 
     xlab='Numero de cajas', ylab='Eje Y', col='red')
#agrega la linea asociada a un MRLS
abline(lm(Tiempo~Cajas), col='blue')

#para generar el modelo  
#nota: sin cambiar el orden debe ir (respuesta~variable regresora)
mod1=lm(Tiempo~Cajas)

names(mod1)

##VERIFICACIÓN DE SUPUESTOS##

#Para la independencia#

#prueba gráfica#
plot(mod1$residuals, main="Aleatoriedad",las=1, 
     xlab="Orden de corrida de los datos", ylab="Residuos")
lines(mod1$residuals, col="blue") 

#Prueba numÉrica#
library(tseries)
#hipótesis
#H0:datos independientes 
#Ha: datos NO independientes.
#si valor p < 0.05 entonces rechazamos hip nula
res=mod1$residuals
res
x=factor(sign(res))
runs.test(x)

#test de Durbin Watson
library(lmtest)
dwtest(mod1, alternative = "two.sided")

#para la varianza constante
plot(mod1$fitted.values,mod1$residuals, main="Varianza constante",las=1)
abline(h=0.1, lty=2, col="red")

#prueba numérica
#hip?tesis
#H0: varianza constante
#Ha: varianza NO constante.
#si valor p <  0.05 entonces rechazamos hip nula

bartlett.test(Tiempo~Cajas)
#funciona si hay al menos una rélica de cada diferente x 

#otras opciones

#Breusch-Pagan Test(del paquete lmtest)

bptest(mod1)

#Score Test for Non-Constant Error Variance (del paquete car)
library(car)
ncvTest(mod1)

##para la normalidad##

#forma gráfica
library(car)
qqPlot(mod1$residuals,main='Normalidad', las=1)

#forma numérica
#H0= distrbución normal
#Ha no  distribución No normal
#rechazo Ho si Vp < alpha = 0.05
shapiro.test(res)

#Para la prueba de falta de ajuste

#verificación grafica
#con residuales vs valores ajustados 

#hipótesis
#Ho= modelo lineal adecuado
#Ha= modelo lineal no adecuado
#rechazo h0 si vp <alpha
mod.aux=lm(Tiempo~as.factor(Cajas))
anova(mod.aux)
anova(mod1,mod.aux)

#residuales estandarizados#
di=rstandard(mod1)    
abs(di)>3

##INFERENCIA DEL MODELO##
#########################

#Para visualizar un resumen del modelo
summary(mod1)

# valor de la distrbución t con parámetros alpha/2 y n-2
qt(0.025, 13, lower.tail = FALSE)

#para los I.C para los parámetros
confint(mod1, level=0.95)

#para el análisis de varianza
anova(mod1)

#para buscar F de Fisher con parámetros alpha, 1 y n-2
qf(0.05,1,13,lower.tail = FALSE)

#correción de pearson
cor(datos)

#para IC para la respuesta media
predict(mod1,newdata=data.frame(Cajas=c(17)),
        level=0.95,interval="confidence")

#Para I.P para una observación futura
predict(mod1,newdata=data.frame(Cajas=c(17)),
        level=0.95,interval="prediction")
 