###################################
#DISE?O COMPLETAMENTE ALEATORIZADO#
###################################

#se deben instalar los paquetes
#agricolae
#tseries
#car
#lmtest

#para cargarlos  
library(agricolae)
library(car)
library(tseries)
library(lmtest)

#################################################
#PARA GENERAR LAS CORRIDAS QUE SE VAN A REALIZAR#
#################################################

#ejemplo restaurantes

#tratamientos es un vector columna donde est?n los niveles del factor a 
#estudiar, pueden ser letras o palabras 
tratamientos <-c('R1','R2','R3','R4') 

# rep:  dice cuantos experimentos hay de 
#cada nivel(todos podr?an ser diferentes, con lo cual se genera un dise?o
# desbalanceado)
rep <- c(5,5,5,5)

#genera un dise?o completamente aleatorizado con las condiciones que se dan
# en los argumentos de la funci?n(factor y replicas)
diseño <- design.crd(tratamientos,rep) 
diseño 

#########################################
#PARA LEER LOS DATOS Y GENERAR EL MODELO#
#########################################

#para leer los datos cuando ya se tiene la columna de resultados
#nombre de los datos "tel"
DCA<-read.table(file.choose(),header=TRUE)
DCA
#modelo para el dise?o, tiempo es la variable respuesta y tratamientos es 
#la variable independiente o factor que para este caso est? fijada en 
#4 niveles diferentes
mod1<-aov(tiempo~tratamientos, data=DCA)

###################################
#Resumen del modelo o tabla ANOVA#
###################################

# este se hace para la prueba de hipotesis 
# H0=todas las medias son iguales
# vs Ha=hay al menos dos medias diferentes
#Se rechaza H0 si el valor p<0.05
summary(mod1)

# si se desea verificar con el valor de F tabulado
# aqui 0.05 es el nivel de significacncia, 
#3 es gl del factor
#16 es gl del error
qf(0.05,3,16,lower.tail=F)

#si NO se rechaza H0 entonces el ejercicio llega hasta aqui.
# caso contrario se hacen las comparaciones para las medias

###########################
##EVALUACIÓN DE SUPUESTOS##
###########################

##para la INDEPENDENCIA##
#----------------------#
##PRUEBA GR?FICA##
#  
plot(mod1$residuals, ylab='residuales',xlab='orden de experimentación',  main='Independencia', las=1,pch=20)
lines(mod1$residuals, col='red')

##PRUEBA NUMÉRICA##
# es necesario instalar la libreria tseries 
library(tseries)
#hipítesis
#H0:datos independientes
#Ha: datos NO independiente.
#si valor p < o = a 0.05 entonces rechazamos hip nula
res=residuals(mod1)
x<-factor(sign(res))
runs.test(x)

##para la normalidad##
#--------------------#

##PRUEBA GR?FICA##

require(car)
qqPlot(res, dist="norm", mean=0, sd=1, ylab='residuales', 
       xlab='cuantiles normales', main='Normalidad', las=1)

##PRUEBA NUM?RICA##
#Hip nula: la distribución es normal
#Ha: la distribución NO es normal 
#si valor p < o = a 0.05 entonces rechazamos hip nula
shapiro.test(res)


##para la homogeneidad de varianza##
#----------------------------------#

##PRUEBA GR?FICA##
plot(mod1$residuals,mod1$fitted.values, ylab='valores ajustados',xlab='residuales',pch=20)
#agrega una linea en el medio
abline(h=4.3, lty=2)

##PRUEBA NUM?RICA##
#Hip nula: la varianza es constante 
#Ha: la varianza no es constante
#si valor p < o = a 0.05 entonces rechazamos hip nula
bartlett.test(tiempo~tratamientos, data=DCA)


#para calcular los efectos#
#----------------------#
model.tables(mod1, 'effect')


##grafico de caja y bogote 
boxplot(DCA$tiempo~DCA$tratamientos, horizontal = FALSE, 
        col = c('blue', 'green', 'red', 'orange'),las=1, 
        ylab='Restaurantes',xlab='Tiempo', 
        main='Tiempo de atención por restaurante')

#para calcular las medias
p=model.tables(mod1, "mean")

#para puntos en gráfico horizontal
points(p$tables$tratamientos, 1:4, pch =20, cex=0.8)

#para puntos en gr?fico vertical
points(p$tables$tratamientos, pch =20, cex=1)
#################################
#pruebas de comparaci?n multiple#
#################################

#Fisher (minima diferencia significativa)#
#----------------------------------------#
# en este caso
#DCA$tiempo es la restricci?n de los datos a la columna de respuesta 
#DCA$tratamientos es la restricci?n de los datos al factor
# 16 son los grados de libertad del error(se saca de la tabla ANOVA) 
# 0.390 es la media cuadratica del error (se saca de la tabla ANOVA)
# 0.05 es el nivel de significancia
F<-LSD.test(DCA$tiempo,DCA$tratamientos, 16, 0.390, alpha=0.05, group=TRUE)
F
#gráfico de medias
plot(F)

#lo ultimo que sale,luego de correer este test, muestra en la tercera columna 
# los posibles grupos que se armaron de acuerdo a quienes tiene medias 
#que no difieren(tienen la misma letra)

#Tukey (diferencia significativa honesta)#
#----------------------------------------#
# en este caso 
#DCA$resitencia es la restricci?n de los datos a la columna de respuesta 
#DCA$telares es la restricci?n de los datos al factor
# 16 son los grados de libertad del error(se saca de la tabla ANOVA) 
# 0.390 es la media cuadratica del error (se saca de la tabla ANOVA)
Tukey<-HSD.test(DCA$tiempo, DCA$tratamientos, 16, 0.390, group=TRUE)
Tukey

#lo ultimo que sale,luego de correer este test, muestra en la tercera columna 
# los posibles grupos que se armaron de acuerdo a quienes tiene medias 
#que no difieren(tienen la misma letra)

#Dunca(Rango m?ltiple de Duncan)#
#----------------------------------------#
#DCA$resitencia es la restricci?n de los datos a la columna de respuesta 
#DCA$telares es la restricci?n de los datos al factor
# 16 son los grados de libertad del error(se saca de la tabla ANOVA) 
# 0.390 es la media cuadratica del error (se saca de la tabla ANOVA)
Duncan=duncan.test(DCA$tiempo, DCA$tratamientos,16, 0.390, group=TRUE)
Duncan
plot(Duncan)

