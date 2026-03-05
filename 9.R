############################################
#DISE?O POR BLOQUES COMPLETOS ALEATORIZADO#
############################################

# para cargar los paquetes 
library(car)
library(agricolae)
library(asbio)
library(tseries)
library(lmtest)
# no olvidar cargarlos antes empezar el análisis

#################################################
#PARA GENERAR LAS CORRIDAS QUE SE VAN A REALIZAR#
#################################################

#definir cuales serán los tratamientos y 
#la cantidad de repeticiones(bloques)

#tratamientos es un vector columna donde están los niveles del factor a 
#estudiar, pueden ser letras o palabras
tratamientos <- c('S1','S2','S3') 

# repeticiones es un vector columna que dice cuantos niveles del fator de bloqueo
#hay 
rep <- 5

# crear el diseño DBCA 

#genera un diseño por bloques completo aleatorizado con las condiciones que se dan
# en los argumentos de la función
diseño <- design.rcbd(tratamientos,rep) 

#muestra el orden como quedaron las corridas experimentales 
diseño 

#en la tabla de datos, la columna block debe estar cada renglon 
#con block1....blockn

#########################################
#PARA LEER LOS DATOS Y GENERAR EL MODELO#
#########################################

#para leer los datos cuando ya se tiene la columna de resultados
#nombre de los datos "info"

datos<-read.table(file.choose(),header=TRUE)

attach(datos)

mod1<-aov(tiempo~tratamientos+block, data=datos)

###################################
#Resumen del modelo o tabla ANOVA#
###################################

# este se hace para la prueba de hipotesis H0=todas las medias son iguales
# vs Ha=hay al menos dos medias diferentes
#Se rechaza H0 si el valor p<0.05
#####OJO### REVISAR SI HAY ERROR EN LOS GRADOS DE LIBERTAD
#si hay error, revisar que en la columna block, se encuentre la palabra block 
#pergada al numero correspondiente a cada bloque

summary(mod1)

qf(0.05, 2, 8, lower.tail = FALSE)

# Ho: no valió la pena la inclusión del factor de bloqueo
# Ha: vali? la pena la inclusi?n del factor de bloqueo
#rechazo Ho si vp<alpha o si F_bloque>F_t
qf(0.05,4,8, lower.tail = FALSE)

###########################
##EVALUACI?N DE SUPUESTOS##
###########################

##para la aleatoriedad##
#----------------------#
##PRUEBA NUM?RICA##
# es necesario instalar la libreria tseries 
library(tseries)
#hip?tesis
#H0:datos independientes  
#Ha: datos NO independientes.
#si valor p < o = a 0.05 entonces rechazamos hip nula
res=residuals(mod1)
x<-factor(sign(res))
runs.test(x)

#otra prueba : Test de Durbin-Watson

dwtest(mod1, alternative = "two.sided")

##PRUEBA GR?FICA##
plot(res, main='Aleatoriedad',ylab='residuales', xlab='orden de experimentaci?n', pch=20, las=1)
lines(res, col='red') 

plot(mod1$residuals,ylab='Residuales', las=1, xlab="Orden de experimentaci?n")
lines(mod1$residuals, col="blue")
x<-factor(sign(residuals(mod1)))
legend("topright", bg="white",legend = paste("p-(Rachas):",
                                             round(runs.test(x)$p.value, 4)))

##para la normalidad##
#--------------------#

##PRUEBA NUM?RICA##
#Hip nula: la distribución  es normal
#Ha: la distribuci?n  NO es normal 
#si valor p < o = a 0.05 entonces rechazamos hip nula
shapiro.test (res)

##PRUEBA GR?FICA##
require(car)
qqPlot(res, distribution = 'norm', mean=0, main='Normalidad', las=1, xlab='Cuantiles normales', ylab='Residuales')

qqPlot(residuals(mod1), las=1, xlab='Cuantiles te?ricos', ylab='Residuales', grid = T)
legend("bottomright", bg="white",legend = paste("p-(Shapiro-Wilk):",
                                                round(shapiro.test(mod1$residuals)$p.value, 4)))


##para la homogeneidad de varianza##
#----------------------------------#

##PRUEBA NUM?RICA##
#Hip nula: la varianza es constante 
#Ha: la varianza no es constante
#si valor p < o = a 0.05 entonces rechazamos hip nula

##test de Bartlett##
# Es sensible a la falta de normalidad
bartlett.test(tiempo~tratamientos, data=datos)

##test de Levene## 
#versión del paquete car
require(car)
#Es robusto a la falta de normalidad
leveneTest(tiempo~tratamientos, data=datos)

#versión del paquete asbio para test de levene
require(asbio)
modlevene.test(datos$tiempo, datos$tratamientos)


##PRUEBA GRÁFICA##
plot(mod1$fitted.values,res, xlab='Valores ajustados',ylab='Residuales', pch=20,las=1)
#agrega una linea en el medio
abline(h=0, lty=2)

plot(fitted.values(mod1),residuals(mod1), xlab="Valores ajustados",
     ylab="Residuales", las=1)
legend("topleft",legend =paste("p-(Bartlett-t):",
                               round(bartlett.test(tiempo~tratamientos, data=datos)$p.value, 4)))


#para calcular los efectos#
#----------------------#
model.tables(mod1, 'effect')

##prueba de aditvidad##
#-------------------- #

##PRUEBA GRÁFICA##
plot(mod1$fitted.values,res, xlab='valores ajustados',ylab='residuales', pch=20,las=1)
#agrega una linea en el medio
abline(h=-0.2, lty=2)

#hip nula : el modelo es aditivo o no hay interacción
#si valor p < o = a 0.05 entonces rechazamos hip nula

#prueba de aditividad del paquete asbio
#argumentos (variable respuesta, factor 1, factor 2) 
tukey.add.test(datos$tiempo, datos$tratamientos, datos$block)

#prueba de aditividad del paquete agricolae
#argumentos (variable respuesta, factor 1, factor 2, gl error, MCE) 
nonadditivity(datos$tiempo, datos$tratamientos, datos$block, 8, 0.92) 

#################################
#pruebas de comparación multiple#
#################################

#boxplot
boxplot(datos$tiempo~datos$tratamientos, horizontal =FALSE, 
        col = c('blue', 'green', 'red', 'orange'), xlab='Sistemas',
        ylab='Tiempo', las=1)
p=model.tables(mod1, "mean")

#para puntos en gr?fico vertical
points(p$tables$tratamientos, pch =20, cex=0.8)

#para puntos en gr?fico horizontal
points(p$tables$tratamientos, 1:4, pch =20, cex=0.8)

#Fisher (minima diferencia significativa)#
#----------------------------------------#
# en este caso
#datos$respuesta es la restricci?n de los datos a la columna de respuesta 
# datos$tratamientos es la restricci?n de los datos al factor
# 8 son los grados de libertad del error(se saca de la tabla ANOVA) 
# 0.92 es la media cuadratica del error (se saca de la tabla ANOVA)
# 0.05 es el nivel de significancia
F<-LSD.test(datos$tiempo,datos$tratamientos, 8, 0.92, alpha=0.05, group=TRUE)
F

#lo ultimo que sale,luego de correer este test, muestra en la tercera columna 
# los posibles grupos que se armaron de acuerdo a quienes tiene medias 
#que no difieren(tienen la misma letra)

#Tukey (diferencia significativa honesta)#
#----------------------------------------#
# en este caso 
#DCA$tiempo es la restricci?n de los datos a la columna de respuesta 
#DCA$tratamientos es la restricci?n de los datos al factor
# 8 son los grados de libertad del error(se saca de la tabla ANOVA) 
# 0.92 es la media cuadratica del error (se saca de la tabla ANOVA)
Tukey<-HSD.test(datos$tiempo,datos$tratamientos, 8, 0.92, group = TRUE)
Tukey

#lo ultimo que sale,luego de correer este test, muestra en la tercera columna 
# los posibles grupos que se armaron de acuerdo a quienes tiene medias 
#que no difieren(tienen la misma letra)

#Dunca(Rango m?ltiple de Duncan)#
#----------------------------------------#
#DCA$respuesta es la restricci?n de los datos a la columna de respuesta 
#DCA$tratamientos es la restricci?n de los datos al factor
# 8 son los grados de libertad del error(se saca de la tabla ANOVA) 
# 0.92 es la media cuadratica del error (se saca de la tabla ANOVA)
Duncan=duncan.test(datos$tiempo,datos$tratamientos, 8, 0.92, group=TRUE)
Duncan

