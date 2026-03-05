#lee el archivo .txt
datos=read.table(file.choose(), header=T)
#CARGA BASE DE DATOS Y NOMBRES DE VARIABLES EN R
attach(datos)
#muestra datos
datos

#son necesarios 5 paquetes
# es necesario instalarlos antes de cargarlos.

library(car)
library(leaps)
library(mctest)
library(tseries)
library(lmtest)

#AJUSTE DEL MODELO

modelo<-lm(dioxidocarbono~x1+x2+x3+x4+x5)

summary(modelo)

round(confint(modelo, level=0.95),3)

#Anova(modelo) #PRODUCE SUMA DE CUADRADOS SS2

#se debe calcular el siguiente anova, pero para el analisis se usan los
#resulatos de miAnova
anova(modelo) #PRODUCE SUMAS DE CUADRADOS SS1
#PARA OBTENER LA ANOVA DEL MODELO DE RLM CREAMOS LA SIGUIENTE FUNCI?N

miAnova<-function(modeloreg){
  SSq<-unlist(anova(modeloreg)["Sum Sq"])
  k<-length(SSq)-1
  SSR<-sum(SSq[1:k])
  SSE<-SSq[(k+1)]
  MSR<-SSR/k
  df.error<-unlist(anova(modeloreg)["Df"])[k+1]
  MSE<-SSE/df.error
  F0<-MSR/MSE
  VP<-pf(F0,k,df.error,lower.tail=F)
  result<-
    data.frame(SumSq=c(SSR,SSE),Df=c(k,df.error),MeanSq=c(MSR,MSE),F0=c(round(F0,digits=3),' '),
               P.value=c(format(VP,scientific = TRUE,digits=3),' '),row.names =c("Modelo","Error"))
  cat("Tabla ANOVA Modelo de Regresi?n","\n")
  result
}

miAnova(modelo)

qf(0.05,5,44,lower.tail = FALSE)

#TABLA DE TODAS LAS REGRESIONES POSIBLES
allregtable<-function(modeloreg,respuesta){
  t1<-summary(regsubsets(model.matrix(modeloreg)[,-1],respuesta,nbest=20),all.best=TRUE)
  t2<-as.vector(apply(t1$which[,-1],1,sum))
  t3<-apply(t1$which[,-1],1,function(x) as.character(paste(colnames(
    model.matrix(modeloreg)[,-1])[x],collapse=" ")))
  results<-data.frame(NoOfVars=t2,R2=round(t1$rsq,4),adjR2=round(t1$adjr2,4),
                      SSE=round(t1$rss,5),Cp=round(t1$cp,4),
                      MSE=round(t1$rss/(nrow(model.matrix(modeloreg)[,-1])-(t2+1)),5),
                      Variables.in.model=t3)
  results
}
allregtable(modelo,dioxidocarbono)


#CALCULO DE RESIDUALES ESTUDENTIZADOS
restud<-round(rstudent(modelo),4)

##Evaluación de los supuestos##

##varianza constante##

#GR?FICO DE RESIDUALES VS VALORES AJUSTADOS
plot(fitted(modelo),restud, xlab="Valores Ajustados",ylab="Residuales Estudentizados",
     main="HOMOCEDASTICIDAD")
abline(h=0,lty=2,col=2)

#Ho: var cte
# Ha: var No cte
#PRUEBA BREUSCH-PAGAN/ HOMOEDASTICIDAD
#Breusch-Pagan Test(del paquete lmtest)

bptest(modelo)

#otra opción
ncvTest(modelo)

##Normalidad##

#GRÁFICO DE CUANTILES NORMALES

qqPlot(restud, las=1)

#PRUEBA DE NORMALIDAD DE SHAPIRO-WILK
#Ho: normalidad
#Ha: No normalidad
shapiro.test(restud)

##Independencia##

#GRAFICO RESIDUALES VS ORDEN
plot(restud, main="Aleatoriedad", xlab="Orden de corrida de los datos", ylab="Residuos")
lines(restud, col="green") 

#PRUEBA DE INDEPENDENCIA/CORRELACIÓN
#Ho: independencia 
#Ha: No independencia
x<-factor(sign(restud))
runs.test(x)

### OTRA OPCIÓN ###
# Durbin-Watson test
dwtest(modelo, alternative = "two.sided")

#DIAGN?STICOS DE DATOS ATÍPICOS, DE BALANCEO E INFLUENCIALES
t1<-predict(modelo,se.fit=T)
t2<-round(residuals(modelo),4)
t3<-round(cooks.distance(modelo),4)
t4<-round(hatvalues(modelo),4)
t5<-round(dffits(modelo),4)
W=data.frame(dioxidocarbono,yhat=t1$fit,se.yhat=t1$se.fit,residuals=t2,res.estud=restud,Cooks.D=t3,
             hii.value=t4,Dffits=t5)
W
res.stand=rstandard(modelo)
res.stand

##PARA EVALUAR MULTICOLINEALIDAD##
##################################

#MATRIZ DE DISPERSIÓN
plot(datos[,-1])

#MATRIZ DE CORRELACIONES PARA VARIABLES 
cor(datos[,-1])

#otra versión
library(GGally)
ggpairs(datos[,-1], lower = list(continuous = "smooth"),diag = list(continuous = "barDiag"),
        axisLabels = "none")

#CREANDO FUNCI?N PARA EXTRAER COEFICIENTES ESTIMADOS, SUS IC DEL 95%, VIF'S Y COEFICIENTES ESTANDARIZADOS
miscoeficientes=function(modeloreg,datosreg){
  coefi=coef(modeloreg)
  datos2=as.data.frame(scale(datosreg))
  coef.std=c(0,coef(lm(update(formula(modeloreg),~.+0),datos2)))
  limites=confint(modeloreg,level=0.95)
  vifs=c(0,vif(modeloreg))
  resul=data.frame(Estimaci?n=coefi,Limites=limites,Vif=vifs,Coef.Std=coef.std)
  cat("Coeficientes estimados, sus I.C, Vifs y Coeficientes estimados estandarizados","\n")
  resul
}

#CREANDO FUNCION PARA EXTRAER RESULTADOS PARA DIAGN?STICOS DE MULTICOLINEALIDAD
misDiagnostcolin=function(modelo,Inter=T){
  if(Inter==T){
    X=eigprop(modelo)
    resul=data.frame(Val.propio=X$ev,Ind.Cond=X$ci,Pi=X$pi)
    cat("Diagn?sticos Multicolinealidad - Intercepto inclu?do","\n",
        "?ndices de Condici?n y Proporciones de Varianza","\n")
  }
  else{
    X=eigprop(modelo, Inter = F)
    resul=data.frame(Val.propio=X$ev,Ind.Cond=X$ci,Pi=X$pi)
    cat("Diagn?sticos Multicolinealidad - Intercepto ajustado","\n",
        "?ndices de Condici?n y Proporciones de Varianza","\n")
  }
  resul
}

miscoeficientes(modelo,datos)
misDiagnostcolin(modelo)
misDiagnostcolin(modelo,Inter =F)

#intervalo de confianza para los betas
confint(modelo, level=0.95)

#C?LCULO DE ESTAD?STICOS PARA PUNTOS DE PREDICCI?N [h00,y0hat y se(y0hat)]
x01<-c(1,40,75.07,10.07, 62, 185,185)
x02<-c(1,25,60.2,55, 62, 150,185)
xpred<-rbind(x01,x02)
colnames(xpred)<-colnames(model.matrix(modelo))
A<-model.matrix(modelo)
hvalues<-diag(xpred%*%solve(t(A)%*%A)%*%t(xpred))
prednew<-predict(modelo,newdata=data.frame(xpred[,-1]),se.fit=T, level = 0.95, interval = "prediction")
data.frame(h00.value=hvalues,y0hat=prednew$fit,se.y0hat=prednew$se.fit )

