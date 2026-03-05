library(readxl)
datosT <- read_excel("C:/Users/JONATAN/OneDrive/Escritorio/Trabajo evaluativo unidad 7.xlsx", 
                                          sheet = "Punto 1")
View(datosT)

datosT$mediaM = (rowSums(datosT[ , 2:10]))/9
vectorMedias <- c(datosT[,11])

vectorLimInf <- c(datosT[,11]-1.96*(4/3))
vectorLimInf

vectorLimSup <- c(datosT[,11]+1.96*(4/3))
vectorLimSup

datosT$LimInferior = ((rowSums(datosT[ , 2:10]))/9-1.96*(4/3))

datosT$LimSuperior = ((rowSums(datosT[ , 2:10]))/9+1.96*(4/3))

vectorLog <- c()

for(i in 1:100){
  if(vectorMedias[[i]] > vectorLimInf[[i]] | vectorMedias[[i]] < vectorLimSup[[i]] ){
    vectorLog <- c(TRUE)
  }
}
