data2 <- read.csv("testing.csv")
#Aplicamos un muestreo aleatorio simple para hacer la validacion de nuestro modelo


#Las siguientes lineas convierten a la variable objetivo en valores booleanos
data2$s<-c(data2$MEDV=="s")
data2$n<-c(data2$MEDV=="n")

#Con la librer�a neuralnet se contruye la red neuronal con el algoritmo de retropropagaci�n
modelo=neuralnet(s + n ~ age	+ sex +	cp + trestbps + chol + restecg + thalach +	exang	+ oldpeak	+ slope	+ ca	+ thal,
                 data2, hidden = 4,
                 lifesign = "full",
                 algorithm = "rprop+")

#Ahora, se va a construir la gr�fica de la red neuronal
plot(modelo, rep = "best")
plot(modelo, rep = "best", intercept = FALSE)

#Ahora se recorre la tabla completa para hacer calculo de as predicciones
prediccion <- compute(modelo, data2[1:12])
prediccion
which.max(prediccion$net.result[1,])
result<-0
for (i in 1:73) {result[i] <- which.max(prediccion$net.result[i,])}
for (i in 1:73) {if (result[i]==1) {result[i] = "s"}}
for (i in 1:73) {if (result[i]==2) {result[i] = "n"}}


#Se hace una comparaci�n de las clases reales con las predicciones
comparacion<-data2
comparacion$predicted<-result
comparacion
result

(MC <- table(data2$MEDV, result))


#Exportar la tabla con la predicciones para sacar tablas de contigencia
setwd("C:/Users/saira/Documents/DIPLOMADO/MODULO IV/Proyecto-ModuloIV-MineriaDeDatos/DataSets/")
write.csv(comparacion, file="prediccion-testing.csv")
