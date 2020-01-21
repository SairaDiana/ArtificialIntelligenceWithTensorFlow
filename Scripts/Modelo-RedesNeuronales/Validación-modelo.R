data1 <- read.csv("Heart3.csv")

#Aplicamos un muestreo aleatorio simple para hacer el entrenamiento de nuestro modelo 
#con el 20% de nuestros datos
muestra2<-data1[sample(1:303,243),]
#View(muestra)

#Las siguientes lineas convierten a la variable objetivo en valores booleanos
muestra2$s<-c(muestra2$MEDV=="s")
muestra2$n<-c(muestra2$MEDV=="n")

#Con la librería neuralnet se contruye la red neuronal con el algoritmo de retropropagación
modelo=neuralnet(s + n ~ age	+ sex +	cp + trestbps + chol + restecg + thalach +	exang	+ oldpeak	+ slope	+ ca	+ thal,
                 muestra2, hidden = 3, 
                 lifesign = "full",
                 algorithm = "rprop+")

#Ahora, se va a construir la gráfica de la red neuronal
plot(modelo, rep = "best")
plot(modelo, rep = "best", intercept = FALSE)

#Ahora se recorre la tabla completa para hacer calculo de as predicciones
prediccion <- compute(modelo, muestra2[1:12])
prediccion
which.max(prediccion$net.result[1,])
result<-0
for (i in 1:243) {result[i] <- which.max(prediccion$net.result[i,])}
for (i in 1:243) {if (result[i]==1) {result[i] = "s"}}
for (i in 1:243) {if (result[i]==2) {result[i] = "n"}}


#Se hace una comparación de las clases reales con las predicciones
comparacion<-muestra2
comparacion$predicted<-result
comparacion

#Por último se obtiene la matriz de confusion, en la cual los valores de la diagonal son 
#los más importantes, ya que con ellos se pueden calcular la precisión del modelo
(MC <- table(muestra2$MEDV, result))

#Exportar la tabla con la predicciones para sacar tablas de contigencia
setwd("C:/Users/saira/Documents/DIPLOMADO/MODULO IV/PROYECTO/DataSets")
write.csv(comparacion, file="Prueba3.csv")
