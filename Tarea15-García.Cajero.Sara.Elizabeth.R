##Preparativos Para realizar los cálculos y mostrar las diversas gráficas se requieren de diversos paquetes con los que cuenta el lenguaje R para estas tareas. El paquete **"ggplot2"** es un potente paquete de graficación y visualización de datos. El paquete **"corrplot"** es requerido para el cálculo y la graficación de la matriz de correlación de un conjunto de datos. El paquete **"plyr"** es requerido para la manipulación de datos y subconjunntos de los mismo. Por último El paquete **"plotrix"** es requerido para la graficación de diagramas de torta en 3D.

install.packages("ggplot2")
install.packages("corrplot")
install.packages("plyr")
install.packages("plotrix")

## Carga y reorganización de datos 

datos<-read.csv("C:\\Users\\sara1\\Downloads\\StudentsPerformance.csv")

colnames(datos)<-c("gender","race","education", "lunch", "testprep", "math","reading", "writing")

datos1 <- datos[datos$education=="associate's degree",]
datos2 <- datos[datos$education=="bachelor's degree",]
datos3 <- datos[datos$education=="high school",]
datos4 <- datos[datos$education=="master's degree",]
datos5 <- datos[datos$education=="some college",]
datos6 <- datos[datos$education=="some high school",]

## Resumen y visualización de la composición los datos

head(datos)

tail(datos)

names(datos)

str(datos)

summary(datos)

## Análisis descriptivo de los datos

### Medidas de tendencia central por nivel de educación de los padres

summary(datos1) 
summary(datos2)
summary(datos3)
summary(datos4)
summary(datos5)
summary(datos6)

### Observaciones de las medidas de tendencia central

### Observaciones de las medidas de tendencia no central o análisis de cuartiles


### Observaciones de las medidas de desviación de las variables numéricas de acuerdo al nivel de educación de los padres

sd1=c(sd(datos1[,6]),sd(datos1[,7]),sd(datos1[,8]))
sd2=c(sd(datos2[,6]),sd(datos2[,7]),sd(datos2[,8]))
sd3=c(sd(datos3[,6]),sd(datos3[,7]),sd(datos3[,8]))
sd4=c(sd(datos4[,6]),sd(datos4[,7]),sd(datos4[,8]))
sd5=c(sd(datos5[,6]),sd(datos5[,7]),sd(datos5[,8]))
sd6=c(sd(datos6[,6]),sd(datos6[,7]),sd(datos6[,8]))

notas=c("Notas de matemática", "Notas de Lectura", "Notas de escritura")
datosd= data.frame(notas,sd1,sd2,sd3,sd4,sd5,sd6)
colnames(datosd)=c("                  ","  SDAD  ","  SDBD  ", "  SDHS  ", "  SDMD  ", "  SDSC  ", "  SDSH  ") 
datosd

#SD: Desviación Estándar
#SDAD: SD Associate's Degree
#SDBD: SD Bachelor's Degree
#SDHS: SD High School
#SDMD: SD Master's Degree
#SDSC: SD Some College
#SDSH: SD Some High School

## Análisis Gráfico

### Matriz de dispersión 

pairs(datos1, main="MDD Associate's Degree")  
pairs(datos2, main="MDD Bachelor's Degree")  
pairs(datos3, main="MDD High School")  
pairs(datos4, main="MDD Master's Degree")  
pairs(datos5, main="MDD Some College") 
pairs(datos6, main="MDD Some High School")  

### Observaciones de la matriz de dispersión 

### Diagramas de dispersión de todas las asignaturas de acuerdo al nivel de educación de los padres
require("ggplot2")

mdd1=ggplot(datos, aes(math,reading, color=education)) +geom_point() + stat_smooth(method = "loess", formula = y ~ x)+ labs(title="Diagrama de Dispersión Notas de Matemáticas vs Notas de Lectura")+ labs(x="Notas de Matemática", y="Notas de Lectura") 
mdd1

mdd2=ggplot(datos, aes(reading,writing, color=education)) +geom_point() + stat_smooth(method = "loess", formula = y ~ x) + labs(title="Diagrama de Dispersión Notas de Lectura vs Notas de Escritura")+ labs(x="Notas de Lectura", y="Notas de Escritura") 
mdd2

mdd3=ggplot(datos, aes(math,writing, color=education)) +geom_point() + stat_smooth(method = "loess", formula = y ~ x) + labs(title="Diagrama de Dispersión Notas de Matemática vs Notas de Escritura")+ labs(x="Notas de Lectura", y="Notas de Escritura") 
mdd3


### Histogramas 


layout(matrix(c(1:6), nrow=2, byrow=FALSE))
hist(datos1$math, main="Associate's Degree", xlab="Notas de Matemáticas", ylab="Frecuencia")
hist(datos2$math, main="Bachelor's Degree", xlab="Notas de Matemáticas", ylab="Frecuencia")
hist(datos3$math, main="High School", xlab="Notas de Matemáticas", ylab="Frecuencia")
hist(datos4$math, main="Master's Degree", xlab="Notas de Matemáticas", ylab="Frecuencia")
hist(datos5$math, main="Some College", xlab="Notas de Matemáticas", ylab="Frecuencia")
hist(datos6$math, main="Some High School", xlab="Notas de Matemáticas", ylab="Frecuencia")

layout(matrix(c(1:6), nrow=2, byrow=FALSE))
hist(datos1$reading, main="Associate's Degree", xlab="Notas de Lectura", ylab="Frecuencia")
hist(datos2$reading, main="Bachelor's Degree", xlab="Notas de Lectura", ylab="Frecuencia")
hist(datos3$reading, main="High School", xlab="Notas de Lectura", ylab="Frecuencia")
hist(datos4$reading, main="Master's Degree", xlab="Notas de Lectura", ylab="Frecuencia")
hist(datos5$reading, main="Some College", xlab="Notas de Lectura", ylab="Frecuencia")
hist(datos6$reading, main="Some High School", xlab="Notas de Lectura", ylab="Frecuencia")

layout(matrix(c(1:6), nrow=2, byrow=FALSE))
hist(datos1$writing, main="Associate's Degree", xlab="Notas de Escritura", ylab="Frecuencia")
hist(datos2$writing, main="Bachelor's Degree", xlab="Notas de Escritura", ylab="Frecuencia")
hist(datos3$writing, main="High School", xlab="Notas de Escritura", ylab="Frecuencia")
hist(datos4$writing, main="Master's Degree", xlab="Notas de Escritura", ylab="Frecuencia")
hist(datos5$writing, main="Some College", xlab="Notas de Escritura", ylab="Frecuencia")
hist(datos6$writing, main="Some High School", xlab="Notas de Escritura", ylab="Frecuencia")

hist1=ggplot(data=datos, aes(math)) + geom_histogram(binwidth=5, color="gray", aes(fill=education))+ labs(title="Notas de Matemáticas vs Educación de Padres")+ labs(x="Notas de Matemática", y="Frecuencia") 
hist1

hist2=ggplot(data=datos, aes(reading)) + geom_histogram(binwidth=5, color="gray", aes(fill=education))+ labs(title="Notas de Lectura vs Educación de Padres")+ labs(x="Notas de Lectura", y="Frecuencia") 

hist3=ggplot(data=datos, aes(writing)) + geom_histogram(binwidth=5, color="gray", aes(fill=education))+ labs(title="Notas de Escritura vs Educación de Padres")+ labs(x="Notas de Escritura", y="Frecuencia") 
hist3

### Observaciones de los histogramas 

### Diagramas de caja y bigote 

box1=ggplot(datos, aes(education, math, color = education)) + geom_boxplot() + labs(title="Notas de Matemáticas vs Educación de Padres")+ labs(x="Educación de Padres", y="Notas de Matemáticas") + stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")
box1


box2=ggplot(datos, aes(education, reading, color = education)) + geom_boxplot() + labs(title="Notas de Lectura vs Educación de Padres")+ labs(x="Educación de Padres", y="Notas de Lectura") + stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="green")
box2
```

box3=ggplot(datos, aes(education, writing, color = education)) + geom_boxplot() + labs(title="Notas de Escritura vs Educación de Padres")+ labs(x="Educación de Padres", y="Notas de Escritura") + stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="blue")
box3

### Observaciones de los diagramas de caja y bigote 

### Matriz de correlación de los datos

require("corrplot")

corrplot(cor(datos[,6:8]), method="color", addCoef.col = "white", type="upper", diag=FALSE, tl.col="black")

### Observaciones de la matriz de Correlación

### Prueba gráfica de normalidad  

layout(matrix(c(1:6), nrow=2, byrow=FALSE))

qqnorm(datos1$math, main= "Math Associate's Degree")
qqline(datos1$math)

qqnorm(datos2$math, main= "Math Bachelor's Degree")
qqline(datos2$math)

qqnorm(datos3$math, main= "Math High School")
qqline(datos3$math)

qqnorm(datos4$math, main= "Math Master's Degree")
qqline(datos4$math)

qqnorm(datos5$math, main= "Math Some College")
qqline(datos5$math)

qqnorm(datos6$math, main= "Math Some High School")
qqline(datos6$math)

layout(matrix(c(1:6), nrow=2, byrow=FALSE))

qqnorm(datos1$reading, main= "Reading Associate's Degree")
qqline(datos1$reading)

qqnorm(datos2$reading, main= "Reading Bachelor's Degree")
qqline(datos2$reading)

qqnorm(datos3$reading, main= "Reading High School")
qqline(datos3$reading)

qqnorm(datos4$reading, main= "Reading Master's Degree")
qqline(datos4$reading)

qqnorm(datos5$reading, main= "Reading Some College")
qqline(datos5$reading)

qqnorm(datos6$reading, main= "Reading Some High School")
qqline(datos6$reading)

layout(matrix(c(1:6), nrow=2, byrow=FALSE))

qqnorm(datos1$writing, main= "Writing Associate's Degree")
qqline(datos1$writing)

qqnorm(datos2$writing, main= "Writing Bachelor's Degree")
qqline(datos2$writing)

qqnorm(datos3$writing, main= "Writing High School")
qqline(datos3$writing)

qqnorm(datos4$writing, main= "Writing Master's Degree")
qqline(datos4$writing)

qqnorm(datos5$writing, main= "Writing Some College")
qqline(datos5$writing)

qqnorm(datos6$writing, main= "Writing Some High School")
qqline(datos6$writing)

### Observaciones de la prueba gráfica de normalidad

### Observaciones Finales

# Cálculo de probabilidades con lenguaje R

## Simulación de datos que siguen una distribución normal

nx=rnorm(100,25,sqrt(5))

#Prueba gráfica de normalidad
qqnorm(nx, main = "Prueba de normalidad de los valores simulados")
qqline(nx)

hist(nx,freq=F,ylim=c(0.0,0.3), main="Histograma de nuestros datos vs la curva normal")
curve(dnorm(x,mean=25,sd=sqrt(5)),add=T)

plot(density(nx),col="red",ylim=c(0.0,0.3), main="Densidad de nuestros datos vs la curva normal")
curve(dnorm(x,mean=25,sd=sqrt(5)),add=T, col="blue")
abline(v=mean(nx))

#Gráfica de la función de densidad 
curve(dnorm(x,25,sqrt(5)),xlim=c(10,40),ylab="f(x)",xlab="x", main="Curva Normal Generalizada\n media=25 y var=5", col="blue")

## Cálculo de probabilidades de la distribución normal N(25,5)

#Se calculan las probabilidades considerando una distribución normal de media 25 y varianza 5
1-pnorm(12,25,sqrt(5))  # Observación: este valor arroja 1 ya que la probabilidad 12 es tan pequeña que es casi cero
pnorm(32,25,sqrt(5))-pnorm(17,25,sqrt(5))
pnorm(25,25,sqrt(5))

#Se calculan los valores que dan determinada probabilidad en una distribución normal de media 25 y varianza 5

#Valor k tal que a la derecha hay un área de 0.05 a la derecha
qnorm(1-0.05,25,sqrt(5))

#Valor k tal que a la derecha hay un área de 0.025 a la derecha
qnorm(1-0.025,25,sqrt(5))

#Valor k tal que a la derecha hay un área de 0.95 a la derecha
qnorm(1-0.95,25,sqrt(5))
```


## Cáculo de probabilidades de la distribución binomial B(2000, 4.2%)
require("plotrix")

#Secuencia de 2000 números aleatorios binomiales con n=2000 y p=0.042#
bin=rbinom(2000,1,0.042)

#Tabla de frecuencias para los artículos con defectos
require("plyr")
bin=as.factor(bin)
bin=revalue(bin, c("0"="No defectuoso", "1"="Defectuoso"))
table(bin)

#Se grafican pies de torta para las tablas obtenidas anteriormente
porcentaje= round(100*(table(bin))/sum(table(bin)),2)
lab=paste(porcentaje,"%")
pie(table(bin), col=rainbow(2), labels=lab, main="Calidad de Artículos", border="white")
legend("topleft", legend=c("No defectuoso", "Defectuoso"), fill=rainbow(2))

#Se grafican diagramas de torta usando el paquete plotrix y el comando pie3D
pie3D(table(bin),explode=0.2, labels=lab)

#Calculo de probabilidades usando la distribución binomial
1-pbinom(4,120,0.042)


#Cálculo usando directamente la distribución binomial
sum(dbinom(7:10,120,0.042))

#Se grafica la función de densidad de la distribución binomial ajustada a los parámetros solicitados 
plot(density(as.numeric(bin)),type="h",xlab="X Artículos",ylab="Artículos con defectos",main="Función de Probabilidad B(2000,0.042)")

## Cálculo de probabilidades de la distribución chi-cuadrado con 23 grados de libertad
#Secuencia de 150 números aleatorios chi cuadrado#
chi <- rchisq(150,23)
chi

#Función de densidad chi cuadrado#
curve(dchisq(x,df = 23),xlim=c(0,100),xlab="U",ylab="f(U)", main="Función de densidad chi-cuadrada\n grados de libertad = 23")

#Cálculo de proababilidad usando la distribución chi cuadrado
#P(U>17.75)
1-pchisq(17.75,23)

#P(31.5<U<22.4)
pchisq(31.5,23)-pchisq(22.4,23)

#Valor k tal que a la derecha hay un área de 0.05 a la derecha
qchisq(1-0.05,23)

#Valor k tal que a la derecha hay un área de 0.99 a la derecha
qchisq(1-0.99,23)

##Cálculo de probabilidades de la distribución t-student con 37 grados de libertad

#Secuencia de números aleatorios t-student#
t <- rt(150,37)
t

#Función de densidad t-student#
curve(dt(x,df = 37),xlim=c(-5,5),xlab="T",ylab="f(T)", main="Función de densidad T-Student\n grados de libertad = 37")

#Cálculo de probabilidades usando la distribución t-student

#P(T>17.75)
1-pt(1.43,37)

#P(-0.57<T<0.52)
pt(0.52,37)-pt(-0.57,37)

#Valor k tal que a la derecha hay un área de 0.025 a la derecha
qt(1-0.025,37)

#Valor k tal que a la derecha hay un área de 0.095 a la derecha
qt(1-0.095,37)