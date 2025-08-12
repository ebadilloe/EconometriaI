# Vector de nombres
nombres <- c("Alejandro", "Brian", "Daniel", "Isabella", "Sara", "Willington", 
             "Saili", "Emanuel", "Sebastian", "Erika")

# Mezclar el orden de los nombres aleatoriamente
nombres_mezclados <- sample(nombres)

# Crear parejas
parejas <- matrix(nombres_mezclados, ncol = 2, byrow = TRUE)

# Mostrar parejas
colnames(parejas) <- c("Persona 1", "Persona 2")
parejas

## Ejercicio 1: Calcule la diferencia entre el año actual y el año en que comenzó a estudiar en esta universidad 
#   y divídalo por la diferencia entre el año actual y el año en que nació. 
#   Multiplique esto por 100 para obtener el porcentaje de su vida 
#   que ha pasado en esta universidad. Use corchetes si los necesita

#
### Vectores
b <- c(1,7,9,13)
b
b[2] <- 8 #cambio el valor del elemento 2
b

nombre <- c("John","Luciana","Alexa")
altura <- c(50.1, 89.3, 78.2)
alto <- c(FALSE, TRUE, TRUE)
altura[2] #muestra el valor del elemento 2
altura[2:3] #muestra un rango, de 2 a 3
altura[c(2,3)] #muestra el elemento 2 y 3
altura[-c(2,3)] #no incluye el elemento 2 y 3

#
### Matrices
mat <- matrix(data=c(9,2,3,4,5,6),ncol=3,nrow=2)
mat
mat[1,2] #seleccionar el elemento (1,2) de la matriz
mat[2,] #selecciona toda la fila

#
### Dataframe
df1 <- data.frame(nombre=nombre, altura=altura)
df1
mean(df1$altura)

df2 <- data.frame(nombre=c("John","Luciana","Alexa", "Sara", "Francisco"), altura=c(50.1, 89.3, 78.2, NA, 60.0))
df2
mean(df2$altura)
mean(df2$altura, na.rm=TRUE)

#
### Cargando base de datos
library(haven); library(summarytools) # haven: paquete para leer archivos .dta

data <- read_dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wage1.dta")
View(data)
colnames(data)

## Estadisticos descriptivos
summary(data$wage)
summary(data[,1:3]) #Estadisticas de las primeras 3 variables
summary(data[,c("wage", "educ", "exper")])
mean(data$wage)
median(data$wage)
sd(data$wage)
quantile(data$wage)
freq(data$married)
summary(data$wage[data$married==1])
summary(data$wage[data$married==1 & data$female==1])

## Gráficos
#Histogramas
hist(data$wage)
hist(data$wage, xlab="Salario hora", col="blue", 
     main="Histograma de frecuencia de los salarios")
hist(data$wage, xlab="Salario hora", col="pink", 
     main="Histograma de probabilidad de los salarios", freq=FALSE)

#Densidad
plot(density(data$wage), main="Densidad de los salarios hora")
plot(density(data$wage[data$nonwhite==1]), col="red", main="Densidad por raza", 
     xlab="Salarios hora", ylab="Densidad", lwd=2, lty=1)
lines(density(data$wage[data$nonwhite==0]), col="blue", lwd=2, lty=2)
legend(13, 0.1, legend = c("Afro", "No afro"),col = c("red", "blue"),lty=c(1,2), 
       lwd=2, cex=.8, bty = "n")

#Dispersión
plot(data$wage, col="blue")
plot(data$wage, type="l", col="gold")
plot(data$wage, type="l", col="gold", ylab="Salario por hora", xlab="n")
plot(data$educ,data$wage, xlab="Años de educación", ylab="Salario hora", col="blue")
abline(lm(wage ~ educ, data=data), col="red") # Con esta función se traza la línea de regresión

#ggplot2
library("ggplot2") # Se carga el paquete de ggplot2
ggplot(data = data, mapping = aes(x = educ, y = wage))

ggplot(data = data, mapping = aes(x = educ, y = wage)) + geom_point() +
  labs(x="Años de educación", y="Salario hora", title="Relación salarios-educación")

ggplot(data = data, mapping = aes(x = educ, y = wage, color = factor(female))) +
  geom_point(size=2) + 
  labs(x="Años de educación", y="Salario hora", title="Relación salarios-educación", color="Género") + 
  scale_color_manual(labels = c("Hombres", "Mujeres"), values = c("blue", "red"))

ggplot(data = data, aes(x = educ, y = wage)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="Años de educación", y="Salario hora", title="Relación salarios-educación")

#Cambia la escala de la var Y a log: scale_y_log10
ggplot(data = data, aes(x = educ, y = wage)) + 
  geom_point(alpha = 0.5) + 
  scale_y_log10() + 
  geom_smooth(method="lm", size=1.5) +
  labs(x="Años de educación", y="Log salario hora", title="Relación salarios-educación")

#Dos paneles: facet_grid
ggplot(data = data, aes(x = educ, y = wage, color = factor(female))) +
  geom_point(show.legend = FALSE) + 
  geom_smooth(method="lm",show.legend = FALSE) + 
  facet_grid(~female, labeller = labeller(female=c("0" = "Hombres", "1" = "Mujeres"))) +
  labs(x="Años de educación", y="Salario hora", title="Relación salarios-educación")

#Diversos temas (apariencia): theme_bw, theme_classic y theme_dark
ggplot(data = data, aes(x = educ, y = wage, color = factor(female))) +
  geom_point(show.legend = FALSE) + 
  geom_smooth(method="lm",show.legend = FALSE) + 
  facet_grid(~female, labeller = labeller(female=c("0" = "Hombres", "1" = "Mujeres"))) +
  labs(x="Años de educación", y="Salario hora", title="Relación salarios-educación") +
  theme_bw()

#Boxplot  
boxplot(data$wage, ylab="Salarios hora")
        #Este gráfico muestra los cuartiles, los valores mínimo y máximo y 
        #valores raros o outliers
boxplot(data$wage~data$educ, xlab="Años de educación", ylab="Salarios hora")
boxplot(data$wage~data$educ, xlab="Años de educación", ylab="Salarios hora", 
        xaxt="none", cex.axis = .8)
        axis(1, seq(1,18,1), cex.axis = .8)

## Ejercicio 2:
        #Utilizando la base de datos wage1.dta del libro de Wooldridge realice 
        #un análisis de las diferencias salariales por género. Para ello calcule 
        #las principales estadísticas y la densidad de los salarios hora (wage) 
        #distinguiendo por género (female). Con el fin de determinar si existe 
        #alguna relación entre el diferencial salarial por género y la educación, 
        #haga un scatterplot entre salarios y educación distinguiendo por género. 
        #También realice un boxplot del salario distinguiendo nivel educativo 
        #diferenciando por género. Con los resultados realice un breve análisis 
        #que ayude a entender las diferencias salariales entre hombres y mujeres
        
## Ejercicio 3:
#  Utilizando la base de datos bwght.dta (los datos pueden ser descargados con la 
#        siguiente función read_dta(“http://fmwww.bc.edu/ec-p/data/wooldridge/bwght.dta”)) del libro de Wooldridge realice un análisis del impacto que pueden tener distintos factores en el peso de un bebé al nacer. Para esto, realice las siguientes gráficas y analícelas brevemente:
  
#  Un diagrama de dispersión que relacione el peso del bebé al nacer y el ingreso familiar.

#  Un diagrama de dispersión que relacione el número de cigarrillos que fumaba 
#        la madre del bebé diariamente mientras estaba embarazada y el peso del bebé al nacer. 
#        Diferencie entre un bebé hombre y mujer.

#  Un gráfico de dos paneles que muestre, en uno, la relación entre los cigarrillos 
#        fumados diariamente y el impuesto que estos tenían y, en otro, la relación 
#        entre los cigarillos fumados diariamente y el precio de los mismos.

#  Un gráfico de densidad para el peso del bebé al nacer, diferenciando entre 
#        bebés hombres y muejeres.

#  Un boxplot del ingreso familiar.

#  En un gráfico con dos paneles grafique el modelo de regresión lineal simple que 
#        relacione el peso del bebé al nacer con el número de cigarrillos 
#        fumados diariamente, diferenciando entre bebés hombres y mujeres.
        
        