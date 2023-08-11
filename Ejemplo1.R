# Econometría 1 #
# ejecutamos las instrucciones en Rstudio con Ctrl+enter en la linea que queremos ejecutar
R.version
Ctrl+l limpiar la console
install.packages("installr")
library(installr)
updateR()



#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#   Algunas funciones básicas para el manejo de datos         #  
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#     El análisis de datos permite brindar una mirada precisa de la información

#**Estadística:** ¿Para qué nos sirve?
#     Analizar información y tomar decisiones frente a situaciones inciertas
#     Adquirir, organizar y analizar datos
#     Cuantificar un determinado escenario de la vida real

#**Estadistica descriptiva** y estadística inferencial
#     La estadística descriptiva está formada por procedimientos empleados 
#      para resumir y describir las características importantes de un conjunto 
#      de mediciones. 
#      La estadística inferencial está formada por procedimientos empleados 
#      para extraer ideas y aproximaciones acerca de características 
#      poblacionales, a partir de información contenida en una muestra

#**Análisis exploratorio de los datos (Describirlos)**
#     ¿Cuántas observaciones tenemos?
#     ¿Cuántas variables? 
#     ¿En qué unidad se encuentran almacenados?
#     ¿Cuál es la periodicidad de nuestros datos?
#     ¿Cómo están distribuidos? 

#**Existen diversos programas**, aplicaciones, paquetes, softwares e, incluso, 
#      lenguajes de programación que permiten realizar exploración, análisis, 
#      cálculos, inferencia, ilustración, entre otros, de los datos:
#      Stata, SQL, R, R-studio, Spss, Excel, Python
#      R: Lenguaje de código abierto (gratuito)

#**Una librería** es un conjunto de códigos de programación que facilitan el trabajo
#      con lenguajes de programación. Existen librerías enfocadas a distintas tareas. 
#      Las librerías contienen funciones para realizar esas tareas.
#      Tidyverse: Es una colección de paquetes para R diseñados para la ciencia de datos
#      Ggplot2: Es un paquete parte de Tidyverse para crear gráficos. 
#      En ggplot, aes() hace referencia al contenido estético del gráfico. 
#      Es decir, la función le dará indicios a ggplot2 sobre cómo dibujar los 
#      distintos trazos, formas, colores y tamaños
#      Dplyr: manipulación de marcos de datos    

# Iniciando la sesion de trabajo podemos fijar el directorio 
# donde vamos a realizar esta, por ejemplo, el directorio que contiene la base de 
# datos a importar

# Fijando el directorio
#setwd("C:/.../")



#install.packages("gapminder")
library(gapminder)
data("gapminder")
head(gapminder)
tail(gapminder)

#install.packages("writexl")
library(writexl)
write_xlsx(gapminder, "C:/Users/...") ##hay que decirle cuál es la fuente de datos que estamos utilizando: gapminder, "" dónde la va guardar

install.packages(readxl)
library(readxl)
read_xlsx

#View(gapminder)
head(gapminder)
distinct(gapminder)
distinct(gapminder, country)
distinct(gapminder, year)
str(gapminder) #conocer la estructura de la bbdd
min(gapminder$lifeExp)
min(gapminder$pop)
max(gapminder$lifeExp)
max(gapminder$pop)


# install.packages('tidyverse')
library(tidyverse)
#si se quiere ver información relevante de una única variable se utilizan filtros mediante filtros (tydiverse)
# Activar |> %>% (shift+ctrl+m): Tools/Global options/Code/Use native pipe operator
gapminder %>% 
  filter(country == 'Colombia')
gapminder %>%
 filter(country == 'Colombia',
        year == 2002)

paises <- gapminder["country"]
ordenado <- gapminder[order(gapminder$pop),] #ordenar por pop y tener en cuenta todas las filas , 

gapminder %>%
  group_by(gapminder$country)%>%
  summarize(mean_size=mean(year))

gapminder %>%
  group_by(gapminder$country)%>%
  summarize(max=max(year))

## agrupacion bases de datos
# primero creamos las dos bases de datos a pegar
df1 <- gapminder[c(1:1000), c("country", "year", "continent", "lifeExp")] #para las primeras mil obs 
head(df1) #verificamos
df2 <- gapminder[c("country", "year", "gdpPercap")]
head(df2)

# innerjoin unir unicamente las observaciones en comun de las dos bases, es decir, las mil
df_inner <- merge(x=df1, y=df2, by =c("country", "year")) #en comun pais y año, x=izsq, y=der
head(df_inner)

# outerjoin unir todas las observaciones de las dos bases
df_outer <- merge(x=df1, y=df2, all=TRUE) 
head(df_outer)
tail(df_outer)



##########################
# Gráficos #
#Gráfico de pie o torta

#Existen gráficas que nos sirven para representar porcentajes y proporciones, como el pie o torta

#install.packages('gapminder')
#install.packages('tidyverse')
library(gapminder)
library(tidyverse)

  data("gapminder")

#Saber el número de veces que aparece un continente
#Podemos hacer uso de la variable continente y saber cuál es el continente que tiene mayor representación en la base de datos
#Una forma de hacerlo en R es pidiéndole al programa que diga el número de apariciones de cada valor deseado:
  
  length(gapminder$continent[gapminder$continent == "Africa"])
  length(gapminder$continent[gapminder$continent == "Asia"])
  length(gapminder$continent[gapminder$continent == "Europe"]) 
  length(gapminder$continent[gapminder$continent == "Americas"])
  length(gapminder$continent[gapminder$continent == "Oceania"])

#Gráfico de torta o pie básico

#Matplotlib dispone de la función pie, cuya sintaxis depende del grado de personalización y control que se requiera sobre la gráfica de pastel a dibujar
  
Observaciones = c(624, 396, 360, 300, 24)
Continentes = c("África,","Asia,","Europe,","Américas,", "Oceanía,")
pie(Observaciones,labels=Continentes)

#Poniendo los porcentajes
Observaciones = c(624, 396, 360, 300, 24)
Continentes = c("África","Asia","Europe","Américas", "Oceanía")
pie(Observaciones,labels=paste0(Continentes, " ", Observaciones))

#Cambiar colores
Observaciones = c(624, 396, 360, 300, 24)
Continentes = c("África","Asia","Europe","Américas", "Oceanía")
pie(Observaciones,labels=paste0(Continentes, " ", Observaciones),col = 1:6)

#Paleta de colores: función brewer.pal del paquete RColorBrewer.

#install.packages("RColorBrewer")
library(RColorBrewer)
color <- brewer.pal(length(Observaciones), "Set2")
pie(Observaciones,labels=paste0(Continentes, " ", Observaciones),col = color)

#Y ¿qué tal si rebanamos la torta o pie, separando los pedazos?
  #install.packages("plotrix")
  library(plotrix)
color <- brewer.pal(length(Observaciones), "Set2")
pie3D(Observaciones,labels=paste0(Continentes, " ", Observaciones),col = color, explode=0.25)



#Gráfico de barras

#MatplotLib: se pueden crear múltiples tipos de gráficos 
#las gráficas de barras suelen ser usadas para visualizar el valor de datos categóricos. 
#Por un lado, la gráfica o gráfico de barras tiene barras rectangulares con longitudes proporcionales a los valores
#y ejes que representan. 
#Las gráficas de barras se utilizan para comparar dos o más valores. Las barras pueden ser horizontales o verticales.

plot(gapminder$continent)

#Podemos ajustar los parámetros gráficos con los argumentos main, xlab, ylab y col. En este caso, podemos darle a col un vector de colores, uno por barra, para que cada una sea distinta.

plot(x = gapminder$continent, 
     main = "Número de observaciones por continente", 
     xlab = "Continentes", ylab = "Observaciones", 
     col = c("royalblue", "seagreen", "purple", "grey"))
                                                                     
                                                                     
#Finalmente también es posible organizar a nuestro gusto el orden de las barras, creando las posiciones a través de una lista creada.
  Observaciones = c(396, 624, 360, 300, 24)
  Continentes = c("Asia","África","Europe","Américas", "Oceanía")
  barplot(height = Observaciones, names  = Continentes, col = c("red", "green", "white", "blue"))

#Gráfico de barras horizontales
#En algunos casos por restricciones de espacio o estética, entre otras razones, 
  es ideal generar el gráfico de barras acostadas o de manera horizontal. 
                                                                                               
  Observaciones = c(396, 624, 360, 300, 24)
  Continentes = c("Asia","África","Europe","Américas", "Oceanía")
  barplot(height = Observaciones, names  = Continentes, col = c("red", "green", "white", "blue"), horiz=1, las=1)

#Gráfico de dispersión
  #medidas de tendencia central
  #media
  mean(gapminder$pop)
  mean(gapminder$gdpPercap)
  #mediana
  median(gapminder$pop)
  #moda

#Histograma
#  una vision de la dn de nuestros datos
#  representación grafica de una variable en forma de barras, 
#  donde la superficie de cada barra es proporcional a la frecuencia de los valores representados

  hist(gapminder$lifeExp, main="Histograma expectativa de vida", ylab= "Frecuencia")
  hist(gapminder$lifeExp, breaks=5)
  boxplot(gapminder$lifeExp)

  #Después de ver los gráficos de dispersión de las variables expectativa de vida (lifeExp) e ingresos o PIB Per cápita (gdpPercap) 
  #la variable de expectativa de vida tiene muy poca dispersión pues al ubicarse en un rango entre aproximadamente 30 y 80, 
  #los datos tienden a estar acumulados; mientras que, la variable de los ingresos por persona tiene una dispersión mucho más alta, 
  #pues sus valores están en un rango mucho más grande y los puntos tienden a acumularse en diferentes valores. 
  
    
  
#Medidas de dispersion
  #cuantificar cuánto se desvia cada punto con respecto al valor promedio de la variable
  
  sd(gapminder$pop)  #poblaciones muy altas vs poblaciones pequeñas
  sd(gapminder$lifeExp) #se apartan del promedio en 12.9 años
  var(gapminder$pop)
  var(gapminder$lifeExp)
  
  
  
  