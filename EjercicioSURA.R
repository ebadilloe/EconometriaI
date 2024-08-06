
install.packages("tidyverse")
install.packages("summarytools")

library(readr); library(tidyverse); library(dplyr); library(summarytools)

#setwd("C:/Users/unaula/Downloads/Econometria")

setwd("C:/Users/INVESTIGADOR/OneDrive - Universidad Autónoma Latinoamericana/1 Econometria/1 ECONOMETRIA I-UNAULA/Rstudio_clases/SURA")

data <- read_delim("bd_house.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(data)

names(data)

#Pipeline
#Desde el paquete Dplyr, “%>%” es una herramienta para la ejecución de pipeline o tuberías de comandos que permite una encadenamiento de funciones
df1 <- data %>%
  group_by(BedroomAbvGr) %>%
  summarise(media_precio=mean(SalePrice))
df1

df2<- data %>%
  filter(SalePrice==150000)
freq(df2$GarageCars)
