
setwd("C:/Users/ErikaBad/OneDrive - Universidad Aut?noma Latinoamericana/1 Econometria/1 ECONOMETRIA I-UNAULA/Rstudio_clases/Unidad 2")
install.packages("readxl")
install.packages("haven")
library(readxl); library(haven)

data<-read_excel("wage1.xlsx", range="A1:D527")
View(data) #verificar lo que se est? corriendo

#leer desde archivo Stata
PRMINWGE <- read_dta("PRMINWGE.DTA")
View(PRMINWGE)
#dejar los nombres con min?scula

# Recode
summary(data$exper)
data$bexper <- NA
#bsexo <- NA
data$bexper[data$exper>=17 & data$exper<=52] <- 1
data$bexper[data$exper<17] <- 0
table(data$bexper)

table(data$female)
data$bsexo <- NA
View(data)
#bsexo <- NA
data$bsexo[data$female==1] <- 1
data$bsexo[data$female==2] <- 0
table(data$bsexo)

# Labels
data$bsexo <- factor(data$bsexo,
                     levels=c(1,0),
                     labels=c("Mujer (1)", "Hombre (0)"))

# Estad?sticas condicionales (filtros)
summary(data$wage)
summary(data$wage[data$bsexo=="Mujer (1)"])
summary(data$wage[data$bsexo=="Hombre (0)"])

# Densidades condicionales
d1 <- density(data$wage[data$bsexo=="Mujer (1)"])
plot(d1)
d2 <- density(data$wage[data$bsexo=="Hombre (0)"])
plot(d2)

# Overlay las densidades (lwd grosor de la linea, lty punteada)
plot(density(data$wage[data$bsexo=="Mujer (1)"]), col="red", main="Densidad por genero", 
     xlab="Salarios", ylab="Densidad", lwd=2, lty=1)
lines(density(data$wage[data$bsexo=="Hombre (0)"]), col="blue", lwd=2, lty=2)
legend(17, 0.2, legend = c("Mujer", "Hombre"),col = c("red", "blue"),lty=c(1,2), 
       lwd=2, cex=.8, bty = "n")

# Matriz de correlacion
subdata <- data[,c("wage","educ")]
cor(subdata)
cor<-cor(data[,c("wage","educ")])
cor

# Correlaciones con significancia estadistica
install.packages("Hmisc")
library(Hmisc)
rcorr(as.matrix(subdata))
rcorr(as.matrix(data[,c("wage","educ")]))
cor2<-rcorr(as.matrix(data[,c("wage","educ")]))
cor2

# corrplot function
install.packages("corrplot")
library(corrplot)
corrplot(cor, type="upper", order = "hclust",tl.col = "black",tl.srt = 45)
      #sacar corr insig
corrplot(cor, type="upper", order="hclust", 
         p.mat = cor2$P, sig.level = 0.05, insig = "blank")

# Scatter plot
plot(data$wage~data$educ)
plot(data$wage~data$educ, pch = 16, cex = 1.3, col = "blue", 
     main = "Relacion entre salarios y educaci?n", xlab = "Educacion (a?os)", ylab = "Salarios (hora)")

# chart.Correlation function
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(data[,c("wage","educ")], histogram=TRUE, pch=19)
chart.Correlation(data[,c("educ","wage")], histogram=TRUE, pch=19)

# Test de diferencia de medias en los salarios diferenciando entre hombre y mujer
mean(data$wage[data$bsexo=="Mujer (1)"])
mean(data$wage[data$bsexo=="Hombre (0)"])
mean(data$wage[data$bsexo=="Mujer (1)"])-mean(data$wage[data$bsexo=="Hombre (0)"])
t.test(wage ~ bsexo, data=data)



# Modelo de RLS
modelo <- lm(wage ~ educ, data=data)
summary(modelo)
modelo_mujer <- lm(wage ~ educ, data=subset(data,bsexo=="Mujer (1)"))
summary(modelo_mujer)
modelo_hombre <- lm(wage ~ educ, data=subset(data,bsexo=="Hombre (0)"))
summary(modelo_hombre)

# Intervalo de confianza
confint(modelo, level = 0.95)

# Scatter plot con linea de regresion
plot(data$wage~data$educ, pch = 16, cex = 1.3, col = "blue", 
     main = "Relaci?n entre salarios y educaci?n", xlab = "Educacion (años)", ylab = "Salarios (hora)")
abline(lm(wage ~ educ, data=data),col="red",lwd=2)


modelo <- lm(wage ~ educ, data=data)
summary(modelo)

data$lwage <- log(data$wage)
summary(data$lwage)
modelolog <- lm(lwage ~ educ, data=data)
summary(modelolog)
