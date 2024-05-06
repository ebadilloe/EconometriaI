setwd("D:/.../Variables categoricas R")

install.packages("stargazer")
library(haven); library(stargazer)

# Cargar y visualizar el dataset
WAGE1 <- read_dta("WAGE1.DTA")
View(WAGE1)

# Convertir el dataset en dataframe y visiualizarlo
WAGE1  <- as.data.frame(WAGE1 )
str(WAGE1 )

### :::::: salario en funci?n de la educaci?n (RLS) :::::::
summary(ols<-lm(wage ~ educ, data=WAGE1)) 

### :::::: Variable categorica genero :::::::
WAGE1$male = ifelse(WAGE1$female=="0", 1, 0)

cbind("Freq"=table(WAGE1$male), "%"=prop.table(table(WAGE1$male)))
cbind("Freq"=table(WAGE1$female), "%"=prop.table(table(WAGE1$female)))

#Correlacion lineal perfecta entre variables male y female
cor(WAGE1$female,WAGE1$male)
#Trampa de las variables binarias
summary(ols_multi<-lm(wage ~ educ + male + female, data=WAGE1)) #Elimina una de las variables que genera multicolinealidad perfecta
#Modelo sin intercepto
summary(ols_sininter<-lm(wage ~ 0 + educ + male + female, data=WAGE1)) 
#Modelo P-1 categorias
summary(ols_male<-lm(wage ~ educ + male, data=WAGE1))
summary(ols_female<-lm(wage ~ educ + female, data=WAGE1))
stargazer(ols_multi, ols_sininter, ols_male, ols_female, type="text")


#modelo4, cambio en pendiente: incluir interacción entre dos variables
WAGE1$educXfemale = WAGE1$educ*WAGE1$female
WAGE1$educXmale = WAGE1$educ*WAGE1$male
summary(ols_pend<-lm(lwage~educ+educXmale, data=WAGE1))

#modelo5, cambio tanto en intercepto como en pendiente
summary(ols_interpend<-lm(lwage~educ+male+educXmale, data=WAGE1))

#modelo6, estimación modelo por separado para los dos grupos
data1=subset(WAGE1,male==1)
data2=subset(WAGE1,male==0)
summary(ols_sepmale<-lm(lwage~educ, data=data1))
summary(ols_sepfemale<-lm(lwage~educ, data=data2))

stargazer(ols_male, ols_female, ols_pend, ols_interpend, ols_sepmale, ols_sepfemale,  type="text")



# Cargar y visualizar el dataset
data <- read_dta("WAGE1.DTA")
View(data)

# Un factor varias categorias
cbind(table(data$educ))
data$primaria <- NA
data$primaria[(data$educ>=0 & data$educ<=5)] <- 1
data$primaria[is.na(data$primaria)] <- 0
cbind(table(data$primaria))

data$secundaria <- NA
data$secundaria[(data$educ>=6 & data$educ<=13)] <- 1
data$secundaria[is.na(data$secundaria)] <- 0
cbind(table(data$secundaria))

data$superior <- NA
data$superior[(data$educ>=14 & data$educ<=18)] <- 1
data$superior[is.na(data$superior)] <- 0
cbind(table(data$superior))

summary(modelo1<-lm(lwage~exper+primaria+secundaria+superior, data=data))
summary(modelo2<-lm(lwage~exper+primaria+secundaria+superior+0, data=data))
summary(modelo3<-lm(lwage~exper+primaria+secundaria, data=data))  
summary(modelo4<-lm(lwage~exper+secundaria+superior, data=data))
summary(modelo5<-lm(lwage~exper+primaria+superior, data=data))

data$educat <- NA
data$educat[(data$educ>=0 & data$educ<=5)] <- 1
data$educat[(data$educ>=6 & data$educ<=13)] <- 2
data$educat[(data$educ>=14 & data$educ<=18)] <- 3
cbind(table(data$educat))

summary(modelo6<-lm(lwage~exper+factor(educat), data=data))


stargazer(modelo1, modelo2, modelo3, modelo4, modelo5, modelo6,  type="text")


