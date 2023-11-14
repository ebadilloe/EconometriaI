rm(list = ls())

##Econometria I
##Profesora Erika Badillo

library(readr)
data <- read.csv("WAGE1.csv",header = T,sep = ",")

# i. Un factor dos categor?as
# A. Cambio en el intercepto (intercepto + una binaria)

summary(lm(wage~educ, data=data))


# A. Cambio en intercepto:
summary(lm(wage~educ+female, data=data))
#0.62282 :intercepto H b1
#0.62282-2.27336=-1.65054 intercepto M b1+b3

cbind("Freq"=table(data$female), "%"=prop.table(table(data$female)))

#####Algunos casos en la regresión, sin intercepto, modelo "ingenuo"
summary(lm(lwage~educ, data=data))
summary(lm(lwage~educ+female, data=data))
summary(lm(lwage~-1+educ+female, data=data))
summary(lm(lwage~1, data=data))

#####Binaria male 
data$male <- NA
data$male[data$female == 0] <- 1
data$male[data$female == 1] <- 0
cbind("Freq"=table(data$male), "%"=prop.table(table(data$male)))

summary(lm(wage~educ+male, data=data))
#-1.65055+2.27336=0.62281: intercepto H b1+b3
#-1.65055 :intercepto M b1

#####no intercepto + dos binaria:
summary(lm(lwage~educ+female+male+0, data=data))
summary(lm(lwage~educ+female+male+0, data=data))


# B. Cambio en la pendiente
data$educXfemale = data$educ*data$female
data$educXmale = data$educ*data$male 

summary(lm(lwage~educ+educXmale, data=data))
summary(lm(lwage~educXmale, data=data))

# C. Cambio en el intercepto y la pendiente
summary(lm(lwage~educ+male+educXmale, data=data))
summary(lm(lwage~educ*female, data=data))

summary(lm(lwage~educ, data=data, subset=data$female==1))
summary(lm(lwage~educ, data=data, subset=data$female==0))


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

summary(lm(lwage~exper+secundaria+superior, data=data))
summary(lm(lwage~exper+primaria+superior, data=data))
summary(lm(lwage~exper+primaria+secundaria, data=data))

data$educat <- NA
data$educat[(data$educ>=0 & data$educ<=5)] <- 1
data$educat[(data$educ>=6 & data$educ<=13)] <- 2
data$educat[(data$educ>=14 & data$educ<=18)] <- 3
cbind(table(data$educat))

summary(lm(lwage~exper+factor(educat), data=data))


summary(lm(lwage~exper+primaria+secundaria+superior+0, data=data))
summary(lm(lwage~exper+primaria+secundaria+superior, data=data))  

