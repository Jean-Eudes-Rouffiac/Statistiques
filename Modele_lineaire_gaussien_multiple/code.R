#setwd(...)
data = read.table("Data_HRI.txt", sep=";", header=T)
data <- subset(data, select=-c(Date, DV.dom, DV.maxvv.fact, jour))
attach(data)
summary(data)


data = na.omit(data)

#pairs(data,gap=0.05,cex=0.8,col="purple")
#cor(data)
source("http://www.sthda.com/upload/rquery_cormat.r")
cc<-rquery.cormat(data, type="flatten", graph=FALSE)$r
cm<-cc[order(abs(cc[,"cor"]),decreasing=T),]
cm[1:10, ]

rescor = rquery.cormat(data, graphType="heatmap")

library(car)
vif(lm(PM10.moy~.,data=data))

data <- subset(data, select=-c(T.moy))

vif(lm(PM10.moy~.,data=data))


set.seed(111) # initialisation du g´en´erateur
# Extraction des ´echantillons
test.ratio = 0.30 # part de l’´echantillon test
npop = nrow(data) # nombre de lignes dans les donn´ees
ntest = ceiling(npop*test.ratio) # taille de l’´echantillon test
testi = sample(1:npop, ntest) # indices de l’´echantillon test
appri = setdiff(1:npop, testi) # indices de l’´echant. d’apprentissage
# Construction des ´echantillons avec les variables explicatives
dataApp = data[appri, ] # construction de l’´echantillon d’apprentissage
dataTest = data[testi, ] # construction de l’´echantillon test

model<-lm(PM10.moy~. , dataApp)
reslm1 = model
model$coefficients

summary(model)


#Backward regression
# --------------------#
summary(lm(PM10.moy~. , dataApp))
#Enleve HR.mmin
dataApp2 <- subset(dataApp, select=-c(HR.min))
summary(lm(PM10.moy~. , dataApp2))
#Enleve HR.moy
dataApp3 <- subset(dataApp2, select=-c(HR.moy))
summary(lm(PM10.moy~. , dataApp3))
#Enleve NO.max
dataApp4 <- subset(dataApp3, select=-c(NO.max))
summary(lm(PM10.moy~. , dataApp4))
#Enleve SO2.max
dataApp5 <- subset(dataApp4, select=-c(SO2.max))
summary(lm(PM10.moy~. , dataApp5))

reslm2 = lm(PM10.moy~. , dataApp5)

anova(reslm1, reslm2)


TitreRes = "Graphe des résidus"
plot(dataApp5$PM10.moy, residuals(reslm2), xlab="Concentration en PM10", ylab="Résidus",main=
       TitreRes , cex=0.6, cex.lab=1, cex.main = 1,cex.axis=1)
abline(0,0, col=2, lwd=2)

Titre1 = "Graphe des résidus studentisés"
plot(dataApp5$PM10.moy, rstudent(reslm2),xlab="Concentration en PM10", ylab="Résidus studentisés",main=
       Titre1 , cex=0.6, cex.lab=1, cex.main = 1,cex.axis=1, ylim=c(-2.8,3))
p=15
n=1745
seuil <- qt(0.975,n-p-2)
abline(h=c(-seuil , 0, seuil),col=2)

Titre = "Nuage de points (Observés/Estimés)"
plot(dataApp5$PM10.moy,predict(reslm2), xlab="Concentration PM10 observée", ylab="Concentration PM10 estimée",main= Titre , cex=0.8, cex.lab=1, cex.main =
       1,cex.axis=1)
abline(0,1, col=2, lwd=2)

shapiro.test(reslm2$residuals)
hist(residuals(reslm2),col="orange",xlab="Résidus",ylab="Fréquences",main="Histogramme des résidus",tck=0.01, freq=FALSE)
lines(density(reslm2$residuals), # density plot
      lwd = 2, # thickness of line
      col = "red")

qqnorm(reslm2$residuals, pch = 1, frame = FALSE)
qqline(reslm2$residuals, col = "steelblue", lwd = 2)

ncvTest(reslm2)
durbinWatsonTest(reslm2) 


reslm3=step( reslm1)
dataAppreslm3 = subset(dataApp, select=-c(HR.min, HR.moy))
summary(reslm3)



rmse <- function(error)
{
  sqrt(mean(error^2))
}

mae <- function(error)
{
  mean(abs(error))
}


PMchap_1 = predict(reslm1, newdata=dataTest)
error1 = dataTest$PM10.moy - PMchap_1
rmse(error1)
mae(error1)
PMchap_2 = predict(reslm2, newdata=dataApp5)
error2 = dataTest$PM10.moy - PMchap_2
rmse(error2)
mae(error2)
PMchap_3 = predict(reslm3, newdata=dataTest)
error3 = dataTest$PM10.moy - PMchap_3
rmse(error3)
mae(error3)


PMchap_1 = predict(reslm1)
error1 = dataApp$PM10.moy - PMchap_1
rmse(error1)
mae(error1)

PMchap_2 = predict(reslm2)
error2 = dataApp5$PM10.moy - PMchap_2
rmse(error2)
mae(error2)

PMchap_3 = predict(reslm3)
error3 = dataAppreslm3$PM10.moy - PMchap_3
rmse(error3)
mae(error3)


source("Perfopm10.R")
reslm = reslm2

Perfopm10(dataApp5$PM10.moy, predict(reslm2))
Perfopm10(dataTest$PM10.moy, predict(reslm2, newdata = dataTest))

source("TabDeppm10.R")
TabDeppm10(dataApp5$PM10.moy, predict(reslm2),50,80,50)
TabDeppm10(dataTest$PM10.moy, predict(reslm2, newdata = dataTest),50,80,50)


source("Fig_obspm10.R")
Fig_obspm10(dataApp5$PM10.moy, predict(reslm2), "Observé/estimés")


res.tst <- cbind(Y=dataTest[,"PM10.moy"],
                 predict(reslm, newdata=dataTest, interval="prediction"))


res.tst <- cbind(Y=dataTest$PM10.moy, predict(reslm, newdata=dataTest,
                                              interval="prediction"))

indices <- sample(1:length(dataTest$PM10.moy),15)
boxplot(rbind(res.tst[indices,3],res.tst[indices,3],res.tst[indices,2],
              res.tst[indices,4],res.tst[indices,4]), col="cyan")
points(res.tst[indices,1], pch=19)







######################################################################

### Subsidiaire

######################################################################




### Toutes les variables

data = read.table("Data_HRI.txt", sep=";", header=T)
data <- subset(data, select=-c(Date))
attach(data)
summary(data)


data = na.omit(data)

library(car)
vif(lm(PM10.moy~.,data=data))

data <- subset(data, select=-c(T.moy))

vif(lm(PM10.moy~.,data=data))

data <- subset(data, select=-c(DV.maxvv.fact))
vif(lm(PM10.moy~.,data=data))


set.seed(111) # initialisation du g´en´erateur
# Extraction des ´echantillons
test.ratio = 0.30 # part de l’´echantillon test
npop = nrow(data) # nombre de lignes dans les donn´ees
ntest = ceiling(npop*test.ratio) # taille de l’´echantillon test
testi = sample(1:npop, ntest) # indices de l’´echantillon test
appri = setdiff(1:npop, testi) # indices de l’´echant. d’apprentissage
# Construction des ´echantillons avec les variables explicatives
dataApp = data[appri, ] # construction de l’´echantillon d’apprentissage
dataTest = data[testi, ] # construction de l’´echantillon test

model<-lm(PM10.moy~. , dataApp)

## Modèle 1
reslm1 = model
model$coefficients
summary(model)



Titre = "Nuage de points (Observés/Estimés)"
plot(dataApp$PM10.moy,predict(reslm1), xlab="Concentration PM10 observée", ylab="Concentration PM10 estimée",main= Titre , cex=0.8, cex.lab=1, cex.main =
       1,cex.axis=1)
abline(0,1, col=2, lwd=2)


reslm2=step(reslm1)
dataAppreslm3 = subset(dataApp, select=-c(HR.min, HR.moy, VV.max, jour, SO2.max))
summary(reslm2)

jour - HR.min - S02.max - HR.moy - VV.max


rmse <- function(error)
{
  sqrt(mean(error^2))
}

mae <- function(error)
{
  mean(abs(error))
}

PMchap_2 = predict(reslm2, newdata=dataTest)
error2 = dataTest$PM10.moy - PMchap_2
rmse(error2)
mae(error2)


PMchap_2 = predict(reslm2)
error2 = dataAppreslm3$PM10.moy - PMchap_2
rmse(error2)
mae(error2)


source("Perfopm10.R")
reslm = reslm2

#modele step
Perfopm10(dataAppreslm3$PM10.moy, predict(reslm))
Perfopm10(dataTest$PM10.moy, predict(reslm, newdata = dataTest))


source("TabDeppm10.R")
TabDeppm10(dataAppreslm3$PM10.moy, predict(reslm),50,80,50)
TabDeppm10(dataTest$PM10.moy, predict(reslm, newdata = dataTest),50,80,50)








######################################################################

### Centré réduit

######################################################################






data = read.table("Data_HRI.txt", sep=";", header=T)
data <- subset(data, select=-c(Date, DV.dom, DV.maxvv.fact, jour))
attach(data)
summary(data)


data = na.omit(data)

library(car)
vif(lm(PM10.moy~.,data=data))

data <- subset(data, select=-c(T.moy))

vif(lm(PM10.moy~.,data=data))

data_PM10 = subset(data, select=c(PM10.moy))

data = data <- subset(data, select=-c(PM10.moy))

data = scale(data)

data = data.frame(data_PM10, data)

set.seed(111) # initialisation du g´en´erateur
# Extraction des ´echantillons
test.ratio = 0.30 # part de l’´echantillon test
npop = nrow(data) # nombre de lignes dans les donn´ees
ntest = ceiling(npop*test.ratio) # taille de l’´echantillon test
testi = sample(1:npop, ntest) # indices de l’´echantillon test
appri = setdiff(1:npop, testi) # indices de l’´echant. d’apprentissage
# Construction des ´echantillons avec les variables explicatives
dataApp = data[appri, ] # construction de l’´echantillon d’apprentissage
dataTest = data[testi, ] # construction de l’´echantillon test

model<-lm(PM10.moy~. , dataApp)

## Modèle 1
reslm1 = model
model$coefficients
summary(model)



Titre = "Nuage de points (Observés/Estimés)"
plot(dataApp$PM10.moy,predict(reslm1), xlab="Concentration PM10 observée", ylab="Concentration PM10 estimée",main= Titre , cex=0.8, cex.lab=1, cex.main =
       1,cex.axis=1)
abline(0,1, col=2, lwd=2)


reslm2=step(reslm1)
dataAppreslm3 = subset(dataApp, select=-c(HR.min, HR.moy))
summary(reslm2)


rmse <- function(error)
{
  sqrt(mean(error^2))
}

mae <- function(error)
{
  mean(abs(error))
}

PMchap_2 = predict(reslm2, newdata=dataTest)
error2 = dataTest$PM10.moy - PMchap_2
rmse(error2)
mae(error2)


PMchap_2 = predict(reslm2)
error2 = dataAppreslm3$PM10.moy - PMchap_2
rmse(error2)
mae(error2)


source("Perfopm10.R")
reslm = reslm2

#modele step
Perfopm10(dataAppreslm3$PM10.moy, predict(reslm))
Perfopm10(dataTest$PM10.moy, predict(reslm, newdata = dataTest))


source("TabDeppm10.R")
TabDeppm10(dataAppreslm3$PM10.moy, predict(reslm),50,80,50)
TabDeppm10(dataTest$PM10.moy, predict(reslm, newdata = dataTest),50,80,50)


