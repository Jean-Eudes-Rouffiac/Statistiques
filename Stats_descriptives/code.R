library(moments)


file_name = 'covtype.data'
data<-read.table(file=file_name,header=F, sep = ",")


## 
Wilderness.Area<-rep(NA,581012)
for(i in 1:581012){Wilderness.Area[i]<-ifelse(data[i,11],"Rawah",ifelse(data[i,12],"Neota",ifelse(data[i,13],"Comanche Peak",ifelse(data[i,14],"Cache la Poudre",NA))))}
Wilderness.Area<-as.factor(Wilderness.Area)

Cover.Type<-as.factor(data[,55])
levels(Cover.Type)=c("Spruce/Fir","Lodgepole Pine","Ponderosa Pine","Cottonwood/Willow","Aspen","Douglas-fir","Krummholz")

Soil.Type<-rep(NA,581012)
for(i in 1:581012){Soil.Type[i]<-(1:40)[as.numeric(data[i,15:54])==1]}
Soil.Type<-as.factor(Soil.Type)


cov_type = data.frame(data[,1:10],Wilderness.Area,Soil.Type, Cover.Type)
attach(cov_type)


names(cov_type)=c("Altitude","Orientation","Pente","Distance_horizontale_point_eau", "Distance_verticale_point_eau", 
                 "Distance_horizontale_route", "Ombrage_9am","Ombrage_midi", "Ombrage_3pm","Distance_horizontale_feu","Zone_sauvage","Sol","Espece")


# Altitude

summary(cov_type$Altitude)
sd(cov_type$Altitude)
hist(cov_type$Altitude , breaks = 30, col= "orange" , prob = TRUE, cex.axis = 1,cex.lab = 1,ylab = "Fréquence", xlab = "Altitude",main = 'Histogramme des Fréquence des altitudes', , cex.main = 0.7)
lines(density(cov_type$Altitude), lwd = 2)
skewness(cov_type$Altitude)
kurtosis(cov_type$Altitude)
boxplot(cov_type$Altitude,col="orange",main="Diagramme en boîte Altitude")


# Orientation

summary(cov_type$Orientation)
sd(cov_type$Orientation)
hist(cov_type$Orientation , col= "orange" , prob = TRUE, cex.axis = 1,cex.lab = 1,ylab = "Fréquence", xlab = "Fréquence des Orientations",main = 'Histogramme des fréquences des Orientations')
lines(density(cov_type$Orientation), lwd = 2)
skewness(cov_type$Orientation)
kurtosis(cov_type$Orientation)
boxplot(cov_type$Orientation,col="orange",main="Diagramme en boîte des Orientations")


# Pente 
summary(cov_type$Pente)
sd(cov_type$Pente)
hist(cov_type$Pente , col= "orange" , prob = TRUE, cex.axis = 1,cex.lab = 1,ylab = "Fréquence", xlab = "Fréquence de la pente",main = 'Histogramme des fréquences de pente')
lines(density(cov_type$Pente), lwd = 2)
skewness(cov_type$Pente)
kurtosis(cov_type$Pente)
boxplot(cov_type$Pente,col="orange",main="Diagramme en boîte des pentes")



#Distance horizontale à un point d'eau
summary(cov_type$Distance_horizontale_point_eau)
sd(cov_type$Distance_horizontale_point_eau)
hist(cov_type$Distance_horizontale_point_eau , col= "orange" , prob = TRUE, cex.axis = 1.4,cex.lab = 1.5,ylab = "Fréquence", xlab = "Fréquence des distances horizontales à un point d'eau",main = "Histogramme des fréquences distances horizontales à un point d'eau")
lines(density(cov_type$Distance_horizontale_point_eau), lwd = 2)
skewness(cov_type$Distance_horizontale_point_eau)
kurtosis(cov_type$Distance_horizontale_point_eau)
boxplot(cov_type$Distance_horizontale_point_eau,col="orange",main="Diagramme en boîte des distances horizontales à un point d'eau")


#Distance verticale à un point d'eau
summary(cov_type$Distance_verticale_point_eau)
sd(cov_type$Distance_verticale_point_eau)
hist(cov_type$Distance_verticale_point_eau , col= "orange" , prob = TRUE, cex.axis = 1.4,cex.lab = 1.5,ylab = "Fréquence", xlab = "Fréquence des distances horizontales à une route",main = "Histogramme des fréquences distances verticales à une route")
skewness(cov_type$Distance_verticale_point_eau)
kurtosis(cov_type$Distance_verticale_point_eau)
boxplot(cov_type$Distance_verticale_point_eau,col="orange",main="Diagramme en boîte des distances verticales à un point d'eau")

#Distance horizontale à une route
summary(cov_type$Distance_horizontale_route)
sd(cov_type$Distance_horizontale_route)
hist(cov_type$Distance_horizontale_route , col= "orange" , prob = TRUE, cex.axis = 1.4,cex.lab = 1.5,ylab = "Fréquence", xlab = "Fréquence des distances horizontales à une route",main = "Histogramme des fréquences distances horizontales à une route")
lines(density(cov_type$Distance_horizontale_route), lwd = 2)
skewness(cov_type$Distance_horizontale_route)
kurtosis(cov_type$Distance_horizontale_route)
boxplot(cov_type$Distance_horizontale_route,col="orange",main="Diagramme en boîte des distances horizontales à une route")


#Ombrage à 9h
summary(cov_type$Ombrage_9am)
sd(cov_type$Ombrage_9am)
hist(cov_type$Ombrage_9am , col= "orange" , prob = TRUE, cex.axis = 1.4,cex.lab = 1.5,ylab = "Fréquence", xlab = "Fréquence de l'ombrage à 9am",main = "Histogramme des fréquences de l'ombrage à 9am")
lines(density(cov_type$Ombrage_9am), lwd = 2)
skewness(cov_type$Ombrage_9am)
kurtosis(cov_type$Ombrage_9am)
boxplot(cov_type$Ombrage_9am,col="orange",main="Diagramme en boîte de l'ombrage à 9am")

#Ombrage à midi
summary(cov_type$Ombrage_midi)
sd(cov_type$Ombrage_midi)
hist(cov_type$Ombrage_midi , col= "orange" , prob = TRUE, cex.axis = 1.4,cex.lab = 1.5,ylab = "Fréquence", xlab = "Fréquence de l'ombrage à midi",main = "Histogramme des fréquences de l'ombrage à midi")
lines(density(cov_type$Ombrage_midi), lwd = 2)
skewness(cov_type$Ombrage_midi)
kurtosis(cov_type$Ombrage_midi)
boxplot(cov_type$Ombrage_midi,col="orange",main="Diagramme en boîte de l'ombrage à midi")

#Ombrage à 3pm
summary(cov_type$Ombrage_3pm)
sd(cov_type$Ombrage_3pm)
hist(cov_type$Ombrage_3pm , col= "orange" , prob = TRUE, cex.axis = 1,cex.lab = 1,ylab = "Fréquence", xlab = "Fréquence de l'ombrage à 3pm",main = "Histogramme des fréquences de l'ombrage à 3pm", cex.main = 0.7)
lines(density(cov_type$Ombrage_3pm), lwd = 2)
skewness(cov_type$Ombrage_3pm)
kurtosis(cov_type$Ombrage_3pm)
boxplot(cov_type$Ombrage_3pm,col="orange",main="Diagramme en boîte de l'ombrage à 3pm")


#Distance horizontale à un feu
summary(cov_type$Distance_horizontale_feu)
sd(cov_type$Distance_horizontale_feu)
hist(cov_type$Distance_horizontale_feu, col= "orange" , prob = TRUE, cex.axis = 1.4,cex.lab = 1.5,ylab = "Fréquence", xlab = "Fréquence des distance horizontales à un feu",main = "Histogramme des fréquences des distance horizontales à un feu")
lines(density(cov_type$Distance_horizontale_feu), lwd = 2)
skewness(cov_type$Distance_horizontale_feu)
kurtosis(cov_type$Distance_horizontale_feu)
boxplot(cov_type$Distance_horizontale_feu,col="orange",main="Diagramme en boîte des distance horizontales à un feu" )



#Zone sauvage
res = table(cov_type$Zone_sauvage)
pie(res, clockwise=T, init.angle=-135,main="Zones sauvages")



#Espèce
res = table(cov_type$Espece)
pie(res, clockwise=T, init.angle=-135,main="Fréquence d'apparition des essences d'arbres")

table(factor(cov_type$Espece))
#Sol
res = table(cov_type$Sol)
barplot(res,col = "orange",main="Répartion par type de sol")
table(factor(cov_type$Sol, levels = 0:40))

x <- c(211840, 283301, 35754, 2747,9493 ,17367 , 20510 )
labels <- c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", "Cottonwood/Willow", "Aspen", "Douglas-fir", "Krummholz")
pct <- round(x/sum(x)*100)
lbls <- paste(labels, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(x,labels = lbls, col=rainbow(length(lbls)),main="city_pie_chart")
legend("topright", c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", "Cottonwood/Willow", "Aspen", "Douglas-fir", "Krummholz"), cex=0.8,fill=rainbow(length(x)))




library("rColorBrewer")
pielabels <- sprintf("%s = %3.1f%s", labels,
                     100*x/sum(x), "%")
pie(x,
    labels=NA,
    clockwise=TRUE,
    #col=brewer.pal(7,"Set1"),
    border="white",
    radius=0.7,
    cex=0.8,
    main="Fréquence d'apparition des essences d'arbres")
legend("bottomright",legend=pielabels,bty="n")


#corrélations avec Altitude
plot(cov_type$Altitude,cov_type$Orientation)
plot(cov_type$Altitude,cov_type$Pente)
plot(cov_type$Altitude,cov_type$Distance_horizontale_point_eau,type = 'p', col = 'blue', main="Tracé de la distance horizontale à un point d'eau en fonction de l'altitude",xlab="Altitude ", ylab="Distance horizontale à un point d'eau")
plot(cov_type$Altitude,cov_type$Distance_verticale_point_eau)
plot(cov_type$Altitude,cov_type$Distance_horizontale_route,type ='p',col = 'blue', main="Tracé de la distance horizontale à une route en fonction de l'altitude",xlab="Altitude ", ylab="Distance horizontale à une route")
plot(cov_type$Altitude,cov_type$Ombrage_9am)
plot(cov_type$Altitude,cov_type$Ombrage_midi)
plot(cov_type$Altitude,cov_type$Ombrage_3pm)
plot(cov_type$Altitude,cov_type$Distance_horizontale_feu,type ='p',col = 'blue', main="Tracé de la distance horizontale à un feu en fonction de l'altitude",xlab="Altitude ", ylab="Distance horizontale à un feu")
boxplot(cov_type$Altitude~cov_type$Zone_sauvage, ylab ="Altitude en m", xlab ="Zone sauvage étudiée", col = c("orange","blue","green","red"), cex.axis = 0.5,cex.lab = 0.5)
boxplot(cov_type$Altitude~cov_type$Espece, ylab ="Altitude en m",  col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"), cex.axis = 0.7,cex.lab = 0.7, las=2)
boxplot(cov_type$Altitude~cov_type$Sol, ylab ="Altitude en m", xlab ="Zone sauvage étudiée")

#corrélations avec Orientation
plot(cov_type$Orientation,cov_type$Pente)
plot(cov_type$Orientation,cov_type$Distance_horizontale_point_eau)
plot(cov_type$Orientation,cov_type$Distance_verticale_point_eau)
plot(cov_type$Orientation,cov_type$Distance_horizontale_route)
plot(cov_type$Orientation,cov_type$Distance_horizontale_feu)
plot(cov_type$Orientation,cov_type$Ombrage_9am)
plot(cov_type$Orientation,cov_type$Ombrage_midi)
plot(cov_type$Orientation,cov_type$Ombrage_3pm)
plot(cov_type$Orientation,cov_type$Distance_horizontale_feu)
boxplot(cov_type$Orientation~cov_type$Zone_sauvage, ylab ="Orientation de la parcelle en °", xlab ="Zone sauvage étudiée", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3"))
boxplot(cov_type$Orientation~cov_type$Espece, ylab ="Orientation de la parcelle en°", xlab ="Essence d'arbres", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"))
boxplot(cov_type$Orientation~cov_type$Sol, ylab ="Orientation de la parcelle en °", xlab ="Type de sol")


#corrélations avec Pente
plot(cov_type$Pente,cov_type$Distance_horizontale_point_eau)
plot(cov_type$Pente,cov_type$Distance_verticale_point_eau)
plot(cov_type$Pente,cov_type$Distance_horizontale_route)
plot(cov_type$Pente,cov_type$Ombrage_9am)
plot(cov_type$Pente,cov_type$Ombrage_midi)
plot(cov_type$Pente,cov_type$Ombrage_3pm, type ='p',col = 'blue', main="Tracé de l'indice d'ombrage à 3p.m. en fonction du pourcentage de pente d'une parcelle",xlab="Pente en% ", ylab="indice de l'ombrage à 3pm")
plot(cov_type$Pente,cov_type$Distance_horizontale_feu)
boxplot(cov_type$Pente~cov_type$Zone_sauvage, ylab ="Pente de la parcelle en%", xlab ="Zone sauvage étudiée", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3"))
boxplot(cov_type$Pente~cov_type$Espece, ylab ="Pente de la parcelle en %", xlab ="Essence d'arbres", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"))
boxplot(cov_type$Pente~cov_type$Sol, ylab ="Pente de la parcelle en %", xlab ="Type de sol")


#corrélations avec la distance horizontale à un point d'eau
plot(cov_type$Distance_horizontale_point_eau,cov_type$Distance_verticale_point_eau)
plot(cov_type$Distance_horizontale_point_eau,cov_type$Distance_horizontale_route)
plot(cov_type$Distance_horizontale_point_eau,cov_type$Ombrage_9am)
plot(cov_type$Distance_horizontale_point_eau,cov_type$Ombrage_midi)
plot(cov_type$Distance_horizontale_point_eau,cov_type$Ombrage_3pm, type ='p',col = 'blue', main="Tracé de l'indice d'ombrage à 3p.m. en fonction de la distance horizontale à un point d'eau",xlab="Distance horizontale à un point d'eau en m" , ylab="indice de l'ombrage à 3pm")
plot(cov_type$Distance_horizontale_point_eau,cov_type$Distance_horizontale_feu)
boxplot(cov_type$Distance_horizontale_point_eau~cov_type$Zone_sauvage, ylab ="Distance horizontale à un point d'eau en m", xlab ="Zone sauvage étudiée", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3"))
boxplot(cov_type$Distance_horizontale_point_eau~cov_type$Espece, ylab ="Distance horizontale à un point d'eau en m", xlab ="Essence d'arbres", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"))
boxplot(cov_type$Distance_horizontale_point_eau~cov_type$Sol, ylab ="Distance horizontale à un point d'eau en m", xlab ="Type de sol")

#corrélations avec la distance verticale à un point d'eau
plot(cov_type$Distance_verticale_point_eau,cov_type$Distance_horizontale_route)
plot(cov_type$Distance_verticale_point_eau,cov_type$Ombrage_9am)
plot(cov_type$Distance_verticale_point_eau,cov_type$Ombrage_midi)
plot(cov_type$Distance_verticale_point_eau,cov_type$Ombrage_3pm)
plot(cov_type$Distance_verticale_point_eau,cov_type$Distance_horizontale_feu)
boxplot(cov_type$Distance_verticale_point_eau~cov_type$Zone_sauvage, ylab ="Distance verticale à un point d'eau en m", xlab ="Zone sauvage étudiée", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3"))
boxplot(cov_type$Distance_verticale_point_eau~cov_type$Espece, ylab ="Distance verticale à un point d'eau en m", xlab ="Essence d'arbres", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"))
boxplot(cov_type$Distance_verticale_point_eau~cov_type$Sol, ylab ="Distance verticale à un point d'eau en m", xlab ="Type de sol")

#corrélations avec la distance horizontale à une à une route
plot(cov_type$Distance_horizontale_route,cov_type$Ombrage_9am)
plot(cov_type$Distance_horizontale_route,cov_type$Ombrage_midi)
plot(cov_type$Distance_horizontale_route,cov_type$Ombrage_3pm)
plot(cov_type$Distance_horizontale_route,cov_type$Distance_horizontale_feu)
boxplot(cov_type$Distance_horizontale_route~cov_type$Zone_sauvage, ylab ="Distance horizontale à une route en m", xlab ="Zone sauvage étudiée", col = c("orange","blue","green","red"), cex.axis = 0.5,cex.lab = 0.5)
boxplot(cov_type$Distance_horizontale_route~cov_type$Espece, ylab ="Distance horizontale à une route en m", xlab ="Essence d'arbres", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"))
boxplot(cov_type$Distance_horizontale_route~cov_type$Sol, ylab ="Distance horizontale à à une route en m", xlab ="Type de sol")

#corrélations avec la distance horizontale à un feu
plot(cov_type$Distance_horizontale_feu,cov_type$Ombrage_9am)
plot(cov_type$Distance_horizontale_feu,cov_type$Ombrage_midi)
plot(cov_type$Distance_horizontale_feu,cov_type$Ombrage_3pm)
boxplot(cov_type$Distance_horizontale_feu~cov_type$Zone_sauvage, ylab ="Distance horizontale à un feu en m", xlab ="Zone sauvage étudiée", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3"))
boxplot(cov_type$Distance_horizontale_feu~cov_type$Espece, ylab ="Distance horizontale à un feu en m", xlab ="Essence d'arbres", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"))
boxplot(cov_type$Distance_horizontale_feu~cov_type$Sol, ylab ="Distance horizontale à un feu en m", xlab ="Type de sol")

#corrélations avec l'indice d'ombrage à 9pm.
plot(cov_type$Ombrage_9am,cov_type$Ombrage_midi)
plot(cov_type$Ombrage_9am,cov_type$Ombrage_3pm)
boxplot(cov_type$Ombrage_9am~cov_type$Zone_sauvage, ylab ="indice d'ombrage à 9pm", xlab ="Zone sauvage étudiée", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3"))
boxplot(cov_type$Ombrage_9am~cov_type$Espece, ylab ="indice d'ombrage à 9pm", xlab ="Essence d'arbres", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"))
boxplot(cov_type$Ombrage_9am~cov_type$Sol, ylab ="indice d'ombrage à 9pm", xlab ="Type de sol")


#corrélations avec l'indice d'ombrage à midi.
plot(cov_type$Ombrage_midi,cov_type$Ombrage_3pm)
boxplot(cov_type$Ombrage_midi~cov_type$Zone_sauvage, ylab ="indice d'ombrage à midi", xlab ="Zone sauvage étudiée", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3"))
boxplot(cov_type$Ombrage_midi~cov_type$Espece, ylab ="indice d'ombrage à midi", xlab ="Essence d'arbres", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"))
boxplot(cov_type$Ombrage_midi~cov_type$Sol, ylab ="indice d'ombrage à midi", xlab ="Type de sol")


#corrélations avec l'indice d'ombrage à 3pm.
boxplot(cov_type$Ombrage_3pm~cov_type$Zone_sauvage, ylab ="indice d'ombrage à 3pm", xlab ="Zone sauvage étudiée", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3"))
boxplot(cov_type$Ombrage_3pm~cov_type$Espece, ylab ="indice d'ombrage à 3pm", xlab ="Essence d'arbres", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"))
boxplot(cov_type$Ombrage_3pm~cov_type$Sol, ylab ="indice d'ombrage à 3pm", xlab ="Type de sol")

#corrélations avec la Zone.

table(cov_type$Zone_sauvage,cov_type$Espece)
table(cov_type$Zone_sauvage,cov_type$Sol)

#corrélation avec Espèce
table(cov_type$Espece,cov_type$Sol)







#Dans rapport

boxplot(cov_type$Altitude~cov_type$Zone_sauvage, ylab ="Altitude en m", xlab ="                        Zone sauvage étudiée", col = c("orange","blue","green","red"), cex.axis = 0.7,cex.lab = 0.7, las=2)
boxplot(cov_type$Distance_horizontale_route~cov_type$Zone_sauvage, ylab ="Distance horizontale à une route en m", xlab ="                    Zone sauvage étudiée", col = c("orange","blue","green","red"), cex.axis = 0.7,cex.lab = 0.7, las=2)
boxplot(cov_type$Distance_horizontale_feu~cov_type$Zone_sauvage, ylab ="Distance horizontale à un feu en m", xlab ="                    Zone sauvage étudiée", col = c("orange","blue","green","red"), cex.axis = 0.7,cex.lab = 0.7, las=2)


boxplot(cov_type$Altitude~cov_type$Espece, ylab ="Altitude en m",  col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"), cex.axis = 0.7,cex.lab = 0.7, las=2)
boxplot(cov_type$Pente~cov_type$Espece, ylab ="Pente de la parcelle en %", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"), cex.axis = 0.7,cex.lab = 0.7, las=2)
boxplot(cov_type$Distance_horizontale_point_eau~cov_type$Espece, ylab ="Distance horizontale à un point d'eau en m",  col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"), cex.axis = 0.7,cex.lab = 0.7, las=2)
boxplot(cov_type$Distance_horizontale_route~cov_type$Espece, ylab ="Distance horizontale à une route en m", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"), cex.axis = 0.7,cex.lab = 0.7, las=2)
boxplot(cov_type$Distance_horizontale_feu~cov_type$Espece, ylab ="Distance horizontale à un feu en m", col = c("cadetblue4","chocolate3","coral2","darkolivegreen3","goldenrod3","bisque3","darkslategray3"), cex.axis = 0.7,cex.lab = 0.7, las=2)


library(Kendall)
dv<-cbind(cov_type$Espece,cov_type$Sol)
dv<-na.omit(dv)
Kendall(as.factor(dv[,1]), as.factor(dv[,2]))

