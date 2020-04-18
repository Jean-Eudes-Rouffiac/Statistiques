library("RCurl")

getURL("http://crihan.airnormand.fr/indice_ATMO.php?agglo=rouen")



# Lecture séquentielle du fichier

#Partie 1

#Question 1

altitude = matrix(ncol=1,nrow=100)
cover_type = matrix(ncol=1,nrow=100)
i = 1

nfile  <- file("covtype.data", open = "r")

while (length(oneLine <- readLines(nfile, n = 1)) > 0 && i < 100) {
  X <- scan(text=oneLine, sep=',') # Acquisition d’une nouvelle donnée
  altitude[i] = X[1]
  cover_type[i] = X[55]
  i = i + 1
}

# Fermeture du fichier
close(nfile)
  
summary(altitude)
sd(altitude)
hist(altitude , breaks = 30, col= "orange" , prob = TRUE, cex.axis = 1,cex.lab = 1,ylab = "Fréquence", xlab = "Altitude",main = 'Histogramme des Fréquence des altitudes', , cex.main = 0.7)
lines(density(altitude), lwd = 2)
boxplot(altitude,col="orange",main="Diagramme en boîte Altitude")


res = table(cover_type)
pie(res, clockwise=T, init.angle=-135,main="Fréquence d'apparition des essences d'arbres")



#Question 2


data_inconnue <- "http://crihan.airnormand.fr/inconnue.php"
data_couleurs <- "http://crihan.airnormand.fr/couleurs.php"

vecteur_inconnue = matrix(ncol=1, nrow=100)
vecteur_couleurs = matrix(ncol=1, nrow=100)

for (i in 1:100) {
  x_inconnue <- as.numeric(getURL(data_inconnue))
  x_couleurs <- (getURL(data_couleurs))
  
  #On enregistre x
  vecteur_inconnue[i] = x_inconnue
  vecteur_couleurs[i] = x_couleurs
  
}

summary(vecteur_inconnue)
sd(vecteur_inconnue)
hist(vecteur_inconnue , breaks = 30, col= "orange" , prob = TRUE, cex.axis = 1,cex.lab = 1,ylab = "Fréquence", xlab = "Altitude",main = 'Histogramme des Fréquence des altitudes', , cex.main = 0.7)
lines(density(vecteur_inconnue), lwd = 2)
boxplot(vecteur_inconnue,col="orange",main="Diagramme en boîte Altitude")


res = table(vecteur_couleurs)
pie(res, clockwise=T, init.angle=-135,main="Fréquence d'apparition des essences d'arbres")


#Question 3


nfile  <- file("covtype.data", open = "r")

i = 0

while (length(oneLine <- readLines(nfile, n = 1)) > 0 && (i<100)) {
  X <- scan(text=oneLine, sep=',') # Acquisition d’une nouvelle donnée
  print(X)
  flush.console()
  i = i + 1
}

# Fermeture du fichier
close(nfile)



#Partie 2

# Question 4



data_inconnue <- "http://crihan.airnormand.fr/inconnue.php"

#Initialisation des variables
min<-+Inf
max<--Inf
mean<-0
var<-0

resultat <- matrix(ncol=5,nrow=1)
colnames(resultat)<-c("Valeur", "Min", "Mean", "Var", "Max")

for (i in 1:100) {
  x <- as.numeric(getURL(data_inconnue))
  
  #Max
  if(x>max)
    max<-x
  
  #Min
  if (x<min)
    min<-x
  
  #Variance
  var <- var + (1/(i))*((i/(i+1))*(x - mean)*(x - mean) - var)
    
  #Moyenne
  mean<-((i-1)*mean+x)/i
  

  
  resultat[1,]=c(x, min, mean, var, max)
  flush.console()
  print(resultat)
}


#Question 5 & 6


barhist <- function(bornes,freq,col=NA,...)
{
  n=length(bornes)
  FF=freq/diff(bornes)
  plot.new()
  plot.window(xlim=c(bornes[1],bornes[n]),ylim=c(0,max(FF)))
  axis(1)
  axis(2)
  rect(bornes[1:(n-1)],rep(0,n-1),bornes[2:n],FF,col=col)
}


data_inconnue <- "http://crihan.airnormand.fr/inconnue.php"

#Initialisation des variables
min<-0
max<-4

K <- 10 #Nombre de points

a <- matrix(nrow=K, ncol = 1) # Initialisation des a_ij, qui définissent les intervalles de l'hisrogramme

# Caclul des a_ij. On a maintenant séparé min + max en K-1 intervalles égaux
for (i in 1:K) {
  a[i] = min + (i - 1)*(max - min)/K
}

# Initialisation du vecteur fréquence. K - 1 composantes
# La composante 1 sera donc la fréquence des données X dans l'intervalle [a_0;a_1[
# La composante 2 sera donc la fréquence des données X dans l'intervalle [a_1;a_2[
# La composante K - 1 sera donc la fréquence des données X dans l'intervalle [a_k-1;a_k[
#Mis à jour à chaque itération
f <- cbind(rep(0, K-1)) 

# Paramètre pour la fenêtre lors de l'estimation de la densité. La valeur de alpha est donné dans l'énoncé
alpha=0.2

grille<-seq(round(min),round(max)+1, 0.05) #Grille sur laquelle sera estimée la densité

#Estimateur de la densité, c'est un vecteur de longueur de la grille choisie
#Pour chaque point de la grille, il a une valeur correspondante
#Lorsque qu'une valeur X est passée, la courbe de l'estimation de la densité va se "relever" au point d'abscisse (de la grille) le plus proche de X
FR<-rep(0,length(grille))

#On lit séquentiellement les données du fichier data_inconnue
for (i in 1:200) {
  x <- as.numeric(getURL(data_inconnue))
  
  #Initialisation de la fonction indicatrice. C'est un vecteur de taille K-1
  indic <- matrix(nrow=K-1, ncol = 1)
  
  #Si la donnée K appartient à l'intervalle [a_0;a_1[, la composante 1 de l'indiactrice prendra la valeur 1, le reste des composantes 0
  #Idem pour les autres cas
  for (j in 1:K-1) {
    if((x >= a[j]) && (x< a[j+1] )){
      indic[j] = 1
    }
    
    else{
      indic[j] = 0
    }
  }
  
  #Mis à jour de la fréquence itérativement
  f = f + (1/(1+i)) * (indic - f)
  
  # Affichage de l'histogramme avec entrée le vecteur a (les bornes) et f le vecteur des nouvelles fréquences
  barhist(a, f)
  
  # Calcul de l'esimation de la densité par noyau (méthode de Parzen-Rosenblatt)
  
  #Fenêtre
  hn<-i^(-alpha)
  #Noyau
  K_kernel<-dnorm((x-grille)/hn)
  #Estimateur
  FR<-FR+(1/i)*((K_kernel/hn)-FR)
  #Affichage de la densité par dessus l'histogramme
  lines(x = grille, FR)

}


#Question 7


nfile  <- file("covtype.data", open = "r")

i = 1

#Initialisation du vecteur fréquence de 7 composantes car 7 modalités dans la variable qualitative
f <- cbind(rep(0, 7)) 


while (length(oneLine <- readLines(nfile, n = 1)) > 0 && (i<10000)) {
  X <- scan(text=oneLine, sep=',') # Acquisition d’une nouvelle donnée
  
  #Initialisation du la fonction indicatrice
  indic <- matrix(nrow=7, ncol = 1)
  
  #Même principe que question 5, si la donnée lue séquentiellement est égale à la modalité j, alors la j ème composante du vecteur indic prend la valeur 1, les autres composantes 0
  #X[55] est la variable qualitative
  for (j in 1:7) {
    if(X[55] == j){
      indic[j] = 1
    }
    
    else{
      indic[j] = 0
    }
  }
  
  #Mis à jour de la fréquence séquentiellement
  f = f + (1/(i+1))*(indic - f)
  
  #Incrémentation de i. Le programme s'arrête quand i = 200
  i = i + 1
}

res = table(f)
pie(res, clockwise=T, init.angle=-135,main="Fréquence d'apparition des essences d'arbres")

  
# Fermeture du fichier
close(nfile)


#Question  8



data_inconnue <- "http://crihan.airnormand.fr/couleurs.php"

#Initialisation du vecteur var_names qui est le vecteur qui stocke le nom des modalités
#A chaque itération on vérifiera si la modalité appartient au vecteur, si elle n'appartient pas, on ajoutera la modalité au vecteur
var_names = matrix(nrow=0, ncol = 1)

#Initialisation du vecteur fréquence. 
#Initialisation à 0 car modalités inconnues
f <- matrix(nrow=0, ncol = 1)

'%ni%' <- Negate('%in%')

for (i in 1:100) {
  x <- (getURL(data_inconnue))
  
  #Si la modalité x est inconnue, on l'ajoute au vecteur var_names
  #On ajoute donc aussi une ligne au vecteur fréquence qui prend la valeur 0
  if (x %ni% var_names) {
    var_names = rbind(var_names, x)
    f = rbind(f, 0)
  }
  
  #Initialisation du la fonction indicatrice qui est un vecteur
  # Nombre de composantes = nombre de modalité dans var_names
  indic <- matrix(nrow=length(var_names), ncol = 1)
  
  #Même principe que question 5, si la donnée lue séquentiellement est égale à la modalité j, alors la j ème composante du vecteur indic prend la valeur 1, les autres composantes 0
  for (j in 1:length(var_names)) {
    if(var_names[j] == x){
      indic[j] = 1
    }
    
    else{
      indic[j] = 0
    }
  }
  
  #Mis à jour de la fréquence séquentiellement
  f = f + (1/(i+1))*(indic - f)
  
  #Incrémentation de i. Le programme s'arrête quand i = 200
  i = i + 1
  
  #Affichage du camembert au fur et à mesure
  res = table(f)
  pie(res, labels = var_names, clockwise=T, init.angle=-135,main="Fréquence d'apparition des couleurs")
  
}

