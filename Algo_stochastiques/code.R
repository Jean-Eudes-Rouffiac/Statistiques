library("RCurl")

data_inconnue <- "http://crihan.airnormand.fr/inconnue.php"

bxp2 <- function(mini,q1,q2,q3,maxi)
{
  w1=q1-1.5*(q3-q1)
  w2=q3+1.5*(q3-q1)
  plot.new()
  plot.window(xlim=range(1L:1, finite = TRUE) + c(-0.5, 0.5),ylim=c(min(w1,mini),max(w2,maxi)))
  axis(1)
  axis(2)
  bxp(list(stats=cbind(c(max(w1,mini),q1,q2,q3,min(w2,maxi))),n=1,conf=cbind(c(w1,w2)),out=c(mini,maxi),group=rep(1,2)))
}

#-----------------------------------------------------
#Calcul séquentiel des indicateurs statistiques de base
#-----------------------------------------------------

#Initialisation des variables
min<-+Inf
max<--Inf
mean<-0
var<-0
q1=0
q2=0
q3=0
a1<-1.0/4.0
a2<-1.0/2.0
a3<-3.0/4.0

resultat <- matrix(ncol=8,nrow=1)
colnames(resultat)<-c("Valeur", "Min", "Mean", "Var", "Max", "q1", "q2", "q3")

for (i in 1:200) {
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
  mean<- ((i-1) /i) * mean + x / i
  
  gamma<-1/((i+1)^0.9)
  
  # Calcul du 1er quartile
  ind <- ifelse((x<q1), 1, 0)
  q1 <- q1-gamma*(ind-a1)
  
  # Calcul de la médiane
  ind <- ifelse((x<q2), 1, 0)
  q2 = q2-gamma*(ind-a2)
  
  # Calcul du 3ème quartile
  ind <- ifelse((x<q3), 1, 0)
  q3=q3-gamma*(ind-a3)
  
  #Affichage boxplot à chaque itération
  bxp2(min,q1,q2,q3,max)
  
}

#Affichage résultat final
resultat[1,]=c(x, min, mean, var, max, q1, q2, q3)
print(resultat)


#----------------------------------------
# Recherche du alpha qui conduit à la meilleure estimation des vraies valeurs des quantiles
#----------------------------------------

resultat2 <- matrix(ncol=4,nrow=1)

#Comparaion des a dans le calcul de gamma
comparaison_a = matrix(ncol=1,nrow=100)
comparaison_abs = matrix(ncol=1,nrow=100)

data_inconnue <- "http://crihan.airnormand.fr/inconnue.php"

#Initialisation des variables utiles au choix de la puissance "a" dans le gamma
mediane_def= 10000
i=0
#Boucle pour choisir le meilleur a
for(a in seq(from=0.1, to=0.9, by=0.1)){
  #Initialisation des quartiles
  q1=0
  q2=0
  q3=0
  a1<-1.0/4.0
  a2<-1.0/2.0
  a3<-3.0/4.0
  
  for (n in 1:200) {
    
    x <- as.numeric(getURL(data_inconnue))
    gamma<-1/((n+1)^a)
    
    # Calcul du 1er quartile
    ind <- ifelse((x<q1), 1, 0)
    q1=q1-gamma*(ind-a1)
    
    # Calcul de la médiane
    ind <- ifelse((x<q2), 1, 0)
    q2=q2-gamma*(ind-a2)
    
    # Calcul du 3ème quartile
    ind <- ifelse((x<q3), 1, 0)
    q3=q3-gamma*(ind-a3)
  }
  
  comparaison_a[i] = a 
  comparaison_abs[i] = abs(q2- 0.7832198)
  i=i+1
  
  if (abs(q2-0.7832198)<abs(mediane_def-0.7832198)){
    q1_def = q1
    q3_def = q3
    mediane_def = q2
    best_a = a
    print(a)
  }
}

colnames(resultat2)<-c("a", "q1", "q2", "q3")
resultat2[1,]=c(best_a, q1_def,mediane_def,q3_def)
plot(comparaison_a, comparaison_abs, type="l", col="red", main="Evolution de l'erreur en fonction de a", ylab = "Erreur", xlab = "a",lwd=2)

# Pour trouver le meilleur a possible, j'ai fais une boucle de a allant de 0.1 à 
# 0.9, j'ai alors calculé séquentiellement q1, q2 et q3. Pour trouver le meilleur a 
# j'ai comparé à chaque fois la valeur absolue de la différence entre la médiane calculée
# itérativement et la valeure exacte de la médiane. J'ai stocké tout cela dans deux tableaux, 
# ce qui m'a permis de tracer une coubre d'erreur en fonction de a. Finalement, 
# la valeure obtimale de a est 0.9.