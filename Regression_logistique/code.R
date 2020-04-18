# Partie 1

# initialisation
p = 49 # Nombre de variables explicatives
moyx = matrix(nrow = 1, ncol= p, 0)
varx = matrix(nrow = 1, ncol= p, 0)
n = 0
# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile = file("covtype_app.csv", open = "r")
while(length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  zz <- scan(text=oneLine, quiet=T, sep=";")
  X = zz[1:p]
  # Calcul itératif de la moyenne et de la variance pour les variables explicatives
  n = n + 1
  #Variance
  varx <- varx + (1/(n))*(((n-1)/(n))*(X - moyx)*(X - moyx) - varx)
  
  #Moyenne
  moyx<-((n-1)*moyx+X)/n
  
}
close(nfile)
# Calcul du vecteur des écart-types
sigmax = sqrt(varx)



## PARTIE 2

# Definition de la fonction pi(z)
pi <- function(x){
  return(1/(1+exp(-x)))
}

#Initialisation
p=49
lambda = 1
Q<-lambda*diag(p+1)
theta<-rep(0,p+1)
# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile = file("covtype_app.csv", open = "r")
while(length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  zz <- scan(text=oneLine, quiet=T, sep=";")
  X = zz[1:p]
  Y = zz[p+1]
  
  #On centre et réduit les données avec la moyenne et écart type calculés itérativement
  phi = c(1, as.numeric((X-moyx)/sigmax))

  alpha<-pi(as.numeric(t(theta)%*%phi))/(1-pi(as.numeric(t(theta)%*%phi)))
  Q<-Q-alpha/as.numeric(1+alpha*t(phi)%*%Q%*%phi)*(Q%*%phi%*%t(phi)%*%Q)
  theta<-theta+Q%*%phi*as.numeric(Y-pi(sum(t(theta)%*%phi)))
}
close(nfile)


# Calcul des intervalles de confiance à 95%
IC.binf = theta - qnorm(1 - 0.05/2, mean = 0) * sqrt(diag(Q)) # Vecteur des bornes inférieures
IC.bsup = theta + qnorm(1 - 0.05/2, mean = 0) * sqrt(diag(Q)) # Vecteur des bornes supérieures
# On retire la constante
IC.binf = IC.binf[-1]
IC.bsup = IC.bsup[-1]
# Identification des variables significatives
varsignif = (1:p)[0 < IC.binf | 0 > IC.bsup]
varsignif
sum(varsignif)


# LE test consiste à enlever les variables explicatives dont 0 n'est pas dans l'intervalle de confiance de l'estimateur.
# On supprime donc du modèle els variables explicatives qui n'ont pas d'influence sur la variable à expliquer. Seulement 18 variables explicatives sont alors gardées, ce qui permet de réduire significativement le nombre de variables dans le modèle.



# initialisation
seuil = 0.5
N00 = 0
N01 = 0
N10 = 0
N11 = 0



# Connexion au fichier puis boucle de lecture s´equentielle, ligne par ligne
nfile = file("covtype_tst.csv", open = "r")
while (length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  zz = scan(text=oneLine, quiet=T, sep=";")
  # centrage-réduction des variables explicatives
  X = zz[1:p]
  Y = zz[p+1]
  phi =  c(1, as.numeric((X-moyx)/sigmax))

  # calcul de la prédiction
  
  ychap = t(theta) %*% phi
  
  if (pi(ychap) > seuil){
    if (Y == 0) {
      N01 = N01 + 1
    }
    if (Y == 1){
      N11 = N11 + 1
    }
  }
  
  if (pi(ychap) < seuil){
    if (Y == 0) {
      N00 = N00 + 1
    }
    if (Y == 1){
      N10 = N10 + 1
    }
  }
  
}
close(nfile)

N = N00 + N10 + N01 + N11

taux_erreurs = (N01+N10)/N
specificite = N11/(N10+N11)
sensibilite = N00/(N00+N01)

resultat <- matrix(ncol=3,nrow=1)
colnames(resultat)<-c("Taux erreurs", "Specificités", "Sensibilité")
resultat[1,] = c(taux_erreurs,specificite,sensibilite)

#On trouve un taux d'erreur de 0.29, ce qui est assez grand. On ne peut pas dire que le modèle classifie les arbres de façon bonne, mais cela reste correct.
# La spécificité de 0.71 indique que lorsqu'un arbre sera un pin tordu, la prédiction sera juste dans 71% des cas. Une sensibilité de 0.69 indique que le modèle prédit correctemment 69% des arbres pins.
# Ceci renforce que le modèle n'est pas excellent mais reste correct pour classer les arbres.



## Partie centrer que variables quantitatives

#Initialisation
p=49
lambda = 1
Q<-lambda*diag(p+1)
theta<-rep(0,p+1)
# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile = file("covtype_app.csv", open = "r")
while(length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  zz <- scan(text=oneLine, quiet=T, sep=";")
  X_quant = zz[1:10]
  X_bin = zz[11:p]
  Y = zz[p+1]
  
  #On centre et réduit les variables quantitatives avec la moyenne et écart type calculés itérativement
  
  X_quant_norm = as.numeric((X_quant-moyx[1:10])/sigmax[1:10])
  phi = c(X_quant_norm,X_bin)
  phi = c(1, phi)
  
  alpha<-pi(as.numeric(t(theta)%*%phi))/(1-pi(as.numeric(t(theta)%*%phi)))
  Q<-Q-alpha*as.numeric(1+alpha*t(phi)%*%Q%*%phi)^(-1)*(Q%*%phi%*%t(phi)%*%Q)
  theta<-theta+Q%*%phi*as.numeric(Y-pi(sum(t(theta)%*%phi)))
}
close(nfile)

# initialisation
seuil = 0.5
N00 = 0
N01 = 0
N10 = 0
N11 = 0



# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile = file("covtype_tst.csv", open = "r")
while (length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  zz = scan(text=oneLine, quiet=T, sep=";")
  # centrage-réduction des variables explicatives
  X = zz[1:p]
  Y = zz[p+1]
  phi = phi = c(1, as.numeric((X-moyx)/sigmax))
  
  # calcul de la prédiction
  
  ychap = phi %*% theta
  
  if (pi(ychap) >= seuil){
    if (Y == 0) {
      N01 = N01 + 1
    }
    else{
      N11 = N11 + 1
    }
  }
  
  if (pi(ychap) <= seuil){
    if (Y == 0) {
      N00 = N00 + 1
    }
    else{
      N10 = N10 + 1
    }
  }
  
}
close(nfile)

N = N00 + N10 + N01 + N11

taux_erreurs = (N01+N10)/N
specificite = N11/(N10+N11)
sensibilite = N00/(N00+N01)

resultat2 <- matrix(ncol=3,nrow=1)
colnames(resultat2)<-c("Taux erreurs", "Specificités", "Sensibilité")
resultat2[1,] = c(taux_erreurs,specificite,sensibilite)

# On remarque que le taux d'erreur reste le même alors que la sensibilité et la précision ont été modifiés par rapport au modèle précédent (+ ou _ 2%)
# Il n'est donc pas nécessaire de centrer et réduire toutes les variables explicatives. Centrer ét réduire seulement les variables quantitatives donne des résultats similaires.


## Partie gradient stochastique


#Initialisation
p=49
lambda = 1
theta<-rep(0,p+1)
n = 0
# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile = file("covtype_app.csv", open = "r")
while(length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  zz <- scan(text=oneLine, quiet=T, sep=";")
  X = zz[1:p]
  Y = zz[p+1]
  n = n + 1
  #On centre et réduit les données avec la moyenne et écart type calculés itérativement
  phi = c(1, as.numeric((X-moyx)/sigmax))
  gamma = n ^(-0.9)
  theta = theta - gamma * phi %*% (pi(t(theta) %*% phi) - Y)
}
close(nfile)


# initialisation
seuil = 0.5
N00 = 0
N01 = 0
N10 = 0
N11 = 0



# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile = file("covtype_tst.csv", open = "r")
while (length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  zz = scan(text=oneLine, quiet=T, sep=";")
  # centrage-réduction des variables explicatives
  X = zz[1:p]
  Y = zz[p+1]
  phi = phi = c(1, as.numeric((X-moyx)/sigmax))
  
  # calcul de la prédiction
  
  ychap = phi %*% theta
  
  if (pi(ychap) >= seuil){
    if (Y == 0) {
      N01 = N01 + 1
    }
    else{
      N11 = N11 + 1
    }
  }
  
  if (pi(ychap) <= seuil){
    if (Y == 0) {
      N00 = N00 + 1
    }
    else{
      N10 = N10 + 1
    }
  }
  
}
close(nfile)

N = N00 + N10 + N01 + N11

taux_erreurs = (N01+N10)/N
specificite = N11/(N10+N11)
sensibilite = N00/(N00+N01)

resultat3 <- matrix(ncol=3,nrow=1)
colnames(resultat3)<-c("Taux erreurs", "Specificités", "Sensibilité")
resultat3[1,] = c(taux_erreurs,specificite,sensibilite)

#Avec le gradient stochastique, nous obtenons de meilleurs résultats. Le taux d'erreur est descendu à 23%.
# La spécificité a augmenté est est maintenant à 75% et la sensibilité à 78%. Utiliser le gradient stochastique permet dans mon cas d'obtenir de meilleurs résulats.