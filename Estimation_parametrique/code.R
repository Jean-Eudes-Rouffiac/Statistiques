## MS ESD - TP2


### Partie 1 : Estimation ponctuelle de la moyenne et de la variance """

B <- 400 ; N <- 1000
mu <- 0
data <- matrix(NA, ncol=B, nrow=N)
for(b in 1:B){
  data[,b] = rnorm(N,mean=0,sd=1)
}

M = matrix(NA, nrow=N, ncol=B)
S2 = matrix(NA, nrow=N, ncol=B)

for(j in 1:B){
  M[1, j] = data[1, j]
  S2[1, j] = 0
  for(i in 2:N){
    M[i, j] <- mean(data[1:i,j])
    S2[i, j] <- var(data[1:i,j])
  }
}

boxplot(t(M[c(50,100,200,500,1000),]),
names=c("n=50","n=100","n=200","n=500","n=1000"), col = "orange", cex.axis = 0.85, cex.main = 0.8, main = "Comportement de l'estimateur de la moyenne")
abline(h = 0, col="red", lwd = 2)

boxplot(t(S2[c(50,100,200,500,1000),]),
        names=c("n=50","n=100","n=200","n=500","n=1000"), col = "orange", cex.main = 0.8, cex.axis = 0.85, main = "Comportement de l'estimateur de la variance")
abline(h = 1, col="red", lwd = 2)


valn <- c(50,100,200,500,1000)
res <- NULL
for(n in valn){
  qtn <- qt(0.975, n-1)
  cpt <- 0
  for(j in 1:B){
    borne.inf <- M[n, j] - qtn*sqrt(S2[n, j])/sqrt(n)
    borne.sup <- M[n, j] + qtn*sqrt(S2[n, j])/sqrt(n)
      if(mu < borne.inf | mu > borne.sup){cpt <- cpt + 1}
  }
  res <- c(res, cpt)
}

res_pourcentage = res/4
tableau = rbind(ValeurN = valn, Résultat = res, Résultat2 = res_pourcentage)
  
## Partie 3
library(moments)
B <- 400 ; N <- 1000
mu <- 0
dataN <- matrix(NA, ncol=B, nrow=N)
for(b in 1:B){
  dataN[,b] = rnorm(N,mean=0,sd=1)
}

SK = matrix(NA, nrow=N, ncol=B)
KU = matrix(NA, nrow=N, ncol=B)

for(j in 1:B){
  SK[1, j] = dataN[1, j]
  KU[1, j] = 0
  for(i in 2:N){
    SK[i, j] <- skewness(dataN[1:i,j])
    KU[i, j] <- kurtosis(dataN[1:i,j])
  }
}

boxplot(t(SK[c(50,100,200,500,1000),]),
        names=c("n=50","n=100","n=200","n=500","n=1000"), col = "orange", cex.axis = 0.85, cex.main = 0.8, main = "Comportement de l'estimateur du coefficient d'asymétrie")
abline(h = 0, col="red", lwd = 2)

boxplot(t(KU[c(50,100,200,500,1000),]),
        names=c("n=50","n=100","n=200","n=500","n=1000"), col = "orange", cex.main = 0.8, cex.axis = 0.85, main = "Comportement du coefficient d'aplatissement")
abline(h = 3, col="red", lwd = 2)



### Partie 4 : Estimateur du maximum de vraisemblance dans le cadre de la loi de Weibull standard ###

B <- 400 ; N <- 1000
dataW <- matrix(NA, ncol=B, nrow=N)
for(b in 1:B){
  dataW[,b] = rweibull(N,1.5,1)
}

g_prime <- function(z, x){
  n = length(x)
  return(-(n/z)- sum(log(x)) + sum(log(x)*x^z))
} 

g_seconde <- function(z, x){
  n = length(x)
  return((n/(z*z)) + sum(log(x)*log(x)*x^z) + sum(log(x)*x^z))
} 

est_teta <- function(x){
  z = 1
  k = 0
  z1 = z - (g_prime(z, x) / g_seconde(z, x))
  while ((abs(z1 - z)) > 0.00001  && k < 100)  {
    z = z1
    z1 = z - (g_prime(z, x) / g_seconde(z, x))
    k = k + 1
  }
  print(k)
  return(z1)
}

theta = matrix(NA, nrow=5, ncol=B)

#for(j in 1:B){
 # theta[1, j] = dataW[1,j]
  #for(i in 2:N){
   # theta[i, j] <- est_teta(dataW[1:i,j])
  #}
#}

for(j in 1:B){
  theta[1, j] = est_teta(dataW[1:50,j])
  theta[2, j] <- est_teta(dataW[1:100,j])
  theta[3, j] <- est_teta(dataW[1:200,j])
  theta[4, j] <- est_teta(dataW[1:500,j])
  theta[5, j] <- est_teta(dataW[1:1000,j])
}

boxplot(t(theta[c(1,2,3,4,5),]),
        names=c("n=50","n=100","n=200","n=500","n=1000"), col = "orange", cex.axis = 0.85, cex.main = 0.8, main = "Comportement de l'estimateur de Weibull - Theta = 1.5")
abline(h = 1.5, col="red", lwd = 2)


### Partie 5 : Estimateur du maximum de vraisemblance dans le cadre de la loi de Weibull standard ###

#Fonction g. Demandée dans l'énoncé, mais ne servira pas dans notre algorithme
g <- function(x, theta, lambda){
  n = length(x) 
  return(-n*log(theta) + n * log(theta) - (theta - 1) * sum(log(x/lambda)) + sum((x/lambda)^theta))
} 

#Gradient de la fonction g à partir des dérivées partielles calculées
g_gradient <- function(x, theta, lambda){
  n = length(x)
  gradient = matrix(NA, nrow=2, ncol=1)
  gradient[1,1] = - n/theta - sum(log(x/lambda)) + sum(log(x/lambda)*(x/lambda)^theta)
  gradient[2,1] = (theta/lambda) * (n - sum((x/lambda)^theta))
  
  return(gradient)
}


#Matrice hessienne de la fonction g à partir des dérivées partielles secondes calculées
g_hessienne <- function(x, theta, lambda){
  n = length(x)
  hessienne = matrix(NA, nrow=2, ncol=2)
  hessienne[1,1] = (n/theta*theta) + sum(log(x/lambda)*log(x/lambda)*(x/lambda)^theta)
  hessienne[1,2] = n/lambda - (1/lambda)*sum((x/lambda)^theta) - (theta/lambda)*sum(log(x/lambda)*(x/lambda)^theta)
  hessienne[2,1] = n/lambda - (1/lambda)*sum((x/lambda)^theta) - (theta/lambda)*sum(log(x/lambda)*(x/lambda)^theta)
  hessienne[2,2] = (-theta/lambda^2)*(n - sum((x/lambda)^theta)) + (theta^2/lambda^2) * sum((x/lambda)^theta)
  
  return(hessienne)
}


#Fonction qui renvoie une estimation des paramètres grâce à l'algorithme de Newton-Raphson
est_teta_lambda <- function(x){
  z = matrix(data = 1, nrow = 2, ncol = 1)
  z1 = matrix(data = 0, nrow = 2, ncol = 1)
  k = 0
  z1 = z - solve(g_hessienne(x, z[1,1], z[2,1])) %*% g_gradient(x, z[1,1], z[2,1])
  while ((norm(z1 - z)) > 0.000001)  {
    z = z1
    z1 = z - solve(g_hessienne(x, z[1,1], z[2,1])) %*% g_gradient(x, z[1,1], z[2,1])
    k = k + 1
    
  }
  print(k)  #Affichage du nombre d'itérations
  return(z1)
}

#Paramètres
theta = 1.5
lambda = 2
set.seed(1234)
#Simulation de 400 échantillons de taille 1000
B <- 400 ; N <- 1000
dataW <- matrix(NA, ncol=B, nrow=N)
for(b in 1:B){
  dataW[,b] = rweibull(N,theta,lambda)
}


#Initialisation des matrices qui vont contenir les 4 estimations pour n= 50, 100, 200, 500, 1000
theta_general = matrix(NA, nrow=5, ncol=B)
lambda_general = matrix(NA, nrow=5, ncol=B)

valeur_n = list(50, 100, 200, 500, 1000)
for(j in 1:B){
  indice = 1
  for(i in valeur_n){
    resultat = est_teta_lambda(dataW[1:i,j]) #Récupère les esimations. Matrice de taille 2x1.
    theta_general[indice, j] = resultat[1] #Matrice qui contient les estimations de theta pour les différentes tailles d'échantillon
    lambda_general[indice, j] = resultat[2] #Matrice qui contient les estimations de lambda pour les différentes tailles d'échantillon
    indice = indice + 1 #Indice qui permet de remplir les matrices de résultat
  }
  
}


#Affichage du boxplot Theta
boxplot(t(theta_general[c(1,2,3,4,5),]),
        names=c("n=50","n=100","n=200","n=500","n=1000"), col = "orange", cex.axis = 0.85, cex.main = 0.75, main = "Comportement de l'estimateur de theta (=3) de la loi de Weibull générale")
abline(h = theta, col="red", lwd = 2)

#Afichage du boxplot lambda
boxplot(t(lambda_general[c(1,2,3,4,5),]),
        names=c("n=50","n=100","n=200","n=500","n=1000"), col = "orange", cex.axis = 0.85, cex.main = 0.75, main = "Comportement de l'estimateur de lambda (=1.5) de la loi de Weibull générale")
abline(h = lambda, col="red", lwd = 2)



