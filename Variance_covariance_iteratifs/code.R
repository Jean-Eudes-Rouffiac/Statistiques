
#Partie 1
set.seed(123)
X = rnorm(100)

N <- length(X) #dimension de X

#Méthode etudiée dans le TP
mean <- X[1]
var <-0
n = 1
for (i in 2:N) {
  n = n + 1
  mean = mean + (X[i] - mean) / n
  var <- var + (n*((mean-X[i])^2)/(n-1) - var) / n

}

#Méthode etudiée dans le cours
mean_2 <-X[1]
var_2 <-0
n = 1
for (i in 2:N) {
  n = n + 1
  var_2 = var_2 + ((n-1)*((X[i]-mean_2)^2)/n - var_2) / n
  mean_2 = mean_2 + (X[i] - mean_2) / n
}

res = var - var_2
#Res est égal à 0, les variances sont les mêmes pour les deux méthodes

#Valeur de la variance donnée par la fonction var de R
var_R = var(X)*(N-1)/N 

#La valeur de la variance donnée par la fonction var de R est la même que pour les 
#variances calculées précédemment

#Partie2

N = 100   # Taille de l’échantillon
p = 10    # Nombre de variables
#  Construction de la matrice de corrélation
rho = 0.8     # correlation entre variables
d  = as.matrix(dist(1:p))
Sigma = rho^d
SigmaRC = chol(Sigma)   # Racine carréee de Sigma
# Génération des données
set.seed(123)


#On crér une matrice de test X, de taille (100,10)
X = matrix(rnorm(N*p), N, p) %*% SigmaRC

#La matrice de Variance Covariance faite avec la fonction de référence
G1 = cov(X)*(N-1)/N 
 
#Calcul itératif de la matrice de covariance
p <- ncol(X) #nombre de variables
n <- nrow(X) #nombre de sujets
  
G2 <- matrix(data=0, nrow=p, ncol = p) 
moyenne = matrix(data=0, nrow=p, ncol = 1) 
for (i in 1:n){
  diff = X[i,]- moyenne
  G2 = G2 + (1/i)*(((i-1)/i) * diff %*% t(diff) - G2) 
  moyenne= ((i-1) /i) *moyenne +X[i,] / i
}
#On compare avec la covariance fournie par R
dist = sum((G1 - G2)^2)