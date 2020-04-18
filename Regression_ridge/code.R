#PArtie non complète, demander à Guillaume le code


# Simulation study to compare OLS and ridge regression
library(MASS)
m <- 1000 # Number of data sets to simulate
n <- 100 # Sample size
p <- 200 # Number of covariates
rho <- 0.10 # correlation between predictors
k <- 0.1 # value of the non-zero true beta
beta <- rep(0,p)
beta[1:10] <- k
d <- as.matrix(dist(1:p))
Sigma <- rho^d
Sig12 <- chol(Sigma)


MSE <- Ychap <- matrix(0,m,2)
colnames(MSE) <- c("OLS","Ridge")
colnames(Ychap) <- c("OLS","Ridge")
lambda <- rep(NA, m)
bt <- c(1,rep(0,p))
seql <- seq(1,100,1)
#Simulate m datasets and analyze each with OLS and Ridge
for(sim in 1:m){
  # Generate data
  set.seed(0820*sim)
  X <- matrix(rnorm(n*p),n,p)%*%Sig12
  Y <- X%*%beta + rnorm(n)
  # Fit least squares
  ols.lm <- lm(Y~X)
  ols <- ols.lm$coef[-1]
  ychap <- sum(ols.lm$coef * bt)
  Ychap[sim,1] <- ychap
  # Fit ridge
  fit <- lm.ridge(Y~X,lambda=seql)
  best <- min(which.min(fit$GCV))
  ridge <- coef(fit)[best,-1]
  lambda[sim] <- seql[best]
  Ychap[sim,2] <- coef(fit)[best,] %*% bt
  # Compute mean squared error
  MSE[sim,1] <- mean((beta-ols)^2)
  MSE[sim,2] <- mean((beta-ridge)^2)
  #print(paste("Done with",sim,"of",m))
}

hist(lambda, main = paste(paste(paste(paste(paste(paste(paste("Hist lambda : n = ", n), "  p = ", p)), " k = ", k))), " rho = ", rho))
# => ajuster la s´equence choisie pour lambda
# Summarize the results


boxplot(MSE, main = paste(paste(paste(paste(paste(paste(paste("MSE : n = ", n), "  p = ", p)), " k = ", k))), " rho = ", rho))
boxplot(Ychap, main = paste(paste(paste(paste(paste(paste(paste("Ychap : n = ", n), "  p = ", p)), " k = ", k))), " rho = ", rho))
print("MSE")
print(round(colMeans(MSE),3))
print("Standard error")
print(round(apply(MSE,2,sd)/sqrt(m),3))
print("Wilcoxon test of equal MSE")
wilcox.test(MSE[,1],MSE[,2], paired=TRUE)







# Partie complète




data = read.table("Data_HRI.txt", sep=";", header=T)
data <- subset(data, select=-c(Date, DV.dom, DV.maxvv.fact, jour))
attach(data)
summary(data)


data = na.omit(data)

X = as.matrix(subset(data, select=-c(PM10.moy)))
Y = as.matrix(subset(data, select=c(PM10.moy)))
n = length(Y)
p = ncol(X)
Xbf <- cbind(rep(1,n),X)
# Recherche du param`etre optimal
seqLambda <- seq(9, 11, by=0.01)
Un <- rep(1, n)
XtX <- t(X)%*%X
UnX <- apply(X, 2, sum)
GCV <- NULL
for(lambda in seqLambda){
  #S.lambda <- XtX+diag(lambda, p)
  B.lambda = rbind( c(n, UnX) , cbind(UnX, XtX+diag(lambda, p)))
  A.lambda <- Xbf %*% solve(B.lambda) %*% t(Xbf)
  u <- (diag(1,n) - A.lambda) %*% Y
  num <- sum(u^2)
  den <- ((n - sum(diag(A.lambda)))^2)/n
  GCV <- c(GCV, num/den)
}
plot(seqLambda, GCV, type="b", lwd=1, cex.axis=1, cex.lab=1,
     xlab = "lambda", ylab = "GCV")
lambda = seqLambda[min(which.min(GCV))]

reslm_ridge = lm.ridge(Y~X, lambda = lambda)
beta.lambda = coef(reslm_ridge)

Y_pred = as.matrix(cbind(const=1,X)) %*% coef(reslm_ridge)

model<-lm(PM10.moy~. , data)
beta.lm = model$coefficients

plot(cbind(rep(1,p),rep(2,p)), cbind(beta.lm[2:(p+1)],beta.lambda[2:(p+1)]),
     xlim=c(0,3), cex=1, pch=19, xaxt="n", xlab="", cex.axis=1, cex.lab=1,
     ylab="Coefficients estimés (hors constante)")
axis(side=1, at = 1:2, labels=c("LM","Ridge"),cex.axis=1)
















######### derniere question

data = read.table("Data_HRI.txt", sep=";", header=T)
data <- subset(data, select=-c(Date, DV.dom, DV.maxvv.fact, jour))
attach(data)
summary(data)




data = na.omit(data)


set.seed(90) # initialisation du g´en´erateur
# Extraction des ´echantillons
test.ratio = 0.30 # part de l’´echantillon test
npop = nrow(data) # nombre de lignes dans les donn´ees
ntest = ceiling(npop*test.ratio) # taille de l’´echantillon test
testi = sample(1:npop, ntest) # indices de l’´echantillon test
appri = setdiff(1:npop, testi) # indices de l’´echant. d’apprentissage
# Construction des ´echantillons avec les variables explicatives
dataApp = data[appri, ] # construction de l’´echantillon d’apprentissage
dataTest = data[testi, ] # construction de l’´echantillon test



Xapp = as.matrix(subset(dataApp, select=-c(PM10.moy)))
Yapp = as.matrix(subset(dataApp, select=c(PM10.moy)))

Xtest = as.matrix(subset(dataTest, select=-c(PM10.moy)))
Ytest = as.matrix(subset(dataTest, select=c(PM10.moy)))

n = length(Yapp)
p = ncol(Xapp)
Xbf <- cbind(rep(1,n),Xapp)
# Recherche du param`etre optimal
seqLambda <- seq(6, 20, by=0.1)
Un <- rep(1, n)
XtX <- t(Xapp)%*%Xapp
UnX <- apply(Xapp, 2, sum)
GCV <- NULL
for(lambda in seqLambda){
  #S.lambda <- XtX+diag(lambda, p)
  B.lambda = rbind( c(n, UnX) , cbind(UnX, XtX+diag(lambda, p)))
  A.lambda <- Xbf %*% solve(B.lambda) %*% t(Xbf)
  u <- (diag(1,n) - A.lambda) %*% Yapp
  num <- sum(u^2)
  den <- ((n - sum(diag(A.lambda)))^2)/n
  GCV <- c(GCV, num/den)
}
plot(seqLambda, GCV, type="b", lwd=2, cex.axis=1.4, cex.lab=1.5,
     xlab = "lambda", ylab = "GCV")
lambda = seqLambda[min(which.min(GCV))]

#S.lambda <- XtX+diag(lambda, p)
#beta_opt = solve(S.lambda) %*% t(Xapp) %*% Yapp

#Y_pred_app = Xapp %*% beta_opt
#Y_pred_test = Xtest %*% beta_opt

reslm_ridge = lm.ridge(Yapp~Xapp, lambda = lambda)
coef(reslm_ridge)


dataAppReduit = subset(dataApp, select=-c(T.moy, HR.min, HR.moy, NO.max, SO2.max))
dataTestReduit = subset(dataTest, select=-c(T.moy, HR.min, HR.moy, NO.max, SO2.max))
reslmreduit = lm(PM10.moy~. , dataAppReduit)



reslm = lm(PM10.moy~. , subset(dataApp, select=-c(T.moy)))

Y_pred_lm_reduit_app = predict(reslmreduit)
Y_pred_lm_reduit_test = predict(reslmreduit, newdata = dataTestReduit)

Y_pred_lm_app = predict(reslm)
Y_pred_lm_test = predict(reslm, newdata = subset(dataTest, select=-c(T.moy)))

Y_pred_ridge_app = as.matrix(cbind(const=1,Xapp)) %*% coef(reslm_ridge)
Y_pred_ridge_test = as.matrix(cbind(const=1,Xtest)) %*% coef(reslm_ridge)



source("Perfopm10.R")

Perfopm10(Yapp, Y_pred_ridge_app)
Perfopm10(Ytest, Y_pred_ridge_test)

Perfopm10(dataAppReduit$PM10.moy, Y_pred_lm_reduit_app)
Perfopm10(dataTestReduit$PM10.moy, Y_pred_lm_reduit_test)

Perfopm10(dataApp$PM10.moy, Y_pred_lm_app)
Perfopm10(dataTest$PM10.moy, Y_pred_lm_test)

source("TabDeppm10.R")
TabDeppm10(Yapp, Y_pred_ridge_app,50,80,50)
TabDeppm10(Ytest, Y_pred_ridge_test,50,80,50)

TabDeppm10(dataAppReduit$PM10.moy, Y_pred_lm_reduit_app,50,80,50)
TabDeppm10(dataTestReduit$PM10.moy, Y_pred_lm_reduit_test,50,80,50)

TabDeppm10(dataApp$PM10.moy, Y_pred_lm_app,50,80,50)
TabDeppm10(dataTest$PM10.moy, Y_pred_lm_test,50,80,50)
