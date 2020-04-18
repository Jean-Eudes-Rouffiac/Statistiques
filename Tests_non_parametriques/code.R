# MS ESD TP3
#set.seed[1234]

# -----  Partie 1 : Etude du niveau et de la puissance empirique du test de Student ---- #

B <- 400 ; N <- 1000
mu <- 0
data <- matrix(NA, ncol=B, nrow=N)
for(b in 1:B){
  data[,b] = rnorm(N,mean=0,sd=1)
}

valn <- c(50,100,200,500,1000)
res <- NULL

niveau <- 0.05
mu0 = 0
for(n in valn){
  nrejet <- 0
  for(j in 1:B){
    p.value <- t.test(data[1:n, j], mu=mu0)$p.value
    if(p.value < niveau){nrejet <- nrejet + 1}
  }
  res <- c(res, nrejet)
}
res.pourcent = 100*res/B

# Resultats inférieur à 5% donc on accepte H0 au risque 5%.


data1 <- data2 <- data3 <- matrix(NA, ncol=B, nrow=N)
for(j in 1:B){
  data1[,j] = rnorm(N,mean=1)
  data2[,j] = rnorm(N,mean=0.3)
  data3[,j] = rnorm(N,mean=0.1)
}

res1 <- res2 <- res3 <- NULL
for(n in valn){
  nrejet1 <- nrejet2 <- nrejet3 <- 0
  for(j in 1:B){
    p.value <- t.test(data1[1:n, j], mu=mu0)$p.value
    if(p.value < niveau){nrejet1 <- nrejet1 + 1}
    p.value <- t.test(data2[1:n, j], mu=mu0)$p.value
    if(p.value < niveau){nrejet2 <- nrejet2 + 1}
    p.value <- t.test(data3[1:n, j], mu=mu0)$p.value
    if(p.value < niveau){nrejet3 <- nrejet3 + 1}
    
  }
  res1 <- c(res1, nrejet1)
  res2 <- c(res2, nrejet2)
  res3 <- c(res3, nrejet3)
}
res1.pourcent = 100*res1/B
res2.pourcent = 100*res2/B
res3.pourcent = 100*res3/B


# Pouvoir séparateur du test

Z <- Z2 <- Z1 <- rep(NA, B)
for(j in 1:B){
  Z[j] <- t.test(data[1:50, j], mu=mu0)$stat
  Z2[j] <- t.test(data2[1:50, j], mu=mu0)$stat
  Z1[j] <- t.test(data1[1:50, j], mu=mu0)$stat
}
x=seq(-3,5,l=100)
plot(x, dt(x, 49), type="l", main="Densités estimées des statistiques de test")
lines(density(Z), lwd=2, col = 'red')
lines(density(Z2), lty=2, lwd=1, col = 'blue')
lines(density(Z1), lty=2, lwd=2, col = 'orange')



# Je test au risque de 5%. Dans 5% des cas, ce test se trompe.


# --- Partie 2 Etude du niveau empirique et de la puissance empirique des tests de Shapiro-Wilk
#et de Jarque-Bera.

res <- NULL
res2 <- NULL
niveau <- 0.05
for(n in valn){
  nrejet <- 0
  nrejet2 <- 0
  for(j in 1:B){
    p.value <- shapiro.test(data[1:n, j])$p.value
    p.value2 <- jarque.bera.test(data[1:n, j])$p.value
    if(p.value < niveau){nrejet <- nrejet + 1}
    if(p.value2 < niveau){nrejet2 <- nrejet2 + 1}
  }
  res <- c(res, nrejet)
  res2 <- c(res2, nrejet2)
}
res.pourcent = 100*res/B
res2.pourcent = 100*res2/B


N <- 1000
dataU <- dataS5 <- dataS10 <- matrix(NA, ncol=B, nrow=N)
for(j in 1:B){
  dataU[,j] = runif(N,-1,1)
  dataS5[,j] = rt(N,5)
  dataS10[,j] = rt(N,10)
}

resU<- resS5 <- resS10 <- resUJB <- resS5JB <- resS10JB <-NULL
for(n in valn){
  nrejetU <- nrejetS5 <- nrejetS10 <- nrejetUJB <- nrejetS5JB <- nrejetS10JB <- 0
  for(j in 1:B){
    p.value <- shapiro.test(dataS10[1:n, j])$p.value
    if(p.value < 0.05){nrejetS10 <- nrejetS10 + 1}
    p.value <- shapiro.test(dataS5[1:n, j])$p.value
    if(p.value < 0.05){nrejetS5 <- nrejetS5 + 1}
    p.value <- shapiro.test(dataU[1:n, j])$p.value
    if(p.value < 0.05){nrejetU <- nrejetU + 1}
    p.value <- jarque.bera.test(dataS10[1:n, j])$p.value
    if(p.value < 0.05){nrejetS10JB <- nrejetS10JB + 1}
    p.value <- jarque.bera.test(dataS5[1:n, j])$p.value
    if(p.value < 0.05){nrejetS5JB <- nrejetS5JB + 1}
    p.value <- jarque.bera.test(dataU[1:n, j])$p.value
    if(p.value < 0.05){nrejetUJB <- nrejetUJB + 1}
  }
  resU <- c(resU, nrejetU)
  resS5 <- c(resS5, nrejetS5)
  resS10 <- c(resS10, nrejetS10)
  resUJB <- c(resUJB, nrejetUJB)
  resS5JB <- c(resS5JB, nrejetS5JB)
  resS10JB <- c(resS10JB, nrejetS10JB)
}
resU.pourcent = 100*resU/B
resS5.pourcent = 100*resS5/B
resS10.pourcent = 100*resS10/B
resUJB.pourcent = 100*resUJB/B
resS5JB.pourcent = 100*resS5JB/B
resS10JB.pourcent = 100*resS10JB/B

# --- Partie 3 Etude du niveau et de la puissance empiriques du test de Mc Nemar.

N <- 5000
B <- 400
p01 <- p10 <- 0.1
p00 <- p11 <- (1 - p01 - p10)/2
dataX <- dataY <- matrix(NA, ncol=B, nrow=N)
for(j in 1:B){
  for(i in 1:N){
    # Simulation d’une réalisation de (X,Y)
    v = c(1:4)[(rmultinom(1, 1, prob = c(p00,p01, p10, p11))==1)]
    if (v == 1) { dataX[i,j] = 0 ; dataY[i,j] = 0 }
    if (v == 2) { dataX[i,j] = 0 ; dataY[i,j] = 1 }
    if (v == 3) { dataX[i,j] = 1 ; dataY[i,j] = 0 }
    if (v == 4) { dataX[i,j] = 1 ; dataY[i,j] = 1 }
  }
}

valn <- c(100, 200, 500, 1000, 5000)
res <- NULL
niveau <- 0.05
for(n in valn){
  nrejet <- 0
  for(j in 1:B){
    data.table <- table(dataX[1:n, j], dataY[1:n, j])
    if(data.table[1,2] + data.table[2,1] > 25)
      p.value <- mcnemar.test(data.table, correct=FALSE)$p.value
    else
      p.value <- mcnemar.test(data.table, correct=TRUE)$p.value
    if(p.value < niveau) nrejet <- nrejet + 1
  }
  res <- c(res, nrejet)
}
res.pourcent = 100*res/B


N <- 5000
B <- 400
p01_1 <- 0.25
p10_1 <- 0.05
p00_1 <- p11_1 <- (1 - p01_1 - p10_1)/2

p01_2 <- 0.15
p10_2 <- 0.1
p00_2 <- p11_2 <- (1 - p01_2 - p10_2)/2

dataX1 <- dataY1 <- matrix(NA, ncol=B, nrow=N)
dataX2 <- dataY2 <- matrix(NA, ncol=B, nrow=N)
for(j in 1:B){
  for(i in 1:N){
    # Simulation d’une réalisation de (X,Y)
    v1 = c(1:4)[(rmultinom(1, 1, prob = c(p00_1,p01_1, p10_1, p11_1))==1)]
    if (v1 == 1) { dataX1[i,j] = 0 ; dataY1[i,j] = 0 }
    if (v1 == 2) { dataX1[i,j] = 0 ; dataY1[i,j] = 1 }
    if (v1 == 3) { dataX1[i,j] = 1 ; dataY1[i,j] = 0 }
    if (v1 == 4) { dataX1[i,j] = 1 ; dataY1[i,j] = 1 }
    
    v2 = c(1:4)[(rmultinom(1, 1, prob = c(p00_2,p01_2, p10_2, p11_2))==1)]
    if (v2 == 1) { dataX2[i,j] = 0 ; dataY2[i,j] = 0 }
    if (v2 == 2) { dataX2[i,j] = 0 ; dataY2[i,j] = 1 }
    if (v2 == 3) { dataX2[i,j] = 1 ; dataY2[i,j] = 0 }
    if (v2 == 4) { dataX2[i,j] = 1 ; dataY2[i,j] = 1 }
  }
}

valn <- c(100, 200, 500, 1000, 5000)
res1 <- res2 <- NULL
niveau <- 0.05
for(n in valn){
  nrejet1 <- nrejet2 <- 0
  for(j in 1:B){
    # cas facile
    data.table <- table(dataX1[1:n, j], dataY1[1:n, j])
    if(data.table[1,2] + data.table[2,1] > 25)
      p.value <- mcnemar.test(data.table, correct=FALSE)$p.value
    else
      p.value <- mcnemar.test(data.table, correct=TRUE)$p.value
    if(p.value < niveau) nrejet1 <- nrejet1 + 1
    # cas difficile
    data.table <- table(dataX2[1:n, j], dataY2[1:n, j])
    if(data.table[1,2] + data.table[2,1] > 25)
      p.value <- mcnemar.test(data.table, correct=FALSE)$p.value
    else
      p.value <- mcnemar.test(data.table, correct=TRUE)$p.value
    if(p.value < niveau) nrejet2 <- nrejet2 + 1
  }
  res1 <- c(res1, nrejet1)
  res2 <- c(res2, nrejet2)
}
res1.pourcent = 100*res1/B
res2.pourcent = 100*res2/B



### Partie 4 : Etude puissance test de conformité loi de Weibull


g <- function(x, theta){
  n = length(x) 
  return(-n*log(theta) + n * log(theta) - (theta - 1) * sum(log(x)) + sum((x)^theta))
} 

g_prime <- function(z, x){
  n = length(x)
  return(-(n/z)- sum(log(x)) + sum(log(x)*x^z))
} 

g_seconde <- function(z, x){
  n = length(x)
  return((n/(z*z)) + sum(log(x)*log(x)*x^z))
} 

est_teta <- function(x){
  z = 1
  k = 0
  z1 = z - (g_prime(z, x) / g_seconde(z, x))
  while ((abs(z1 - z)) > 0.0001  && k < 100)  {
    z = z1
    z1 = z - (g_prime(z, x) / g_seconde(z, x))
    k = k + 1
  }
  #print(k)
  return(z1)
}

variable_Z <- function(theta,theta_theorique, x){
  n = length(x)
  return(sqrt(g_seconde(theta,x))*(theta-theta_theorique))
} 

set.seed(20)

valn <- c(50,70,100,200,500,1000)
valtheta <- c(0.05,0.2,1.5,3)

theta1 = theta2 = theta3 = theta4  = matrix(NA, nrow=6, ncol=B)
res1 = res2 = res3 = res4 = matrix(NA, nrow=6, ncol=B)

l = 1

for (theta_theorique in valtheta) {
  
  B <- 400 ; N <- 1000
  dataW <- matrix(NA, ncol=B, nrow=N)
  for(b in 1:B){
    dataW[,b] = rweibull(N,theta_theorique,1)
  }
    
    for(j in 1:B){
      k = 1
      
      for(i in valn) {
        if (l == 1){
          theta1[k, j] = est_teta(dataW[1:i,j])
          res1[k,j] = variable_Z(theta1[k,j],theta_theorique, dataW[1:i,j])
          k = k + 1          
        }
        
        if (l == 2){
          theta2[k, j] = est_teta(dataW[1:i,j])
          res2[k,j] = variable_Z(theta2[k,j],theta_theorique, dataW[1:i,j])
          k = k + 1          
        }
        
        if (l == 3){
          theta3[k, j] = est_teta(dataW[1:i,j])
          res3[k,j] = variable_Z(theta3[k,j],theta_theorique, dataW[1:i,j])
          k = k + 1          
        }
        
        if (l == 4){
          theta4[k, j] = est_teta(dataW[1:i,j])
          res4[k,j] = variable_Z(theta4[k,j],theta_theorique, dataW[1:i,j])
          k = k + 1          
        }

      }
    }
  
  l =l + 1
}

x = seq(-3,3, by = 0.01)
plot(x, dnorm(x,0,1), ylim = c(0, 0.5) , type="l", cex.main = 0.7, main="Densités estimées de Z pour différentes valeurs de n - Theta = 0.05", xlab = "x", ylab = "Densité")
lines(density(res1[1,]), lwd=2, col = 'red')
lines(density(res1[2,]), lwd=2, col = 'blue')
lines(density(res1[3,]), lwd=2, col = 'yellow')
lines(density(res1[4,]), lwd=2, col = 'cyan')
legend("topright",legend=c("Loi normale","n=50", "n=70", "n=100", "n=200"), col=c("black","red", "blue", "yellow", "cyan"),pch=15, bty="n",cex=0.8)

x = seq(-3,3, by = 0.01)
plot(x, dnorm(x,0,1), ylim = c(0, 0.5) , type="l", cex.main = 0.7, main="Densités estimées de Z pour différentes valeurs de n - Theta = 0.2", xlab = "x", ylab = "Densité")
lines(density(res2[1,]), lwd=2, col = 'red')
lines(density(res2[2,]), lwd=2, col = 'blue')
lines(density(res2[3,]), lwd=2, col = 'yellow')
lines(density(res2[4,]), lwd=2, col = 'cyan')
legend("topright",legend=c("Loi normale","n=50", "n=70", "n=100", "n=200"), col=c("black","red", "blue", "yellow", "cyan"),pch=15, bty="n",cex=0.8)

x = seq(-3,3, by = 0.01)
plot(x, dnorm(x,0,1), ylim = c(0, 0.5) , type="l", cex.main = 0.7, main="Densités estimées de Z pour différentes valeurs de n - Theta = 1.5", xlab = "x", ylab = "Densité")
lines(density(res3[1,]), lwd=2, col = 'red')
lines(density(res3[2,]), lwd=2, col = 'blue')
lines(density(res3[3,]), lwd=2, col = 'yellow')
lines(density(res3[4,]), lwd=2, col = 'cyan')
legend("topright",legend=c("Loi normale","n=50", "n=70", "n=100", "n=200"), col=c("black","red", "blue", "yellow", "cyan"),pch=15, bty="n",cex=0.8)

x = seq(-3,3, by = 0.01)
plot(x, dnorm(x,0,1), ylim = c(0, 0.5) , type="l", cex.main = 0.7, main="Densités estimées de Z pour différentes valeurs de n - Theta = 3", xlab = "x", ylab = "Densité")
lines(density(res4[1,]), lwd=2, col = 'red')
lines(density(res4[2,]), lwd=2, col = 'blue')
lines(density(res4[3,]), lwd=2, col = 'yellow')
lines(density(res4[4,]), lwd=2, col = 'cyan')
legend("topright",legend=c("Loi normale","n=50", "n=70", "n=100", "n=200"), col=c("black","red", "blue", "yellow", "cyan"),pch=15, bty="n",cex=0.8)



resultat1 = resultat2 = resultat3 = resultat4  = matrix(NA, nrow=6, ncol=2)

niveau1 <- 0.05
niveau2 <- 0.01

for (i in 1:6){
  nrejet1 = nrejet2= nrejet3= nrejet4= nrejet5= nrejet6= nrejet7= nrejet8 = 0
  for(j in 1:B){
    if(abs(res1[i,j]) > qnorm(1-niveau1/2)){nrejet1 <- nrejet1 + 1}
    if(abs(res1[i,j]) > qnorm(1-niveau2/2)){nrejet2 <- nrejet2 + 1}
    if(abs(res2[i,j]) > qnorm(1-niveau1/2)){nrejet3 <- nrejet3 + 1}
    if(abs(res2[i,j]) > qnorm(1-niveau2/2)){nrejet4 <- nrejet4 + 1}
    if(abs(res3[i,j]) > qnorm(1-niveau1/2)){nrejet5 <- nrejet5 + 1}
    if(abs(res3[i,j]) > qnorm(1-niveau2/2)){nrejet6 <- nrejet6 + 1}
    if(abs(res4[i,j]) > qnorm(1-niveau1/2)){nrejet7 <- nrejet7 + 1}
    if(abs(res4[i,j]) > qnorm(1-niveau2/2)){nrejet8 <- nrejet8 + 1}
    
  }
  resultat1[i,1]=nrejet1
  resultat1[i,2]=nrejet2
  resultat2[i,1]=nrejet3
  resultat2[i,2]=nrejet4
  resultat3[i,1]=nrejet5
  resultat3[i,2]=nrejet6
  resultat4[i,1]=nrejet7
  resultat4[i,2]=nrejet8
}  

resultat1.pourcent = 100*resultat1/B
resultat2.pourcent = 100*resultat2/B
resultat3.pourcent = 100*resultat3/B
resultat4.pourcent = 100*resultat4/B







#Pouvoir séparateur du test


dataR1 <- dataR2 <- dataR3 <- dataR4 <- dataR5 <-matrix(NA, ncol=B, nrow=N)
for(j in 1:B){
  dataR1[,j] = rweibull(N,1,1)
  dataR2[,j] = rweibull(N,0.8,1)
  dataR3[,j] = rweibull(N,1.2,1)
  dataR4[,j] = rweibull(N,1.5,1)
  dataR5[,j] = rweibull(N,3,1)
}


set.seed(20)

valn <- c(50,70,100,200,500,1000)
valtheta <- c(1,0.8,1.2,1.5,3)

theta5 = theta6 = theta7 = theta8  = theta9 = matrix(NA, nrow=6, ncol=B)
res5 = res6 = res7 = res8 = res9 = matrix(NA, nrow=6, ncol=B)


theta0 = 1

for(j in 1:B){
    k = 1
    
    for(i in valn) {
        theta5[k, j] = est_teta(dataR1[1:i,j])
        res5[k,j] = variable_Z(theta5[k,j],theta0, dataR1[1:i,j])
      
        theta6[k, j] = est_teta(dataR2[1:i,j])
        res6[k,j] = variable_Z(theta6[k,j],theta0, dataR2[1:i,j])

        theta7[k, j] = est_teta(dataR3[1:i,j])
        res7[k,j] = variable_Z(theta7[k,j],theta0, dataR3[1:i,j])

        theta8[k, j] = est_teta(dataR4[1:i,j])
        res8[k,j] = variable_Z(theta8[k,j],theta0, dataR4[1:i,j])

        theta9[k, j] = est_teta(dataR5[1:i,j])
        res9[k,j] = variable_Z(theta9[k,j],theta0, dataR5[1:i,j])

        k = k +1
    }
  }


resultat5 = resultat6 = resultat7 = resultat8 = resultat9  = matrix(NA, nrow=6, ncol=1)

niveau1 <- 0.05

for (i in 1:6){
  nrejet5 = nrejet6 = nrejet7 = nrejet8 = nrejet9 = 0
  for(j in 1:B){
    if(abs(res5[i,j]) > qnorm(1-niveau1/2)){nrejet5 <- nrejet5 + 1}
    if(abs(res6[i,j]) > qnorm(1-niveau1/2)){nrejet6 <- nrejet6 + 1}
    if(abs(res7[i,j]) > qnorm(1-niveau1/2)){nrejet7 <- nrejet7 + 1}
    if(abs(res8[i,j]) > qnorm(1-niveau1/2)){nrejet8 <- nrejet8 + 1}
    if(abs(res9[i,j]) > qnorm(1-niveau1/2)){nrejet9 <- nrejet9 + 1}
    
  }
  resultat5[i,1]=nrejet5
  resultat6[i,1]=nrejet6
  resultat7[i,1]=nrejet7
  resultat8[i,1]=nrejet8
  resultat9[i,1]=nrejet9

}  

resultat5.pourcent = 100*resultat5/B
resultat6.pourcent = 100*resultat6/B
resultat7.pourcent = 100*resultat7/B
resultat8.pourcent = 100*resultat8/B
resultat9.pourcent = 100*resultat9/B




x = seq(-3,3, by = 0.01)
plot(x, dnorm(x,0,1), ylim = c(0, 0.5) , type="l", cex.main = 0.7, main="Densités estimées de Z pour différentes valeurs de n - Theta = 0.05", xlab = "x", ylab = "Densité")
lines(density(res5[1,]), lwd=2, col = 'red')
lines(density(res5[2,]), lwd=2, col = 'blue')
lines(density(res5[3,]), lwd=2, col = 'yellow')
lines(density(res5[4,]), lwd=2, col = 'cyan')
legend("topright",legend=c("Loi normale","n=50", "n=70", "n=100", "n=200"), col=c("black","red", "blue", "yellow", "cyan"),pch=15, bty="n",cex=0.8)

x = seq(-9,3, by = 0.01)
plot(x, dnorm(x,0,1), ylim = c(0, 0.5) , type="l", cex.main = 0.7, main="Densités estimées de Z pour différentes valeurs de n - Theta = 0.8", xlab = "x", ylab = "Densité")
lines(density(res6[1,]), lwd=2, col = 'red')
lines(density(res6[2,]), lwd=2, col = 'blue')
lines(density(res6[3,]), lwd=2, col = 'yellow')
lines(density(res6[4,]), lwd=2, col = 'cyan')
legend("topright",legend=c("Loi normale","n=50", "n=70", "n=100", "n=200"), col=c("black","red", "blue", "yellow", "cyan"),pch=15, bty="n",cex=0.8)

x = seq(-3,7, by = 0.01)
plot(x, dnorm(x,0,1), ylim = c(0, 0.5) , type="l", cex.main = 0.7, main="Densités estimées de Z pour différentes valeurs de n - Theta = 1.2", xlab = "x", ylab = "Densité")
lines(density(res7[1,]), lwd=2, col = 'red')
lines(density(res7[2,]), lwd=2, col = 'blue')
lines(density(res7[3,]), lwd=2, col = 'yellow')
lines(density(res7[4,]), lwd=2, col = 'cyan')
legend("topright",legend=c("Loi normale","n=50", "n=70", "n=100", "n=200"), col=c("black","red", "blue", "yellow", "cyan"),pch=15, bty="n",cex=0.8)

x = seq(-3,9, by = 0.01)
plot(x, dnorm(x,0,1), ylim = c(0, 0.5) , type="l", cex.main = 0.7, main="Densités estimées de Z pour différentes valeurs de n - Theta = 1.5", xlab = "x", ylab = "Densité")
lines(density(res8[1,]), lwd=2, col = 'red')
lines(density(res8[2,]), lwd=2, col = 'blue')
lines(density(res8[3,]), lwd=2, col = 'yellow')
lines(density(res8[4,]), lwd=2, col = 'cyan')
legend("topright",legend=c("Loi normale","n=50", "n=70", "n=100", "n=200"), col=c("black","red", "blue", "yellow", "cyan"),pch=15, bty="n",cex=0.8)


x = seq(-3,15, by = 0.01)
plot(x, dnorm(x,0,1), ylim = c(0, 0.5) , type="l", cex.main = 0.7, main="Densités estimées de Z pour différentes valeurs de n - Theta = 3", xlab = "x", ylab = "Densité")
lines(density(res9[1,]), lwd=2, col = 'red')
lines(density(res9[2,]), lwd=2, col = 'blue')
lines(density(res9[3,]), lwd=2, col = 'yellow')
lines(density(res9[4,]), lwd=2, col = 'cyan')
legend("topright",legend=c("Loi normale","n=50", "n=70", "n=100", "n=200"), col=c("black","red", "blue", "yellow", "cyan"),pch=15, bty="n",cex=0.8)
