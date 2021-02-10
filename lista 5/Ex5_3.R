rm(list=ls())
library(mlbench)
source("~/Documents/UFMG/9/Redes Neurais/exemplos/trainELM.R")
source("~/Documents/UFMG/9/Redes Neurais/exemplos/YELM.R")

data <- mlbench.circle(100) 
xin<-data[["x"]]
class<-data[["classes"]]
plot(xin[,1], xin[,2], col=data$classes, xlim=c(-1.1, 1.1), ylim=c(-1.1, 1.1), ylab = "x2", xlab = "x1")

yin <- rep(0,100)
for (count in 1:length(class)) {
  if (class[count] == 1 ){
    yin[count] = -1
  }
  else if(class[count] == 2){
    yin[count] = 1
  }
}

p<-30
retlist<-trainELM(xin,yin,p,1)

W<-retlist[[1]]
H<-retlist[[2]]
Z<-retlist[[3]]

y_hat <- YELM(xin, Z, W, 1)
err_train <- sum((yin - y_hat)^2)/4 #divide-se por 4 pelo fato de a distância entre -1 e 1 ser 2
print(err_train)

# Plotando superfície
seqx1x2 <- seq(-2, 2, 0.1)
MZ<-matrix(nrow = length(seqx1x2), ncol = length(seqx1x2))
cr<-0
for (i in 1:length(seqx1x2)) {
  for (j in 1:length(seqx1x2)) {
    cr<-cr+1
    x1<-seqx1x2[i]
    x2<-seqx1x2[j]
    x1x2<-matrix(cbind(x1,x2), nrow = 1)
    MZ[i,j]<-YELM(x1x2, Z, W, 1)
  }
}

contour(seqx1x2, seqx1x2, MZ, nlevels = 1, xlim=c(-2,2), ylim=c(-2,2), xlab = "x1", ylab = "x2")
par(new=T)
plot(xin[,1], xin[,2], col=data$classes, xlim=c(-2,2), ylim=c(-2,2), ylab = "", xlab = "")