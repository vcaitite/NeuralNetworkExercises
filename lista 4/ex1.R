rm(list = ls())

library("plot3D")
source("~/Documents/UFMG/9/Redes Neurais/listas/lista 4/trainPerceptron.R")
source("~/Documents/UFMG/9/Redes Neurais/listas/lista 4/yperceptron.R")

s1<-0.4
s2<-0.4
nc<-200

# Distribuição normal N(2, 2, σ^2)
xc1<-matrix(rnorm(nc*2), ncol=2)*s1 + t(matrix((c(2,2)), ncol=nc, nrow=2)) 
# Distribuição normal N(4, 4, σ^2)
xc2<-matrix(rnorm(nc*2), ncol=2)*s2 + t(matrix((c(4,4)), ncol=nc, nrow=2)) 

# Plotando dados:
plot(xc1[,1], xc1[,2], col = "red", xlim=c(0,6), ylim=c(0,6), ylab="x_2", xlab="x_1")
par(new=T)
plot(xc2[,1], xc2[,2], col = "blue", xlim=c(0,6), ylim=c(0,6), ylab="", xlab="")

# Reta que separa os dados (inferida vidualmente)
x1_reta<-seq(6/100,6,0.01)
x2_reta<- -x1_reta+6 
par(new=TRUE)
plot(x1_reta, x2_reta, type="l", col="orange", xlim=c(0,6), ylim=c(0,6), ylab="", xlab="")

# ___________________________________________________________________________________________
# Treinando modelo:

# Definindo entradas da função (Nesse ex. todos os dados etão sendo usados para treino):
xin = as.matrix(rbind(xc1,xc2))
yc1_train<-matrix(0, nrow=nc)
yc2_train<-matrix(1, nrow=nc )
yp<-as.matrix(rbind(yc1_train, yc2_train))
retlist<-trainPerceptron(xin, yp, 0.1, 0.01, 100, 1) #função que faz o treinamento do perceptron
w<-retlist[[1]]
print(w)

#____________________________________________________________________________________________
# Teste do modelo:
# Pontos de teste
seqi<-seq(0,6,0.1)
seqj<-seq(0,6,0.1)

# Cálculo do perceptron para pontos de teste 
M<-matrix(0, nrow=length(seqi), ncol=length(seqj))
ci<-0
for(i in seqi) {
  ci<-ci+1
  cj<-0
  for(j in seqj) {
    cj<-cj+1
    x<-matrix(c(i,j), nrow = 1, ncol = 2)
    M[ci,cj]<-yperceptron(x,w,1)
  }
}

# Plotando resultados:
plot(xc1[,1], xc1[,2], xlim=c(0,6), ylim=c(0,6), col="red", ylab="x_2", xlab="x_1")
par(new=TRUE)
plot(xc2[,1], xc2[,2], xlim=c(0,6), ylim=c(0,6), col="blue",  ylab="", xlab="")
par(new=TRUE)
contour(seqi, seqj, M, xlim=c(0,6), ylim=c(0,6), ylab="", xlab="")
persp3D(seqi, seqj, M, counter=T, theta=55, phi=30, r=40, d=0.1, expand=0.5, ltheta=90, lphi=180, shade=0.4, ticktype="detailed", nticks=5)

