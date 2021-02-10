rm(list = ls())

# Obtenção do tempo de amostragem:
t<-as.matrix(read.table("~/Documents/UFMG/9/Redes Neurais/listas/lista 3/dados/t"))
# Obtenção dos pontos de entrada:
x_all<-as.matrix(read.table("~/Documents/UFMG/9/Redes Neurais/listas/lista 3/dados/x"))
x1<-as.matrix(x_all[,1])
x2<-as.matrix(x_all[,2])
x3<-as.matrix(x_all[,3])
# Obtenção da saída:
y<-as.matrix(read.table("~/Documents/UFMG/9/Redes Neurais/listas/lista 3/dados/y"))



# Classificando dados em dados de treinamento (1) ou teste (0) 
class<-c(1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1)
xtrain<-matrix(nrow=14, ncol = 3)
ytrain<-as.matrix(1:14, ncol = 1)
xtest<-matrix(nrow=6, ncol = 3)
ytest<-as.matrix(1:6, ncol = 1)
index_train = 1
index_test = 1
for (index in 1:20) {
  if((class[index] == 1)){
    xtrain[index_train, 1]<-x1[index, 1]
    xtrain[index_train, 2]<-x2[index, 1]
    xtrain[index_train, 3]<-x3[index, 1]
    ytrain[index_train, 1]<-y[index, 1]
    index_train<-index_train+1
  } else {
    xtest[index_test, 1]<-x1[index, 1]
    xtest[index_test, 2]<-x2[index, 1]
    xtest[index_test, 3]<-x3[index, 1]
    ytest[index_test, 1]<-y[index, 1]
    index_test<-index_test+1
  }
}

# Treinamento do modelo:
source("~/Documents/UFMG/9/Redes Neurais/exemplos/treinaadaline.R")
retlist<-trainadaline(xtrain, ytrain, 0.1, 0.01, 100, 1)
w<-retlist[[1]]
erro<-retlist[[2]]

# Plotando gráfico de saída
yhat = w[4]*x3+w[3]*x2+w[2]*x1+w[1]
plot(t, yhat, type='b', xlab = 'x', ylab = 'y', xlim = c(0,6), ylim = c(-2,10), col = "red" )

#Calculando Erro Médio Quadrático
yhat_test = cbind(1, xtest) %*% w
err=0
for (index in 1:6) {
  err=err+(yhat_test[index]-ytest[index])^2  
}
err=err/6

print("O erro médio quadrático para as amostras de teste foi:")
print(err)

print("Seja a saída dada por ŷ = w1*x + w0. Os parâmetros w0 e w1 são respectivamente:")
print(w)

