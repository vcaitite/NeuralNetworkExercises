rm(list = ls())

# Obtenção do tempo de amostragem:
t<-as.matrix(read.table("~/Documents/UFMG/9/Redes Neurais/listas/lista 3/dados/Ex1_t"))
# Obtenção dos pontos de entrada:
x<-as.matrix(read.table("~/Documents/UFMG/9/Redes Neurais/listas/lista 3/dados/Ex1_x"))
# Obtenção da saída:
y<-as.matrix(read.table("~/Documents/UFMG/9/Redes Neurais/listas/lista 3/dados/Ex1_y"))

# Classificando dados em dados de treinamento (1) ou teste (0) 
class<-c(1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1)
xtrain<-as.matrix(1:14, ncol = 1)
ytrain<-as.matrix(1:14, ncol = 1)
xtest<-as.matrix(1:6, ncol = 1)
ytest<-as.matrix(1:6, ncol = 1)
index_train = 1
index_test = 1
for (index in 1:20) {
  if((class[index] == 1)){
    xtrain[index_train, 1]<-x[index, 1]
    ytrain[index_train, 1]<-y[index, 1]
    index_train<-index_train+1
  } else {
    xtest[index_test, 1]<-x[index, 1]
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
yhat = w[2]*x+w[1]
plot(t, yhat, type='b', xlab = 'x', ylab = 'y', xlim = c(0,6), ylim = c(-1,1), col = "red" )
par(new=T)
plot(t,x, type='b', xlab = 'x', ylab = 'y', xlim = c(0,6), ylim = c(-1,1), col = "blue")
  
#Calculando Erro Médio Quadrático
yhat_test = w[2]*xtest+w[1]
err=0
for (index in 1:6) {
  err=err+(yhat_test[index]-ytest[index])^2  
}
err=err/6

print("O erro médio quadrático para as amostras de teste foi:")
print(err)

print("Seja a saída dada por ŷ = w1*x + w0. Os parâmetros w0 e w1 são respectivamente:")
print(w)


