rm(list=ls())
source("~/Documents/UFMG/9/Redes Neurais/listas/lista 4/trainPerceptron.R")
source("~/Documents/UFMG/9/Redes Neurais/listas/lista 4/yperceptron.R")
library(caret)

# Carregando base de dados:
path <- file.path("~/Documents/UFMG/9/Redes Neurais/listas/lista 6/cancer", "wdbc.csv")
data <- read.csv(path)

# Separando dados de entrada e saída:
x_all <- as.matrix(data[1:569, 3:32])
class <- as.matrix(data[1:569, 2])
y_all <- rep(0,569)
for (count in 1:length(class)) {
  if (class[count] == 'M' ){
    y_all[count] = 0
  }
  else if(class[count] == 'B'){
    y_all[count] = 1
  }
}

# Realiza pelo 20 execuções diferentes
accuracy_train <- rep(0, 20)
accuracy_test <- rep(0, 20)
for(execution in 1:20){
  # Separando dados entre treino e teste aleatoriamente:
  positions_train <- createDataPartition(1:569,p=.7)
  length_train <- length(positions_train$Resample1)
  length_test <- length(y_all) - length_train
  x_train <- matrix(rep(0, 30*length_train), ncol=30, nrow=length_train)
  y_train <- rep(0, length_train)
  x_test <- matrix(rep(0, (30*length_test)), ncol=30, nrow=(length(y_all) - length_train))
  y_test <- rep(0, (length(y_all) - length_train))
  index_train <- 1
  index_test <- 1
  for (count in 1:length(y_all)) {
    if (index_train <= length_train && count == positions_train$Resample1[index_train]){
      x_train[index_train, ] <- x_all[count, 1:30 ]
      y_train[index_train] <- y_all[count]
      index_train = index_train + 1
    } else {
      x_test[index_test, ] <- x_all[count, 1:30 ]
      y_test[index_test] <- y_all[count]
      index_test = index_test + 1    
    }
  }
    
  # Treinando modelo:
  retlist<-trainPerceptron(x_train, y_train, 0.1, 0.01, 1000, 1)
  W<-retlist[[1]]
    
  # Calculando acurácia de treinamento
  y_hat_train <- as.matrix(yperceptron(x_train, W, 1), nrow = length_train, ncol = 1)
  accuracy_train[execution]<-1-((t(y_hat_train-y_train) %*% (y_hat_train-y_train))/length_train)
  #print(paste("Acurácia de treinamento para execução", execution, "com", p, "nerônios:", accuracy_train))
    
  # Calculando acurácia de Teste:
  y_hat_test <- as.matrix(yperceptron(x_test, W, 1), nrow = length_test, ncol = 1)
  accuracy_test[execution]<-1-((t(y_hat_test-y_test) %*% (y_hat_test-y_test))/length_test)
  #print(paste("Acurácia de teste para execução", execution, "com", p, "nerônios:", accuracy_test))
}
# Média das acurácias
mean_accuracy_train <- mean(accuracy_train) * 100
mean_accuracy_test <- mean(accuracy_test) * 100
  
# Desvio Padrão das acurácias
sd_accuracy_train <- sd(accuracy_train) * 100
sd_accuracy_test <- sd(accuracy_test) * 100
  
print(paste("Acurácia de treinamento do modelo com perceptron simples", mean_accuracy_train, "%", "±", sd_accuracy_train, "%"))
print(paste("Acurácia de teste do modelo com perceptron simples", mean_accuracy_test, "%", "±", sd_accuracy_test, "%"))

