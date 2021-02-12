rm(list=ls())
source("~/Documents/UFMG/9/Redes Neurais/exemplos/trainRBF.R")
source("~/Documents/UFMG/9/Redes Neurais/exemplos/YRBF.R")
source("~/Documents/UFMG/9/Redes Neurais/exemplos/escalonamento_matrix.R")
library(caret)

# Carregando base de dados:
path <- file.path("~/Documents/UFMG/9/Redes Neurais/listas/lista 8/databases/cancer", "wdbc.csv")
data <- read.csv(path)

# Separando dados de entrada e saída:
x_all <- as.matrix(data[1:569, 3:32])
class <- as.matrix(data[1:569, 2])
y_all <- rep(0,569)
for (count in 1:length(class)) {
  if (class[count] == 'M' ){
    y_all[count] = -1
  }
  else if(class[count] == 'B'){
    y_all[count] = 1
  }
}

# Escalonando os valores dos atributos para que fiquem restritos entre 0 e 1
x_all <- staggeringMatrix(x_all, nrow(x_all), ncol(x_all))

for (p in c(2,5,10,30,50,100)){
  # Realiza pelo 20 execuções diferentes
  accuracy_train <- rep(0, 10)
  accuracy_test <- rep(0, 10)
  for(execution in 1:10){
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
    modRBF<-trainRBF(x_train, y_train, p)
    
    # Calculando acurácia de treinamento
    y_hat_train <- as.matrix(YRBF(x_train, modRBF), nrow = length_train, ncol = 1)
    yt <- (1*(y_hat_train >= 0)-0.5)*2
    accuracy_train[execution] <- 1 - ((t(yt - y_train) %*% (yt - y_train))/(4*length(y_train)))
    #print(paste("Acurácia de treinamento para execução", execution, "com", p, "nerônios:", accuracy_train))
    
    # Calculando acurácia de Teste:
    y_hat_test <- as.matrix(YRBF(x_test, modRBF), nrow = length_test, ncol = 1)
    yt <- (1*(y_hat_test >= 0)-0.5)*2
    accuracy_test[execution] <- 1 - ((t(yt - y_test) %*% (yt - y_test))/(4*length(y_test)))
    #print(paste("Acurácia de teste para execução", execution, "com", p, "nerônios:", accuracy_test))
  }
  # Média das acurácias
  mean_accuracy_train <- mean(accuracy_train) * 100
  mean_accuracy_test <- mean(accuracy_test) * 100
  
  # Desvio Padrão das acurácias
  sd_accuracy_train <- sd(accuracy_train) * 100
  sd_accuracy_test <- sd(accuracy_test) * 100
  
  print(paste("Acurácia de treinamento do modelo com", p, "neurônios:", mean_accuracy_train, "%", "±", sd_accuracy_train, "%"))
  print(paste("Acurácia de teste do modelo com", p, "neurônios:", mean_accuracy_test, "%", "±", sd_accuracy_test, "%"))
}
