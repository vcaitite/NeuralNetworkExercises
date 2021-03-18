rm(list=ls())
library(caret)
library(RSNNS)
source("~/Documents/UFMG/9/Redes Neurais/exemplos/escalonamento_matrix.R")

# Carregando base de dados:
data <- read.table("~/Documents/UFMG/9/Redes Neurais/listas/lista 10/databases/housing.data",
           sep = "", dec = ".", header = FALSE)

executions <- 5
for (p in c(6,9,12)) {
  executions <- 5
  for(exec in 1:executions){
    # Separando dados de entrada e saída e treino e teste aleatoriamente:
    partition <- createDataPartition(1:dim(data)[1],p=.7)
    train <- as.matrix(data[partition$Resample1,])
    test <- as.matrix(data[- partition$Resample1,])
    x_train <- as.matrix(train[, 1:(ncol(train)-1)])
    y_train <- as.matrix(train[, ncol(train)])
    x_test <- as.matrix(test[, 1:(ncol(train)-1)])
    y_test <- as.matrix(test[, ncol(train)])
    
    # Escalonando os valores dos atributos para que fiquem restritos entre 0 e 1
    x_all <- rbind(x_train, x_test)
    x_all <- staggeringMatrix(x_all, nrow(x_all), ncol(x_all))
    x_train <- x_all[1:nrow(x_train), ]
    x_test <- x_all[(nrow(x_train)+1):(nrow(x_train)+nrow(x_test)), ]
    
    # Criando modelo:
    model <- mlp(x_train, y_train, size = p, maxit = 1000, initFunc = "Randomize_Weights",
          initFuncParams = c(-0.3, 0.3), learnFunc = "Std_Backpropagation",
          learnFuncParams = c(0.01, 0.05), updateFunc = "Topological_Order",
          updateFuncParams = 0.0, hiddenActFunc = "Act_Logistic",
          shufflePatterns = TRUE, linOut = TRUE, inputsTest = NULL,
          targetsTest = NULL)
    
    # Testando:
    yhat <- predict(model, as.matrix(x_test))
    mean_error <- mean(abs(yhat - y_test))
    sd <- sd(yhat - y_test)
    mse <- sum((yhat - y_test)^2)/length(y_test)
    print(paste("Para p = ", p, ", o erro médio para a execução ", exec, "foi: ", round(mean_error, 2), " +/- ", round(sd, 2)))
    print(paste("Para p = ", p, ", o erro quadrático médio para a execução ", exec, ": MSE = ", round(mse, 2)))
    print("__________________________________________________________________________________")
  }
}
