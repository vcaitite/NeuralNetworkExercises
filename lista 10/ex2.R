rm(list=ls())
library(caret)
library(RSNNS)
source("~/Documents/UFMG/9/Redes Neurais/exemplos/escalonamento_matrix.R")

# Carregando base de dados:
data <- read.table("~/Documents/UFMG/9/Redes Neurais/listas/lista 10/databases/heart.dat",
                   sep = "", dec = ".", header = FALSE)

executions <- 5
for (p in c(6,9,12)) {
  error <- rep(0, 5) 
  executions <- 5
  for(exec in 1:executions){
    # Separando dados de entrada e saída e treino e teste aleatoriamente:
    partition <- createDataPartition(1:dim(data)[1],p=.7)
    train <- as.matrix(data[partition$Resample1,])
    test <- as.matrix(data[- partition$Resample1,])
    x_train <- as.matrix(train[, 1:(ncol(train)-1)])
    y_train <- as.matrix(train[, ncol(train)])
    y_train <- ifelse(y_train == 2, -1, 1)
    x_test <- as.matrix(test[, 1:(ncol(train)-1)])
    y_test <- as.matrix(test[, ncol(train)])
    y_test <- ifelse(y_test == 2, -1, 1)
    
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
    accuracy<-((sum(abs(y_test + yhat)))/2)/length(y_test)
    error[exec] <- 1 - accuracy
    print(paste("Para p = ", p, ", o erro para a execução ", exec, "foi: ", round(error[exec], 2)))
    print(paste("Para p = ", p, ", a acurácia para a execução ", exec, "foi: ", round(accuracy, 2)))
    print("__________________________________________________________________________________")
  }
  txt <- paste("Para p = ", p, ", a média do erro foi: ", round(mean(error), 2), "+/-", round(sd(error), 2))
  cat(paste0("\033[0;", 46, "m",txt,"\033[0m","\n"))
}
