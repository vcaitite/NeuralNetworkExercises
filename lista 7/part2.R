rm(list = ls())
source(file = "/home/vitor/Documents/UFMG/9/Redes Neurais/exemplos/trainRBF.R")
source(file = "/home/vitor/Documents/UFMG/9/Redes Neurais/exemplos/YRBF.R")

x <- matrix(runif (100, -15, 15), ncol=1)
y <- matrix(sin(x)/x + rnorm(100, 0, 0.05), ncol=1)

x_test <- matrix(runif (50, -15, 15), ncol=1)
y_test <- matrix(sin(x_test)/x_test + rnorm(50, 0, 0.05), ncol=1)

pvector<-c(3,10,20)
for (p in pvector) {
  modRBF <- trainRBF(x, y, p)
  xrange <- matrix(seq(-15, 15, 0.01), ncol = 1)
  yhat <- YRBF(xrange, modRBF)
  yhat_test <- YRBF(x_test, modRBF)
  mse <- sum((yhat_test - y_test)^2)/length(y_test)
  print(paste("Erro quadrático médio: MSE = ", mse))
  ############# Plots: ###############
  print(paste("Aproximação utilizando valor k = ", p))
  plot(x, y, xlim = c(-15, 15), ylim = c(-0.5,1.5), xlab = "x", ylab = "f(x), yhat", col = "black")
  par(new=T)
  plot(xrange, yhat, xlim = c(-15, 15), ylim = c(-0.5,1.5), xlab = "x", ylab = "f(x), yhat", col = "red", type = "l")
}

