rm(list = ls())
source(file = "/home/vitor/Documents/UFMG/9/Redes Neurais/exemplos/trainRBF.R")
source(file = "/home/vitor/Documents/UFMG/9/Redes Neurais/exemplos/YRBF.R")
library(mlbench)

data <- mlbench.2dnormals(200)

xin<-matrix(data[["x"]], ncol = 2)
class<-matrix(data[["classes"]], ncol=1)
plot(xin[,1], xin[,2], col=data$classes, xlim=c(-5, 5), ylim=c(-5, 5), ylab = "x2", xlab = "x1")

yin <- rep(0,200)
for (count in 1:length(class)) {
  if (class[count] == 1 ){
    yin[count] = -1
  }
  else if(class[count] == 2){
    yin[count] = 1
  }
}

pvector<-c(1,2,5)
for (p in pvector) {
  
  modRBF<-trainRBF(xin,yin,p)
  
  
  # Plotando superfície
  seqx1x2 <- seq(-5, 5, 0.1)
  MZ<-matrix(nrow = length(seqx1x2), ncol = length(seqx1x2))
  cr<-0
  for (i in 1:length(seqx1x2)) {
    for (j in 1:length(seqx1x2)) {
      cr<-cr+1
      x1<-seqx1x2[i]
      x2<-seqx1x2[j]
      x1x2<-matrix(cbind(x1,x2), nrow = 1)
      MZ[i,j]<-YRBF(x1x2, modRBF)
    }
  }
  
  #contour(seqx1x2, seqx1x2, MZ, nlevels = 1, xlim=c(-5,5), ylim=c(-5,5), xlab = "x1", ylab = "x2")
  #par(new=T)
  print(paste("Classificação utilizando valor k = ", p))
  plot(xin[,1], xin[,2], col=data$classes, xlim=c(-5,5), ylim=c(-5,5), ylab = "", xlab = "")
  cols = c('lightblue', 'darkred')
  filled.contour(seqx1x2, seqx1x2, MZ, nlevels = 1, xlim=c(-5,5), ylim=c(-5,5), xlab = "x1", ylab = "x2", col = cols, axes=F, plot.axes = { points(xin[,1], xin[,2],col=data$classes, xlim=c(-5,5), ylim=c(-5,5)); axis(1); axis(2) })
}
