rm(list=ls())

sech2<-function(u){
  return(((2/(exp(u)+exp(-u)))*(2/(exp(u)+exp(-u)))))
}
mse_total=rep(0,5);

for (execution in 1:5) {
  x_train <- matrix(runif (45, 0, 2*pi), ncol=1)
  y_train <- matrix(sin(x_train) + rnorm(45, 0, 0.1), ncol=1)
  
  x_test <- seq(0, 2*pi, 0.01) 
  y_test <- sin(x_test)
  
  #plot(x_train, y_train, xlim = c(0,2*pi), ylim = c(-1,1), xlab = "x", ylab = "y", col='red')
  #par(new=T)
  #plot(x_test, y_test, type = 'l', xlim = c(0,2*pi), ylim = c(-1,1), xlab = "", ylab = "")
  n<-1
  p<-3
  #Inicialização dos pesos.
  #Matriz Z nxp, neste caso n+1xp ==> 3x2
  Z<-matrix(runif((n+1)*p)-0.5,ncol=p,nrow=n+1)
  
  #Matriz W pxm, nestecaso p+1xm ==> 3x2
  W<-matrix(runif((p+1)*1)-0.5,ncol=1,nrow=p+1)
  
  xatual<-matrix(nrow=2,ncol=1)
  
  tol<-0.01
  eta<-0.01
  maxepocas<-2000
  nepocas<-0
  eepoca<-tol+1
  N<-45
  evec<-matrix(nrow=maxepocas,ncol=1)

  while((nepocas<maxepocas)&&(eepoca>tol))
  {
    ei2<-0
    #Sequênciaaleatóriadetreinamento.
    xseq<-sample(N)
    for(i in 1:N)
    {
      #Amostradadodasequênciaaleatória.
      irand<-xseq[i]
      xatual[1,1]<-x_train[irand,1]
      xatual[2,1]<-1
      
      yatual<-y_train[irand, 1]
      
      U<-t(xatual)%*%Z
      #xatualé3x1eZé3x2
      H<-tanh(U)
      Haug<-cbind(H,1)#Haugé1x3
      
      O<-Haug%*%W
      yhat<-O
      
      e<-yatual-yhat
      flinhaO<-1
      dO<-e*flinhaO#Produtoelementoaelemento
      
      Wminus<-W[-(p+1),]
      #Saı́dadoviésnãosepropaga
      ehidden<-dO%*%t(Wminus)#dO1x2eW3x2,ehiddené1x2
      flinhaU<-sech2(U)
      dU<-ehidden*flinhaU#Produtoelementoaelemento
      
      W<-W+eta*(t(Haug)%*%dO)
      
      Z<-Z+eta*(xatual%*%dU)
      
      ei2<-ei2+(e%*%t(e))
    }
    #Incrementa número de épocas.
    nepocas<-nepocas+1
    evec[nepocas]<-ei2/N
    #Armazena erro por época.
    eepoca<-evec[nepocas]
  }
  print(paste("O erro quadrático médio da execução ", execution, " foi: ", ei2/N * 100, "%"))
  mse_total[execution] = ei2/N 
}
print(paste("O erro quadrático médio considerando as 5 execuções foi ", mean(mse_total) * 100, "±", sd(mse_total)*100, "%"))


# TESTE:
u <- cbind(x_test,1) %*% Z
H<-tanh(u)
O<-cbind(H,1)%*%W
yhat_test<-O
plot(x_test, yhat_test,  type = 'l', xlim = c(0,2*pi), ylim = c(-1,1), xlab = "x", ylab = "y")
par(new=T)
plot(x_test, y_test, type = 'l', xlim = c(0,2*pi), col="red", ylim = c(-1,1), xlab = "", ylab = "")

plot(evec[1:nepocas],type='l')