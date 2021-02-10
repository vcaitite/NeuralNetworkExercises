#Exercício 1.2 A)
teta<-1
x<-seq(-2,2,0.01)
y<-1*(x>teta)
par(new=T)
plot(x,y,type='l', col='red')

#Exercício 1.2 B)
teta<-(-1)
x<-seq(-2,2,0.01)
y<-(-1)*(x>teta)
par(new=T)
plot(x,y,type='l', col='blue')

#Exercício 1.2 C)
w<-1
x<-seq(-2,2,0.01)
y<-tanh(w*x)
par(new=T)
plot(x,y,type='l', col='purple',xlim=c(-2,2),ylim = c(-1,1))

#Exercício 1.2 D)
w<-(-1)
x<-seq(-2,2,0.01)
y<-tanh(w*x)
par(new=T)
plot(x,y,type='l', col='black')


