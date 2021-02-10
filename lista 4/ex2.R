rm(list = ls())

library("plot3D")
library("bnlearn") 
library("caret") #Para plotar matriz de confusão
source("~/Documents/UFMG/9/Redes Neurais/listas/lista 4/trainPerceptron.R")
source("~/Documents/UFMG/9/Redes Neurais/listas/lista 4/yperceptron.R")


#_______________________________________________________________________________
#Gerando dados:
# Distribuição normal N(2, 2, σ^2)
xc1<-matrix(rnorm(200*2), ncol=2)*0.4 + t(matrix((c(2,2)), ncol=200, nrow=2)) 
# Distribuição normal N(4, 4, σ^2)
xc2<-matrix(rnorm(200*2), ncol=2)*0.4 + t(matrix((c(4,4)), ncol=200, nrow=2)) 

#_______________________________________________________________________________
# Separando em conjunto de treinamento e conjunto de teste:

# Seleção das amostra de Treinamento:
ntrain<-200*0.7 # Número de amostras, de cada uma das classes, selecionadas para treinamento.
seqc1<-sample(200) #Gera um vetor com números int de 1 a 200 em posições aleatórias
xc1_train<-xc1[seqc1[1:ntrain],] # 140 amostras de treino 
yc1_train<-matrix(0, nrow=ntrain) # 0 - amostras distribuidas em torno de 2
seqc2<-sample(200) #Gera um vetor com números int de 1 a 50 em posições aleatórias
xc2_train<-xc2[seqc2[1:ntrain],] # 140 amostras de treino 
yc2_train<-matrix(1, nrow=ntrain) # 1 - amostras distribuídas em trono de 4

# Seleção das amostra de Teste:
xc1_test<-xc1[seqc1[(ntrain+1):200],] # 20 amostras de treino 
yc1_test<-matrix(0, (nrow=200-ntrain)) # 0 - setosa 
xc2_test<-xc2[seqc2[(ntrain+1):200],] # 20 amostras de treino 
yc2_test<-matrix(1, (nrow=200-ntrain)) # 1 - vesicolor 

#Concatenando dados das 2 classes
xin_train<-as.matrix(rbind(xc1_train, xc2_train))
yd_train<-rbind(yc1_train, yc2_train)
xin_test<-as.matrix(rbind(xc1_test, xc2_test))
yd_test<-rbind(yc1_test, yc2_test)

#_______________________________________________________________________________
# Treinamento:

retlist<-trainPerceptron(xin_train, yd_train, 0.1, 0.01, 100, 1)
wt<-retlist[[1]]
print(wt)

#_______________________________________________________________________________
# Teste:

yt<-yperceptron(xin_test,wt,1)
print(confusionMatrix(table(yt, yd_test)))
