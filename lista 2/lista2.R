#teste
rm(list = ls())
x = seq(-1, 1, by = 0.1)
y = seq(-1, 1, by = 0.1)
create_grid <- expand.grid(x,y)
circle <- function(x,y) {
  return(sqrt(x^2+y^2))
}
plot(create_grid, type="n", xaxis="teste");
par(new=T)
raio = 0.6
classe = 1*(circle(create_grid$Var1,create_grid$Var2)>raio)
index=1
for (v1 in y) {
  for (v2 in x) {
    if(classe[index] == 1){
      points(v2,v1)      
    }else{
      points(v2,v1,col="red")
    }
    index=index+1
  }
}


