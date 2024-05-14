### librerías necesarias
library(nortest)



###### Cabiemos el n = 10

########## Potencia de la prueba basada en permutaciones

B=500
d=20
del=seq(-1,1,length=d)
m=100
n=10


# Función de prueba
# Colocar la matriz de datos tranformados
# Bajo la hipótesis nula
# M: Matriz de datos
# Bajo bonferroni al/m
bb.test <- function(M,al=0.05){
  s<- sd(M)
  n=ncol(M)
  m=nrow(M)
  pval = rep(0,n)
  for(i in 1:n){
    pval[i]=ks.test(M[i,],"pnorm",mean=0,sd=s)[[2]]
  }
  return(1-quantile(pval,1-al))
}


# ejemplo con cero


MM=list()
P <- matrix(0,B,n)
for(i in 1:B){
  MM[[i]] <- matrix(0,n,m)
  for (j in 1:n) {
    MM[[i]][j,]<-rnorm(m)
    P[i,j]<-ks.test(MM[[i]][j,],"pnorm",0,1)[[2]]
  }
}
pcol=apply(P, 1, mean)
quantile(pcol,c(0.025,0.975))
quantile(pcol,c(0.01,0.99))
quantile(pcol,c(0.005,0.995))


########### para n=20


###### Cabiemos el n = 20

########## Potencia de la prueba basada en permutaciones

B=500
d=20
del=seq(-1,1,length=d)
m=100
n=20


# Función de prueba
# Colocar la matriz de datos tranformados
# Bajo la hipótesis nula
# M: Matriz de datos
# Bajo bonferroni al/m
bb.test <- function(M,al=0.05){
  s<- sd(M)
  n=ncol(M)
  m=nrow(M)
  pval = rep(0,n)
  for(i in 1:n){
    pval[i]=ks.test(M[i,],"pnorm",mean=0,sd=s)[[2]]
  }
  return(1-quantile(pval,1-al))
}


# ejemplo con cero


MM=list()
P <- matrix(0,B,n)
for(i in 1:B){
  MM[[i]] <- matrix(0,n,m)
  for (j in 1:n) {
    MM[[i]][j,]<-rnorm(m)
    P[i,j]<-ks.test(MM[[i]][j,],"pnorm",0,1)[[2]]
  }
}
pcol=apply(P, 1, mean)
quantile(pcol,c(0.025,0.975))
quantile(pcol,c(0.01,0.99))
quantile(pcol,c(0.005,0.995))


###### Cabiemos el n = 30

########## Potencia de la prueba basada en permutaciones

B=500
d=20
del=seq(-1,1,length=d)
m=100
n=30


# Función de prueba
# Colocar la matriz de datos tranformados
# Bajo la hipótesis nula
# M: Matriz de datos
# Bajo bonferroni al/m
bb.test <- function(M,al=0.05){
  s<- sd(M)
  n=ncol(M)
  m=nrow(M)
  pval = rep(0,n)
  for(i in 1:n){
    pval[i]=ks.test(M[i,],"pnorm",mean=0,sd=s)[[2]]
  }
  return(1-quantile(pval,1-al))
}


# ejemplo con cero


MM=list()
P <- matrix(0,B,n)
for(i in 1:B){
  MM[[i]] <- matrix(0,n,m)
  for (j in 1:n) {
    MM[[i]][j,]<-rnorm(m)
    P[i,j]<-ks.test(MM[[i]][j,],"pnorm",0,1)[[2]]
  }
}
pcol=apply(P, 1, mean)
quantile(pcol,c(0.025,0.975))
quantile(pcol,c(0.01,0.99))
quantile(pcol,c(0.005,0.995))

########## Potencia de la prueba basada en permutaciones

B=500
d=20
del=seq(-1,1,length=d)
m=100
n=50


# Función de prueba
# Colocar la matriz de datos tranformados
# Bajo la hipótesis nula
# M: Matriz de datos
# Bajo bonferroni al/m
bb.test <- function(M,al=0.05){
  s<- sd(M)
  n=ncol(M)
  m=nrow(M)
  pval = rep(0,n)
  for(i in 1:n){
    pval[i]=ks.test(M[i,],"pnorm",mean=0,sd=s)[[2]]
  }
  return(1-quantile(pval,1-al))
}


# ejemplo con cero


MM=list()
P <- matrix(0,B,n)
for(i in 1:B){
  MM[[i]] <- matrix(0,n,m)
  for (j in 1:n) {
    MM[[i]][j,]<-rnorm(m)
    P[i,j]<-ks.test(MM[[i]][j,],"pnorm",0,1)[[2]]
  }
}
pcol=apply(P, 1, mean)
quantile(pcol,c(0.025,0.975))
quantile(pcol,c(0.01,0.99))
quantile(pcol,c(0.005,0.995))


###### Cabiemos el n = 50

########## Potencia de la prueba basada en permutaciones

B=500
d=20
del=seq(-1,1,length=d)
m=100
n=50


# Función de prueba
# Colocar la matriz de datos tranformados
# Bajo la hipótesis nula
# M: Matriz de datos
# Bajo bonferroni al/m
bb.test <- function(M,al=0.05){
  s<- sd(M)
  n=ncol(M)
  m=nrow(M)
  pval = rep(0,n)
  for(i in 1:n){
    pval[i]=ks.test(M[i,],"pnorm",mean=0,sd=s)[[2]]
  }
  return(1-quantile(pval,1-al))
}


# ejemplo con cero


MM=list()
P <- matrix(0,B,n)
for(i in 1:B){
  MM[[i]] <- matrix(0,n,m)
  for (j in 1:n) {
    MM[[i]][j,]<-rnorm(m)
    P[i,j]<-ks.test(MM[[i]][j,],"pnorm",0,1)[[2]]
  }
}
pcol=apply(P, 1, mean)
quantile(pcol,c(0.025,0.975))
quantile(pcol,c(0.01,0.99))
quantile(pcol,c(0.005,0.995))

########## Potencia de la prueba basada en permutaciones

B=500
d=20
del=seq(-1,1,length=d)
m=100
n=50


# Función de prueba
# Colocar la matriz de datos tranformados
# Bajo la hipótesis nula
# M: Matriz de datos
# Bajo bonferroni al/m
bb.test <- function(M,al=0.05){
  s<- sd(M)
  n=ncol(M)
  m=nrow(M)
  pval = rep(0,n)
  for(i in 1:n){
    pval[i]=ks.test(M[i,],"pnorm",mean=0,sd=s)[[2]]
  }
  return(1-quantile(pval,1-al))
}


# ejemplo con cero


MM=list()
P <- matrix(0,B,n)
for(i in 1:B){
  MM[[i]] <- matrix(0,n,m)
  for (j in 1:n) {
    MM[[i]][j,]<-rnorm(m)
    P[i,j]<-ks.test(MM[[i]][j,],"pnorm",0,1)[[2]]
  }
}
pcol=apply(P, 1, mean)
quantile(pcol,c(0.025,0.975))
quantile(pcol,c(0.01,0.99))
quantile(pcol,c(0.005,0.995))




###### Cabiemos el n = 70

########## Potencia de la prueba basada en permutaciones

B=500
d=20
del=seq(-1,1,length=d)
m=100
n=70


# Función de prueba
# Colocar la matriz de datos tranformados
# Bajo la hipótesis nula
# M: Matriz de datos
# Bajo bonferroni al/m
bb.test <- function(M,al=0.05){
  s<- sd(M)
  n=ncol(M)
  m=nrow(M)
  pval = rep(0,n)
  for(i in 1:n){
    pval[i]=ks.test(M[i,],"pnorm",mean=0,sd=s)[[2]]
  }
  return(1-quantile(pval,1-al))
}


# ejemplo con cero


MM=list()
P <- matrix(0,B,n)
for(i in 1:B){
  MM[[i]] <- matrix(0,n,m)
  for (j in 1:n) {
    MM[[i]][j,]<-rnorm(m)
    P[i,j]<-ks.test(MM[[i]][j,],"pnorm",0,1)[[2]]
  }
}
pcol=apply(P, 1, mean)
quantile(pcol,c(0.025,0.975))
quantile(pcol,c(0.01,0.99))
quantile(pcol,c(0.005,0.995))
###### Cabiemos el n = 100

########## Potencia de la prueba basada en permutaciones

B=500
d=20
del=seq(-1,1,length=d)
m=100
n=100


# Función de prueba
# Colocar la matriz de datos tranformados
# Bajo la hipótesis nula
# M: Matriz de datos
# Bajo bonferroni al/m
bb.test <- function(M,al=0.05){
  s<- sd(M)
  n=ncol(M)
  m=nrow(M)
  pval = rep(0,n)
  for(i in 1:n){
    pval[i]=ks.test(M[i,],"pnorm",mean=0,sd=s)[[2]]
  }
  return(1-quantile(pval,1-al))
}


# ejemplo con cero


MM=list()
P <- matrix(0,B,n)
for(i in 1:B){
  MM[[i]] <- matrix(0,n,m)
  for (j in 1:n) {
    MM[[i]][j,]<-rnorm(m)
    P[i,j]<-ks.test(MM[[i]][j,],"pnorm",0,1)[[2]]
  }
}
pcol=apply(P, 1, mean)
quantile(pcol,c(0.025,0.975))
quantile(pcol,c(0.01,0.99))
quantile(pcol,c(0.005,0.995))
#  CONTINUAR
library(MASS)
A<-matrix(c(1,-4,2,1,-3,6,3,-11,10),3,3,byrow=T)
norm( ginv(A)%*%matrix(c(2,0,4),3))







