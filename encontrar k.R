###### Encontrar el k
B=10000
m=10
n=30
KK=matrix(data=rep(0,B*m),nrow=B,ncol=n)
dim(KK)
for (b in 1:B) {
  for (i in 1:n){
    KK[b,i]<-ks.test(rnorm(m),"pnorm",0,1)$statistic
  }
  progress <- round((b / B) * 100, 2)
  cat("\rProgress: ", progress, "%", sep = "")
  
  # Actualizar la consola
  flush.console()
}
M= rep(0,B)
for (b in 1:B) {
  M[b]<-max(KK[b,])
}
round(quantile(M,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)

###### Encontrar el k
B=10000
m=10
n=30
KK2=matrix(data=rep(0,B*m),nrow=B,ncol=n)
dim(KK)
for (b in 1:B) {
  for (i in 1:n){
    KK2[b,i]<-ks.test(rnorm(m),"pnorm",0,1)$statistic
  }
  progress <- round((b / B) * 100, 2)
  cat("\rProgress: ", progress, "%", sep = "")
  
  # Actualizar la consola
  flush.console()
}
KK22=cbind(KK,KK2)
M2= rep(0,B)
for (b in 1:B) {
  M2[b]<-max(KK22[b,])
}
quantile(M,c(0.9,0.92,0.95,0.97,0.99,0.995))
quantile(M2,c(0.9,0.92,0.95,0.97,0.99,0.995))

###### Encontrar el k
B=10000
m=10
n=30
KK3=matrix(data=rep(0,B*m),nrow=B,ncol=n)
dim(KK)
for (b in 1:B) {
  for (i in 1:n){
    KK3[b,i]<-ks.test(rnorm(m),"pnorm",0,1)$statistic
  }
  progress <- round((b / B) * 100, 2)
  cat("\rProgress: ", progress, "%", sep = "")
  
  # Actualizar la consola
  flush.console()
}
KK33=cbind(KK2,KK3)
M3= rep(0,B)
for (b in 1:B) {
  M3[b]<-max(KK33[b,])
}
quantile(M,c(0.9,0.92,0.95,0.97,0.99,0.995))
quantile(M3,c(0.9,0.92,0.95,0.97,0.99,0.995))

###### Encontrar el k
B=10000
m=10
n=30
KK4=matrix(data=rep(0,B*m),nrow=B,ncol=n)
dim(KK)
for (b in 1:B) {
  for (i in 1:n){
    KK4[b,i]<-ks.test(rnorm(m),"pnorm",0,1)$statistic
  }
  progress <- round((b / B) * 100, 2)
  cat("\rProgress: ", progress, "%", sep = "")
  
  # Actualizar la consola
  flush.console()
}
KK44=cbind(KK3,KK4)
M4= rep(0,B)
for (b in 1:B) {
  M4[b]<-max(KK44[b,])
}
quantile(M4,c(0.9,0.92,0.95,0.97,0.99,0.995))




###### Encontrar el k
B=10000
m=10
n=30
KK5=matrix(data=rep(0,B*m),nrow=B,ncol=n)
dim(KK)
for (b in 1:B) {
  for (i in 1:n){
    KK5[b,i]<-ks.test(rnorm(m),"pnorm",0,1)$statistic
  }
  progress <- round((b / B) * 100, 2)
  cat("\rProgress: ", progress, "%", sep = "")
  
  # Actualizar la consola
  flush.console()
}
KK55=cbind(KK4,KK5)
M5= rep(0,B)
for (b in 1:B) {
  M5[b]<-max(KK55[b,])
}
round(quantile(M5,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)


###### Encontrar el k
B=10000
m=10
n=30
KK6=matrix(data=rep(0,B*m),nrow=B,ncol=n)
dim(KK)
for (b in 1:B) {
  for (i in 1:n){
    KK6[b,i]<-ks.test(rnorm(m),"pnorm",0,1)$statistic
  }
  progress <- round((b / B) * 100, 2)
  cat("\rProgress: ", progress, "%", sep = "")
  
  # Actualizar la consola
  flush.console()
}
KK66=cbind(KK5,KK6)
M6= rep(0,B)
for (b in 1:B) {
  M6[b]<-max(KK66[b,])
}
round(quantile(M6,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)


###### Encontrar el k
B=10000
m=10
n=30
KK7=matrix(data=rep(0,B*m),nrow=B,ncol=n)
dim(KK)
for (b in 1:B) {
  for (i in 1:n){
    KK7[b,i]<-ks.test(rnorm(m),"pnorm",0,1)$statistic
  }
  progress <- round((b / B) * 100, 2)
  cat("\rProgress: ", progress, "%", sep = "")
  
  # Actualizar la consola
  flush.console()
}
KK77=cbind(KK6,KK7)
M7= rep(0,B)
for (b in 1:B) {
  M7[b]<-max(KK77[b,])
}
round(quantile(M7,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)



###### Encontrar el k
B=10000
m=10
n=30
KK8=matrix(data=rep(0,B*m),nrow=B,ncol=n)
dim(KK)
for (b in 1:B) {
  for (i in 1:n){
    KK8[b,i]<-ks.test(rnorm(m),"pnorm",0,1)$statistic
  }
  progress <- round((b / B) * 100, 2)
  cat("\rProgress: ", progress, "%", sep = "")
  
  # Actualizar la consola
  flush.console()
}
KK88=cbind(KK7,KK8)
M8= rep(0,B)
for (b in 1:B) {
  M8[b]<-max(KK88[b,])
}
round(quantile(M8,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)




###### Encontrar el k
B=10000
m=10
n=30
KK9=matrix(data=rep(0,B*m),nrow=B,ncol=n)
dim(KK)
for (b in 1:B) {
  for (i in 1:n){
    KK9[b,i]<-ks.test(rnorm(m),"pnorm",0,1)$statistic
  }
  progress <- round((b / B) * 100, 2)
  cat("\rProgress: ", progress, "%", sep = "")
  
  # Actualizar la consola
  flush.console()
}
KK99=cbind(KK8,KK9)
M9= rep(0,B)
for (b in 1:B) {
  M9[b]<-max(KK99[b,])
}
round(quantile(M9,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)


###### Encontrar el k
B=10000
m=10
n=30
KK10=matrix(data=rep(0,B*m),nrow=B,ncol=n)
dim(KK)
for (b in 1:B) {
  for (i in 1:n){
    KK10[b,i]<-ks.test(rnorm(m),"pnorm",0,1)$statistic
  }
  progress <- round((b / B) * 100, 2)
  cat("\rProgress: ", progress, "%", sep = "")
  
  # Actualizar la consola
  flush.console()
}
KK1010=cbind(KK9,KK10)
M10= rep(0,B)
for (b in 1:B) {
  M10[b]<-max(KK1010[b,])
}
round(quantile(M10,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)


round(quantile(M,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)
round(quantile(M2,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)
round(quantile(M3,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)
round(quantile(M4,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)
round(quantile(M5,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)
round(quantile(M6,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)
round(quantile(M7,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)
round(quantile(M8,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)
round(quantile(M9,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)
round(quantile(M10,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)


