###### Encontrar el k
B=10000
m=10
n=50
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
n=50
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
n=50
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
KK33=cbind(KK22,KK3)
M3= rep(0,B)
for (b in 1:B) {
  M3[b]<-max(KK33[b,])
}
quantile(M,c(0.9,0.92,0.95,0.97,0.99,0.995))
quantile(M3,c(0.9,0.92,0.95,0.97,0.99,0.995))

###### Encontrar el k
B=10000
m=10
n=50
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
KK44=cbind(KK33,KK4)
M4= rep(0,B)
for (b in 1:B) {
  M4[b]<-max(KK44[b,])
}
quantile(M4,c(0.9,0.92,0.95,0.97,0.99,0.995))




###### Encontrar el k
B=10000
m=10
n=50
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
KK55=cbind(KK44,KK5)
M5= rep(0,B)
for (b in 1:B) {
  M5[b]<-max(KK55[b,])
}
round(quantile(M5,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)


###### Encontrar el k
B=10000
m=10
n=50
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
KK66=cbind(KK55,KK6)
M6= rep(0,B)
for (b in 1:B) {
  M6[b]<-max(KK66[b,])
}
round(quantile(M6,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)


###### Encontrar el k
B=10000
m=10
n=50
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
KK77=cbind(KK66,KK7)
M7= rep(0,B)
for (b in 1:B) {
  M7[b]<-max(KK77[b,])
}
round(quantile(M7,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)



###### Encontrar el k
B=10000
m=10
n=50
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
KK88=cbind(KK77,KK8)
M8= rep(0,B)
for (b in 1:B) {
  M8[b]<-max(KK88[b,])
}
round(quantile(M8,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)




###### Encontrar el k
B=10000
m=10
n=50
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
KK99=cbind(KK88,KK9)
M9= rep(0,B)
for (b in 1:B) {
  M9[b]<-max(KK99[b,])
}
round(quantile(M9,c(0.9,0.92,0.95,0.97,0.99,0.995)),3)


###### Encontrar el k
B=10000
m=10
n=50
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
KK1010=cbind(KK99,KK10)
M10= rep(0,B)
for (b in 1:B) {
  M10[b]<-max(KK1010[b,])
}
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


library(ggplot2)

# Crear un dataframe con los datos
datos <- data.frame(
  n = 1:10,
  X90 = c(0.552, 0.578, 0.592, 0.601, 0.609, 0.615, 0.620, 0.624, 0.628, 0.631),
  X92 = c(0.560, 0.586, 0.600, 0.609, 0.617, 0.622, 0.627, 0.632, 0.635, 0.638),
  X95 = c(0.578, 0.602, 0.615, 0.624, 0.632, 0.638, 0.643, 0.646, 0.649, 0.653),
  X97 = c(0.596, 0.619, 0.630, 0.641, 0.649, 0.655, 0.661, 0.665, 0.667, 0.668),
  X99 = c(0.629, 0.653, 0.668, 0.677, 0.684, 0.689, 0.692, 0.696, 0.697, 0.698),
  X995 = c(0.650, 0.675, 0.685, 0.695, 0.702, 0.707, 0.711, 0.714, 0.716, 0.717)
)

# Gráfico
library(ggplot2)
library(tidyr)

datos_long <- gather(datos, key = "Cuantil", value = "Valor", -n)

ggplot(datos_long, aes(x = n, y = Valor, color = Cuantil)) +
  geom_line() +
  labs(title = "Gráfico de Cuantiles para m=50",
       x = "Tamaño de muestra",
       y = "Cuantiles") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "brown"))
