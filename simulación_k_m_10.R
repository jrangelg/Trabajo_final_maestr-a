###### Encontrar el k
B=10000
m=10
n=10
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
n=10
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
n=10
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
n=10
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
n=10
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
n=10
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
n=10
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
n=10
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
n=10
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
n=10
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


library(ggplot2)

# Tu dataframe con los cuantiles
quantiles_df <- data.frame(
  n = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
  `90%` = c(0.486, 0.518, 0.533, 0.544, 0.552, 0.560, 0.566, 0.570, 0.574, 0.578),
  `92%` = c(0.497, 0.528, 0.542, 0.553, 0.562, 0.567, 0.573, 0.578, 0.583, 0.587),
  `95%` = c(0.518, 0.546, 0.561, 0.570, 0.578, 0.585, 0.591, 0.595, 0.599, 0.602),
  `97%` = c(0.537, 0.565, 0.579, 0.591, 0.597, 0.602, 0.608, 0.611, 0.616, 0.618),
  `99%` = c(0.576, 0.602, 0.615, 0.623, 0.629, 0.634, 0.640, 0.643, 0.646, 0.651),
  `99.5%` = c(0.602, 0.621, 0.636, 0.642, 0.645, 0.652, 0.657, 0.660, 0.662, 0.665)
)

# Reorganizar el dataframe en formato largo para facilitar el uso con ggplot2
quantiles_df_long <- tidyr::gather(quantiles_df, key = "Cuantil", value = "Valor", -n)

# Crear el gráfico
ggplot(quantiles_df_long, aes(x = n, y = Valor, color = Cuantil)) +
  geom_line() +
  labs(title = "Gráfico de Cuantiles para m = 10",
       x = "Tamaño de Muestra",
       y = "Valor del Estadístico") +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#ff7f00", "#ffff33", "#a65628")) +
  theme_minimal()

