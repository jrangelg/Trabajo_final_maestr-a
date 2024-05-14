########################################
#                                      #
#    REGIONES PUENTES DE CONFIANZA     #
#                                      #
########################################


######## Trayectorias 

# Simulación puente browniano fraccionario

p1<-c(1,1)
p2<-c(2,2)
# Función que toma la trayectoria del punto inicial y el final
al <- function(p1,p2,N){
  t<-seq(0,1,length=N)
  alt<-matrix(0,N,2)
  for(i in 1:N){
    alt[i,1]<-p1[1]+(p2[1]-p1[1])*t[i]
    alt[i,2]<-p1[2]+(p2[2]-p1[2])*t[i]
  }
  return(alt)
}
al(p1,p2,100)
# Función que ayuda a realizar un puente browniano 
#con desviación y numero de iteraciones
fbbm<-function(sig,N){
  t<-seq(0,1,length=N)
  z<-c(0,cumsum(rnorm(N-1)))
  bbm <- NULL
  for( i in 1:N){
    bbm[i]<-sig*(z[i]-t[i]*z[N])
  }
  return(bbm)
}

windows()
plot(fbbm(5,100),type="l")
# puente browniano en R2
N<-100
sig<-0.3
p1<-c(1,1);p2<-c(2,8)
n<-(p2-p1)/sqrt(sum((p2-p1)^2))*c(-1,1)
n
t<-seq(0,1,length=N)
w<-matrix(0,N,2)
trend<-al(p1,p2,N)
f<-fbbm(sig,N)
length(f)
for(i in 1:N){
  w[i,1]<-trend[i,1]+n[1]*f[i]
  w[i,2]<-trend[i,2]+n[2]*f[i]
}
plot(w[,1],w[,2],type="l")

# Función que simula un puente browniano en R2
fbbr<-function(p1,p2,sig,N){
  n<-(p2-p1)/sqrt(sum((p2-p1)^2))*c(-1,1)
  n
  t<-seq(0,1,length=N)
  alt<-matrix(0,N,2)
  for(i in 1:N){
    alt[i,1]<-p1[1]+(p2[1]-p1[1])*t[i]
    alt[i,2]<-p1[2]+(p2[2]-p1[2])*t[i]
  }
  w<-matrix(0,N,2)
  trend<-alt
  #C<-matrix(0,N-1,N-1)
 # for (i in 2:N) {
 #   for (j in 2:N) {
 #     C[i-1,j-1] <- (1/2)*((abs(t[i])^(2*H) + abs(t[j])^(2*H) - abs(t[i]-t[j])^(2*H)))
 #   }
 # }
  z<-cumsum(c(0,rnorm(N)))
  bbm <- NULL
  for( i in 1:N){
    bbm[i]<-sig*(z[i]-t[i]*z[N])
  }
  f<-bbm
  length(f)
  for(i in 1:N){
    w[i,1]<-trend[i,1]+n[1]*f[i]
    w[i,2]<-trend[i,2]+n[2]*f[i]
  }
  return(w)
}
windows()
p1<-c(1,1)
p2<-c(5,8)
N<-100
sig<-0.5

fbbr(p1,p2,sig,N)
# FUNCIÓN


# Ejemplo
par(mfrow = c(1, 1))
p1<-c(1,1)
p2<-c(5,8)
N<-500
sig<-0.05
H<-1/2
n<-200
ww<-  fbbr(p1,p2,sig,N)
plot(ww[, 1], ww[, 2], xlim = c(0, 8), ylim = c(0, 8), type = "l", main = "Migración en dos trayectorias",
     ylab = "Latitud", xlab = "Longitud", col = "#0DF7B7")

# Superponer las curvas adicionales utilizando lines dentro del bucle

for (k in 1:n) {
  ww <- fbbr(p1, p2, sig, N)
  lines(ww[, 1], ww[, 2], type = "l", col = sample(c("#0DF7B7","#1CF4A7","#0AB7B7","#0BC7C1"),1))
}
ww1<-matrix(0,n,N)
for (k in 1:n) {
  ww1[k,]<-fbbm(sig,N)
}
n<-(p2-p1)/sqrt(sum((p2-p1)^2))*c(-1,1)
n
t<-seq(0,1,length=N)
a<-rep(0,N)
b<-rep(0,N)
for (i in 1:N) {
  a[i]<-quantile(ww1[,i],0.025)
  b[i]<-quantile(ww1[,i],0.975)
}
alt<-matrix(0,N,2)
for(i in 1:N){
  alt[i,1]<-p1[1]+(p2[1]-p1[1])*t[i]
  alt[i,2]<-p1[2]+(p2[2]-p1[2])*t[i]
}

lines(alt[,1]+a*n[1],alt[,2]+a*n[2],type="l",col="red",lwd=2)
lines(alt[,1]+b*n[1],alt[,2]+b*n[2],type="l",col="red",lwd=2)

p1<-c(5,8)
p2<-c(8,5)
N<-500
sig<-0.02
H<-1/2
n<-200
ww<-  fbbr(p1,p2,sig,N)

# Superponer las curvas adicionales utilizando lines dentro del bucle

for (k in 1:n) {
  ww <- fbbr(p1, p2, sig, N)
  lines(ww[, 1], ww[, 2], type = "l", col = sample(c("#0DF7B7","#1CF4A7","#0AB7B7","#0BC7C1"),1))
}
ww1<-matrix(0,n,N)
for (k in 1:n) {
  ww1[k,]<-fbbm(sig,N)
}
n<-(p2-p1)/sqrt(sum((p2-p1)^2))*c(-1,1)
n
t<-seq(0,1,length=N)
a<-rep(0,N)
b<-rep(0,N)
for (i in 1:N) {
  a[i]<-quantile(ww1[,i],0.025)
  b[i]<-quantile(ww1[,i],0.975)
}
alt<-matrix(0,N,2)
for(i in 1:N){
  alt[i,1]<-p1[1]+(p2[1]-p1[1])*t[i]
  alt[i,2]<-p1[2]+(p2[2]-p1[2])*t[i]
}

lines(alt[,1]+a*n[1],alt[,2]+a*n[2],type="l",col="red",lwd=2)
lines(alt[,1]+b*n[1],alt[,2]+b*n[2],type="l",col="red",lwd=2)
