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
  z<-c(0,cumsum(rnorm(N-1)))
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
  plot(w[,1],w[,2],type="l")
}
windows()
p1<-c(1,1)
p2<-c(5,8)
N<-100
sig<-0.05


# FUNCIÓN

fbbm<-function(p1,p2,N,sig){
n<-(p2-p1)/sqrt(sum((p2-p1)^2))*c(-1,1)
t<-seq(0,1,length=N)
alt<-matrix(0,N,2)
for(i in 1:N){
  alt[i,1]<-p1[1]+(p2[1]-p1[1])*t[i]
  alt[i,2]<-p1[2]+(p2[2]-p1[2])*t[i]
}
w<-matrix(0,N,2)
trend<-alt
z<-c(0,cumsum(rnorm(N-1)))
bbm <- NULL
for( i in 1:N){
  bbm[i]<-sig*(z[i]-t[i]*z[N])
}
f<-bbm
for(i in 1:N){
  w[i,1]<-trend[i,1]+n[1]*f[i]
  w[i,2]<-trend[i,2]+n[2]*f[i]
}
w
}
ww<- n<-(p2-p1)/sqrt(sum((p2-p1)^2))*c(-1,1)
  n
  t<-seq(0,1,length=N)
  alt<-matrix(0,N,2)
  for(i in 1:N){
    alt[i,1]<-p1[1]+(p2[1]-p1[1])*t[i]
    alt[i,2]<-p1[2]+(p2[2]-p1[2])*t[i]
  }
  w<-matrix(0,N,2)
  trend<-alt
  z<-c(0,cumsum(rnorm(N-1)))
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
# Ejemplo
  p1<-c(1,1)
  p2<-c(5,8)
  N<-50
  sig<-0.2
  
  ww<-  fbbm(p1,p2,N,sig)
plot(ww[,1],ww[,2],xlim=c(0,8),ylim=c(0,8),type="l",main="Migración en dos trayectorias",
     ylab = "Latitud", xlab="Longitud")
for(k in 1:50){
  ww<-  fbbm(p1,p2,N,sig)
  lines(ww[,1],ww[,2],type="l", col=k)
}
q1=c(5,8)
q2=c(8,5)
sig=0.1
www<-  fbbm(q1,q2,N,sig)
lines(www[,1],www[,2],type="l")
for(k in 1:50){
  www<-  fbbm(q1,q2,N,sig)
  lines(www[,1],www[,2],type="l", col=k)
}
