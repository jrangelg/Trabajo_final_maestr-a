# Fraccionario H<-1/2}

# Parámetros iniciales
n_simulations <- 100  # Número de simulaciones
num_trajectories<-100
n_steps <- 1000  # Número de pasos en cada simulación
dt <- 1 / (n_steps)  # Tamaño del paso
t <- cumsum(rep(dt,n_steps))
# Movimiento browniano


# Grid
# H=1/4
I=0#Initial point
F=1#Final point
H<-1/5
sig<-1
N<-1000
n<-100
dt<-(F-I)/N
t<-seq(I,F,length=N)#Domain
p <- length(t) - 1
# Función para generar simulaciones del Puente Browniano
N<-n_steps
C<-matrix(0,N-1,N-1)
for (i in 2:N) {
  for (j in 2:N) {
    C[i-1,j-1] <- (1/2)*((abs(t[i])^(2*H) + abs(t[j])^(2*H) - abs(t[i]-t[j])^(2*H)))
  }
}
I <- matrix(rnorm((n) * p, 0, 1 / sqrt(p)), n, p)%*%chol(C)
B<-I
dim(B)
for (i in 1:100) {
  for (j in 1:999) {
    B[i,j]<-I[i,j]-(j/1000)*I[i,999]
  }
}
brownian_trajectories <- matrix(0,n,n_steps)
for (i in 1:n) {
  brownian_trajectories[i,]<-sin(t*pi)+10*c(B[i,],0)
}  
plot(c(I[1,],0),type="l")
star <- c(1,1)
fin<-c(10,9)
windows(width = 8, height = 3)  # Define una ventana con ancho 8 y alto 6 (en pulgadas)
par(mfrow = c(1, 4))
plot(NULL, xlim = c(-0.5, 1002), ylim = c(-0.5, 2), xlab = "Tiempo", ylab = "Posición",
     main = expression( sigma ~ "=1/2 y H=1/4"),cex.main = 1.5)

for (i in 1:n_simulations) {
  lines(brownian_trajectories[i,], col = "#0DD8F7", lwd = 0.4)
}


Datos<- matrix(0,n_simulations,n_steps)
for (i in 1:n_simulations){
  Datos[i,]<-brownian_trajectories[i,]
}
or2.5<-NULL
for (i in 1:n_steps) {
  or2.5[i] <- quantile(Datos[,i],0.025)
}
or97.5<-NULL
for (i in 1:n_steps) {
  or97.5[i] <- quantile(Datos[,i],0.975)
}
or5<-NULL
for (i in 1:n_steps) {
  or5[i] <- quantile(Datos[,i],0.05)
}
or95<-NULL
for (i in 1:n_steps) {
  or95[i] <- quantile(Datos[,i],0.95)
}
med<-NULL
for (i in 1:n_steps) {
  med[i] <- mean(Datos[,i])
}
lines(or2.5, col = "#F7820D", lwd = 2)
lines(or97.5, col = "#F7820D", lwd = 2)
lines(or5, col = "green", lwd = 2)
lines(or95, col = "green", lwd = 2)
lines(med, col = "red", lwd = 2)




brownian_trajectories <- matrix(0,n,n_steps)
for (i in 1:n) {
  brownian_trajectories[i,]<-sin(t*pi)+7*c(B[i,],0)
}  
star <- c(1,1)
fin<-c(10,9)
plot(NULL, xlim = c(-0.5, 1002), ylim = c(-0.5, 2), xlab = "Tiempo", ylab = "Posición",
     main = expression( sigma ~ "=3/10 y H=1/4"),cex.main = 1.5)

for (i in 1:n_simulations) {
  lines(brownian_trajectories[i,], col = "#0DD8F7", lwd = 0.4)
}


Datos<- matrix(0,n_simulations,n_steps)
for (i in 1:n_simulations){
  Datos[i,]<-brownian_trajectories[i,]
}
or2.5<-NULL
for (i in 1:n_steps) {
  or2.5[i] <- quantile(Datos[,i],0.025)
}
or97.5<-NULL
for (i in 1:n_steps) {
  or97.5[i] <- quantile(Datos[,i],0.975)
}
or5<-NULL
for (i in 1:n_steps) {
  or5[i] <- quantile(Datos[,i],0.05)
}
or95<-NULL
for (i in 1:n_steps) {
  or95[i] <- quantile(Datos[,i],0.95)
}
med<-NULL
for (i in 1:n_steps) {
  med[i] <- mean(Datos[,i])
}
lines(or2.5, col = "#F7820D", lwd = 2)
lines(or97.5, col = "#F7820D", lwd = 2)
lines(or5, col = "green", lwd = 2)
lines(or95, col = "green", lwd = 2)
lines(med, col = "red", lwd = 2)

#2/10
brownian_trajectories <- matrix(0,n,n_steps)
for (i in 1:n) {
  brownian_trajectories[i,]<-sin(t*pi)+5*c(B[i,],0)
}  
star <- c(1,1)
fin<-c(10,9)
plot(NULL, xlim = c(-0.5, 1002), ylim = c(-0.5, 2), xlab = "Tiempo", ylab = "Posición",
     main = expression( sigma ~ "=2/10 y H=1/4"),cex.main = 1.5)

for (i in 1:n_simulations) {
  lines(brownian_trajectories[i,], col = "#0DD8F7", lwd = 0.4)
}


Datos<- matrix(0,n_simulations,n_steps)
for (i in 1:n_simulations){
  Datos[i,]<-brownian_trajectories[i,]
}
or2.5<-NULL
for (i in 1:n_steps) {
  or2.5[i] <- quantile(Datos[,i],0.025)
}
or97.5<-NULL
for (i in 1:n_steps) {
  or97.5[i] <- quantile(Datos[,i],0.975)
}
or5<-NULL
for (i in 1:n_steps) {
  or5[i] <- quantile(Datos[,i],0.05)
}
or95<-NULL
for (i in 1:n_steps) {
  or95[i] <- quantile(Datos[,i],0.95)
}
med<-NULL
for (i in 1:n_steps) {
  med[i] <- mean(Datos[,i])
}
lines(or2.5, col = "#F7820D", lwd = 2)
lines(or97.5, col = "#F7820D", lwd = 2)
lines(or5, col = "green", lwd = 2)
lines(or95, col = "green", lwd = 2)
lines(med, col = "red", lwd = 2)

#####1/10

brownian_trajectories <- matrix(0,n,n_steps)
for (i in 1:n) {
  brownian_trajectories[i,]<-sin(t*pi)+2*c(B[i,],0)
}  
star <- c(1,1)
fin<-c(10,9)
plot(NULL, xlim = c(-0.5, 1002), ylim = c(-0.5, 2), xlab = "Tiempo", ylab = "Posición",
     main = expression( sigma ~ "=2/10 y H=1/4"),cex.main = 1.5)

for (i in 1:n_simulations) {
  lines(brownian_trajectories[i,], col = "#0DD8F7", lwd = 0.4)
}


Datos<- matrix(0,n_simulations,n_steps)
for (i in 1:n_simulations){
  Datos[i,]<-brownian_trajectories[i,]
}
or2.5<-NULL
for (i in 1:n_steps) {
  or2.5[i] <- quantile(Datos[,i],0.025)
}
or97.5<-NULL
for (i in 1:n_steps) {
  or97.5[i] <- quantile(Datos[,i],0.975)
}
or5<-NULL
for (i in 1:n_steps) {
  or5[i] <- quantile(Datos[,i],0.05)
}
or95<-NULL
for (i in 1:n_steps) {
  or95[i] <- quantile(Datos[,i],0.95)
}
med<-NULL
for (i in 1:n_steps) {
  med[i] <- mean(Datos[,i])
}
lines(or2.5, col = "#F7820D", lwd = 2)
lines(or97.5, col = "#F7820D", lwd = 2)
lines(or5, col = "green", lwd = 2)
lines(or95, col = "green", lwd = 2)
lines(med, col = "red", lwd = 2)





# Fraccionario H<-1/2}

# Parámetros iniciales
n_simulations <- 100  # Número de simulaciones
num_trajectories<-100
n_steps <- 1000  # Número de pasos en cada simulación
dt <- 1 / (n_steps)  # Tamaño del paso
t <- cumsum(rep(dt,n_steps))
# Movimiento browniano


# Grid
# H=1/4
I=0#Initial point
F=1#Final point
H<-3/4
sig<-1
N<-1000
n<-100
dt<-(F-I)/N
t<-seq(I,F,length=N)#Domain
p <- length(t) - 1
# Función para generar simulaciones del Puente Browniano
N<-n_steps
C<-matrix(0,N-1,N-1)
for (i in 2:N) {
  for (j in 2:N) {
    C[i-1,j-1] <- (1/2)*((abs(t[i])^(2*H) + abs(t[j])^(2*H) - abs(t[i]-t[j])^(2*H)))
  }
}
I <- matrix(rnorm((n) * p, 0, 1 / sqrt(p)), n, p)%*%chol(C)
B<-I
dim(B)
for (i in 1:100) {
  for (j in 1:999) {
    B[i,j]<-I[i,j]-(j/1000)*I[i,999]
  }
}

brownian_trajectories <- matrix(0,n,n_steps)
for (i in 1:n) {
  brownian_trajectories[i,]<-sin(t*pi)+2*10*c(B[i,],0)
}  
star <- c(1,1)
fin<-c(10,9)
windows(width = 8, height = 3)  # Define una ventana con ancho 8 y alto 6 (en pulgadas)
par(mfrow = c(1, 4))
plot(NULL, xlim = c(-0.5, 1002), ylim = c(-0.5, 2), xlab = "Tiempo", ylab = "Posición",
     main = expression( sigma ~ "=1/2 y H=3/4"),cex.main = 1.5)

for (i in 1:n_simulations) {
  lines(brownian_trajectories[i,], col = "#0DD8F7", lwd = 0.4)
}


Datos<- matrix(0,n_simulations,n_steps)
for (i in 1:n_simulations){
  Datos[i,]<-brownian_trajectories[i,]
}
or2.5<-NULL
for (i in 1:n_steps) {
  or2.5[i] <- quantile(Datos[,i],0.025)
}
or97.5<-NULL
for (i in 1:n_steps) {
  or97.5[i] <- quantile(Datos[,i],0.975)
}
or5<-NULL
for (i in 1:n_steps) {
  or5[i] <- quantile(Datos[,i],0.05)
}
or95<-NULL
for (i in 1:n_steps) {
  or95[i] <- quantile(Datos[,i],0.95)
}
med<-NULL
for (i in 1:n_steps) {
  med[i] <- mean(Datos[,i])
}
lines(or2.5, col = "#F7820D", lwd = 2)
lines(or97.5, col = "#F7820D", lwd = 2)
lines(or5, col = "green", lwd = 2)
lines(or95, col = "green", lwd = 2)
lines(med, col = "red", lwd = 2)




brownian_trajectories <- matrix(0,n,n_steps)
for (i in 1:n) {
  brownian_trajectories[i,]<-sin(t*pi)+2*7*c(B[i,],0)
}  
star <- c(1,1)
fin<-c(10,9)
plot(NULL, xlim = c(-0.5, 1002), ylim = c(-0.5, 2), xlab = "Tiempo", ylab = "Posición",
     main = expression( sigma ~ "=3/10 y H=3/4"),cex.main = 1.5)

for (i in 1:n_simulations) {
  lines(brownian_trajectories[i,], col = "#0DD8F7", lwd = 0.4)
}


Datos<- matrix(0,n_simulations,n_steps)
for (i in 1:n_simulations){
  Datos[i,]<-brownian_trajectories[i,]
}
or2.5<-NULL
for (i in 1:n_steps) {
  or2.5[i] <- quantile(Datos[,i],0.025)
}
or97.5<-NULL
for (i in 1:n_steps) {
  or97.5[i] <- quantile(Datos[,i],0.975)
}
or5<-NULL
for (i in 1:n_steps) {
  or5[i] <- quantile(Datos[,i],0.05)
}
or95<-NULL
for (i in 1:n_steps) {
  or95[i] <- quantile(Datos[,i],0.95)
}
med<-NULL
for (i in 1:n_steps) {
  med[i] <- mean(Datos[,i])
}
lines(or2.5, col = "#F7820D", lwd = 2)
lines(or97.5, col = "#F7820D", lwd = 2)
lines(or5, col = "green", lwd = 2)
lines(or95, col = "green", lwd = 2)
lines(med, col = "red", lwd = 2)

#2/10
brownian_trajectories <- matrix(0,n,n_steps)
for (i in 1:n) {
  brownian_trajectories[i,]<-sin(t*pi)+2*5*c(B[i,],0)
}  
star <- c(1,1)
fin<-c(10,9)
plot(NULL, xlim = c(-0.5, 1002), ylim = c(-0.5, 2), xlab = "Tiempo", ylab = "Posición",
     main = expression( sigma ~ "=2/10 y H=3/4"),cex.main = 1.5)

for (i in 1:n_simulations) {
  lines(brownian_trajectories[i,], col = "#0DD8F7", lwd = 0.4)
}


Datos<- matrix(0,n_simulations,n_steps)
for (i in 1:n_simulations){
  Datos[i,]<-brownian_trajectories[i,]
}
or2.5<-NULL
for (i in 1:n_steps) {
  or2.5[i] <- quantile(Datos[,i],0.025)
}
or97.5<-NULL
for (i in 1:n_steps) {
  or97.5[i] <- quantile(Datos[,i],0.975)
}
or5<-NULL
for (i in 1:n_steps) {
  or5[i] <- quantile(Datos[,i],0.05)
}
or95<-NULL
for (i in 1:n_steps) {
  or95[i] <- quantile(Datos[,i],0.95)
}
med<-NULL
for (i in 1:n_steps) {
  med[i] <- mean(Datos[,i])
}
lines(or2.5, col = "#F7820D", lwd = 2)
lines(or97.5, col = "#F7820D", lwd = 2)
lines(or5, col = "green", lwd = 2)
lines(or95, col = "green", lwd = 2)
lines(med, col = "red", lwd = 2)

#####1/10

brownian_trajectories <- matrix(0,n,n_steps)
for (i in 1:n) {
  brownian_trajectories[i,]<-sin(t*pi)+2*2*c(B[i,],0)
}  
star <- c(1,1)
fin<-c(10,9)
plot(NULL, xlim = c(-0.5, 1002), ylim = c(-0.5, 2), xlab = "Tiempo", ylab = "Posición",
     main = expression( sigma ~ "=2/10 y H=3/4"),cex.main = 1.5)

for (i in 1:n_simulations) {
  lines(brownian_trajectories[i,], col = "#0DD8F7", lwd = 0.4)
}


Datos<- matrix(0,n_simulations,n_steps)
for (i in 1:n_simulations){
  Datos[i,]<-brownian_trajectories[i,]
}
or2.5<-NULL
for (i in 1:n_steps) {
  or2.5[i] <- quantile(Datos[,i],0.025)
}
or97.5<-NULL
for (i in 1:n_steps) {
  or97.5[i] <- quantile(Datos[,i],0.975)
}
or5<-NULL
for (i in 1:n_steps) {
  or5[i] <- quantile(Datos[,i],0.05)
}
or95<-NULL
for (i in 1:n_steps) {
  or95[i] <- quantile(Datos[,i],0.95)
}
med<-NULL
for (i in 1:n_steps) {
  med[i] <- mean(Datos[,i])
}
lines(or2.5, col = "#F7820D", lwd = 2)
lines(or97.5, col = "#F7820D", lwd = 2)
lines(or5, col = "green", lwd = 2)
lines(or95, col = "green", lwd = 2)
lines(med, col = "red", lwd = 2)