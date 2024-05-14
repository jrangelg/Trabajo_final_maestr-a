d=1
m=50
n=100
B=1000
del<-seq(-d,d,length=m)
data<-list()
for(k in 1:m){
  dat<-matrix(0,B,n)
  for(i in 1:B){
    dat[i,]<-rnorm(n,mean=del[k],sd=1)
  }
  data[[k]]<-dat
}
data[[1]]
pval<-matrix(0,B,m)
for(i in 1:B){
  for (j in 1:m) {
    pval[i,j]<-t.test(data[[j]][i,])[[3]]
  }
}
pot<-rep(0,m)
al<-0.05
for(i in 1:m){
  pot[i]<-mean(pval[,i]<al)
}
plot(del,pot,type="l")
?shapiro.test


n<-20
m<-







