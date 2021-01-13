#covariance matrix
library(reshape2)

a<-data.frame(rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1))
b<-data.frame(rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1))
ncol(a)

melt()

covmat<-funtion(data){
  k<-ncol(data)
  l<-rep(k, each=k)
  matrix(ifelse(), ncol=k)
}
l<-rep(l,each=l1)
matrix(cov([,k],cov{,l} , ncol = ncol(a), )

cov(a[,1],a[,1])
cov(a[,1],a[,2])
cov(a[,1],a[,3])
cov(a[,1],a[,4])

matrix
for(i in 4){
  l<-seq(i,1)
  v<-cov(a[,i], a[,l])
  print(v)
}


seq(10, 1)


## 
a1<-cov(a)
b1<-cov(b)


##boot
k<-lm(a$rnorm.100..0..1. ~ a$rnorm.100..0..1..1 + a$rnorm.100..0..1..2)
s<-summary(k)
s$coefficients[2,1]
View((k$coefficients[2]))
View(k)

bootspeed<-function(ny,nx,d,m){
set.seed(1)
  for (i in m) {
   for (i in 10) {
    n<-sample(1:nrow(d),nrow(d),replace = T)
    nnk<-a[n,]
    nnk<-as.data.frame(nnk)
    }
      k<-lm(nnk[,ny]~ nnk[,nx], nnk)
      coco<-c(k$coefficients[2])
      }
  print(coco)
}
bootspeed(1,2,a,100)


set.seed(1)
for (i in 100) {
  for (i in 10) {
    n<-sample(1:nrow(a),nrow(a),replace = T)
    nnk<-a[n,]
    nnk<-as.data.frame(nnk)
  }
  k<-lm(nnk[,1]~ nnk[,2], nnk)
  coco<-c(k$coefficients[2])
}
print(coco)

for(v in 100){
for (i in 10) {
  n<-sample(1:nrow(a),nrow(a),replace = T)
  nnk<-a[n,]
  nnk<-as.data.frame(nnk)
  k<-lm(nnk[,1]~ nnk[,2], nnk)
  kk<-c(k$coefficients[2])
  }
}

for (i in 10) {
  n<-sample(1:nrow(a),nrow(a),replace = T)
  nnk<-a[n,]
  nnk<-as.data.frame(nnk)
  k<-lm(nnk[,1]~ nnk[,2], nnk)
  kk[i]<-c(k$coefficients[2])
}



boot1<-function(x,y,d){
n<-sample(1:nrow(d),nrow(d),replace = T)
nnk<-d[n,]
nnk<-as.data.frame(nnk)
k<-lm(nnk[,y]~ nnk[,x], nnk)
s<-summary(k)
coe<-c(s$coefficients[2,1])
print(coe)
}

boot1(1,2,a)

boot2<-function(kn){
  k<-1
  repeat{
    boot1(1,2,a)[k]
    k<-k+1
    if(k>=kn) break
  }
}
boot2(10)

k<-1
l<-c(100:200)
repeat{
  l[k]<-k
  k<-k+1
  if(k>99) break
}
