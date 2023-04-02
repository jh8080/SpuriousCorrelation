
rm(list=ls(all=TRUE))
setwd("~/")

datamat <- read.csv("returns.csv",header=T) 
datamat[,1] <- as.Date(datamat[,1],format = '%d/%m/%Y')
m=nrow(datamat)

statmat={}
for (i in 3:ncol(datamat)){
  ret=log(datamat[2:m,i]/datamat[1:(m-1),i])*100
  x=datamat[2:m,2]
  
  M = lm(ret ~ x)
  b=M$coef[2]
  t=b/sqrt(vcov(M)[2,2])
  n=length(x)
  statmat=rbind(statmat,cbind(b,t,n))
  
}
tem=colnames(datamat)
plot(statmat[,2])



ret1=log(datamat[2:m,3]/datamat[1:(m-1),3])*100
x1=datamat[2:m,2]

statmat={}
for (i in 3:ncol(datamat)){
  
  M = lm(ret1 ~ x1)
  b=M$coef[2]
  t=b/sqrt(vcov(M)[2,2])
  n=length(x1)
  r2=1-sum(M$resid^2)/sum( (ret1-mean(ret1))^2)
  
  statmat=rbind(statmat,cbind(b,t,n,r2))
  
  ret=log(datamat[2:m,i]/datamat[1:(m-1),i])*100
  x=datamat[2:m,2]
  ret1=c(ret1,ret); x=datamat[2:m,2]
  x1=c(x1,x)
  
}


plot(statmat[,3],statmat[,2],type="l",lwd=2,xlab="Sample Size", ylab="t-statistic")
abline(h=0)
abline(h=1.64,col="red",lwd=2)
lines(statmat[,3],statmat[,6],col="blue",lwd=2)
abline(v=seq(0,180000,10000), col="lightgray", lty="dotted")
abline(h=seq(0,4,1), col="lightgray", lty="dotted")


