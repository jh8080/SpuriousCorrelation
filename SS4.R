
rm(list=ls(all=TRUE))
setwd("d:/pk/weather/sunspot")

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
  
  
  k1=1; k0=1; f=t^2
  tem3=pi^(0.5)/gamma((k1+1)/2)
  v1=n-k0-k1-1
  tem1=(v1/2)^(k1/2)
  tem2=(1+(k1/v1)*f)^(0.5*(v1-1))
  podds=(tem3*tem1/tem2)^(-1)
  podds=2*log(podds)
  
  
  alpha=0.05
  q=1
  tem1= ( qchisq(p=1-alpha,df=q) + q*log(n) )^(0.5*q-1)
  tem2=2^(0.5*q-1) * n^(0.5*q) * gamma(0.5*q)
  tem3 = exp(-0.5*qchisq(p=1-alpha,df=1))
  alphas = -qnorm(tem1*tem3/tem2)
  
  statmat=rbind(statmat,cbind(b,t,n,r2,podds,alphas))
  
  ret=log(datamat[2:m,i]/datamat[1:(m-1),i])*100
  x=datamat[2:m,2]
  ret1=c(ret1,ret); x=datamat[2:m,2]
  x1=c(x1,x)
  
}


#library(latex2exp)
plot(statmat[,3],statmat[,1],xlab="Sample Size",ylab="Slope Coefficient and R-Square",lwd=2,type="l")
abline(v=seq(0,180000,10000), col="lightgray", lty="dotted")
abline(h=seq(-0.0001,0.0005,0.0001), col="lightgray", lty="dotted")
abline(h=0)
lines(statmat[,3],statmat[,4],col="red",lwd=2)
#legend(130000,0.0004,pch=c(16,16), col=c("black","red"),legend=c(TeX("$b_{1}$"),TeX("$R^{2}$")), text.col=c("black","red"),cex=0.9 )




plot(statmat[,3],statmat[,2],type="l",lwd=2,xlab="Sample Size", ylab="t-statistic")
abline(h=0)
abline(h=1.64,col="red",lwd=2)
lines(statmat[,3],statmat[,6],col="blue",lwd=2)
abline(v=seq(0,180000,10000), col="lightgray", lty="dotted")
abline(h=seq(0,4,1), col="lightgray", lty="dotted")



plot(statmat[,3],statmat[,5],type="l",lwd=2,ylab="2log(Bayes Factor)",xlab="Sample Size")
abline(h=2,col="red",lwd=2)
abline(v=seq(0,180000,10000), col="lightgray", lty="dotted")
abline(h=seq(-10,5,5), col="lightgray", lty="dotted")

