
set.seed(12345)
x=cumsum(rnorm(100)) # X: random walk
y=cumsum(rnorm(100)) # Y: random walk
Plot=ts(cbind(y,x))   
plot.ts(Plot)
M=lm(y~x) 
summary(M)