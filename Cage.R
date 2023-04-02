
Cage=c(2,2,2,3,1,1,2,3,4,1,4)
Death=c(109,102,102,98,85,95,96,98,123,94,102)
plot(Cage,Death,col="red",pch=16)
M=lm(Death~Cage)
abline(M)
summary(M)