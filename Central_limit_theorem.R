######################################
x=runif(10000,0,10)
x
hist(x)
n=50
mu = 5
Z=c()
sigma=sqrt(25/3)
for (i in 1:10000){
  y=runif(50,0,10)
  z=(mean(y)-mu)/(sigma/sqrt(50))
  Z=c(Z,z)
}
print(Z)
h=hist(Z)
xfit<-seq(min(Z),max(Z),length=60) 
yfit<-dnorm(xfit,mean=mean(Z),sd=sd(Z)) 
yfit <- yfit*diff(h$mids[1:2])*length(Z) 
lines(xfit, yfit, col="red", lwd=1)
xfit
h
