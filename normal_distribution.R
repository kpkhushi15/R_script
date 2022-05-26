############################################
x=dnorm(14,12,2)
round(x,digits = 3)
dnorm(2)
mu=12
sd=2
y=seq(mu-4*sd,mu+4*sd,100)
y
mean = 12
standev = 2
x = seq(mean - 4*standev, mean + 4*standev, 100)
x
string =  substitute( paste("N( x, ", mu, " = 12,  ", sigma, " = 2 " , ")"  ))
curve(dnorm(x, mean, standev), xlim=c(mean - 4*standev, mean +  4*standev),
      ylab=string, lwd=1.5, cex.lab=1.2, col="blue", main="Gaussian distribution")
dnorm(x, mean, standev)
curve(dnorm(x, mean, standev),xlim=c(mean - 4*standev, mean +  4*standev))

#################################
# unit normal distribution
mu=0
sd=1
x=seq(mu-4*sd,mu+4*sd,100)
x
curve(dnorm(x,mu,sd),xlim=c(mu-4*sd,mu+4*sd))
#plot(dnorm(x,mu,sd),xlim=c(mu-4*sd,mu+4*sd))


pnorm(2.6)
pnorm(-2.6)
qnorm(0.004661188)
a=rnorm(100,15,3)
plot(a)
hist(a)
pnorm(2)
dnorm(2)
b=rnorm(100,12,3)
hist(b)
dnorm(19,12,3)
mu=12
sd=2
mu-4*sd
mu+4*sd
seq(mu-4*sd,mu+4*sd)
