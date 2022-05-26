###########################################
rbinom(20,10,0.4)
dbinom(5,10,0.4)
dbinom(4,10,0.4)
pbinom(5,10,0.4)
pbinom(4,10,0.4)
qbinom(0.6331033,10,0.4)
qbinom(0.8337614,10,0.4)
n=10
p=0.4
x=seq(0,10)
pdense=dbinom(x,10,0.4)
pdense
### binomial density distribution
plot(x,pdense,type='h',col='red',xlab='Binomial distrubution',ylab='frequency',main = 'Frequency distribution of binomial random deviates')


## frequency histogram
n=10
p=0.4
x=rbinom(1000,10,0.4)
plot(table(x),type='o',xlab='binomial random variate',ylab='frequency',main='frequency distribution')
