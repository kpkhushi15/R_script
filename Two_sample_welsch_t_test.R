##Two sample t test for independent samples of unknown and unequal population variances - 
#The Welsch t test
two_sample_welsch_t_test=function(x,y,alpha,alternate){
  

xbar=mean(x)
ybar=mean(y)
n=length(x)
m=length(y)
Sx=var(x)
Sy=var(y)
r=((Sx/n+Sy/m)^2)/(((1/(n-1))*((Sx/n)^2))+((1/(m-1))*((Sy/m)^2)))
r=round(r)
print("Two sample t-test with Unknown and Unequal popualtion variance: ")
print(paste("sample size of X :  ", n,"  sample size of Y : ",m))
t=(xbar-ybar)/sqrt((Sx/n)+(Sy/m))
t_critical=0


## Testing Null Hypothesis by comparing estimated t_statistics with t_critical

if (alternate=='Not_equal_to'){
  t_critical=qt(1-(alpha/2),r)
  if((t<(-t_critical)) | (t>z_critical)){
    print(paste('t = ',t,'  t_critical = ', t_critical))
    print("Estimated t is out of given range ")
    print(paste("Null Hypothesis Rejected at significance level of : ",alpha/2))
    
  }else{
    print(paste('t = ',t,'  t_critical = ', t_critical))
    print("Estimated t is within the given range ")
    print(paste("We failed to reject the Null Hypothesis at significance level of : ", alpha/2))
    
    
  }
}

if (alternate=='Greater_than'){
  t_critical=qt(1-alpha,r)
  if(t>t_critical){
    print(paste('t = ',t,'  t_critical = ', t_critical))
    print("Estimated t is out of the given range  ")
    print(paste("Null Hypothesis Rejected at significance level of : ",alpha))
    
    
  }else{
    print(paste('t= ',t, "  t_critical=  ",t_critical))
    print("Estimated t is within the given range ")
    print(paste("We failed to reject the Null hypothesis at significance level of : ", alpha))
    
    
  }
}

if (alternate=='Less_than'){
  t_critical=qt(alpha,r)
  if(t<t_critical){
    print(paste("Null Hypothesis rejected at significance level of : ",alpha))
    print("Estimated t is out of the given range : ")
    print(paste('t= ',t, "  t_critical=  ",t_critical))
    
  }else{
    print(paste("We failed to reject the Null Hypothesis at significance level of :  ",alpha))
    print("Estimated t is within the given range : ")
    print(paste('t = ',t,' t_critical =', t_critical))
  }
}

### Testing Null Hypothesis by computing p-value 
## compute the p_value
if (t>0){
  p_value=1-pt(t,r)
}else{
  p_value=pt(t,r)
}
## Null Hypothesis says that population mean of both X and Y are equal
if (alternate=='Not_equal_to'){
  if (p_value<alpha/2){
    print(paste('p_value of the test = ',p_value,'  and  signficance level  = ',alpha/2))
    print("p_value is less than the significance level  ")
    print('Null Hypothesis rejected ')
  }else{
    print(paste('p_value of the test = ',p_value,'  significance level = ',alpha/2))
    print("p_value of the test is greater than the significance level ")
    print("We failed to reject the Null Hypothesis")
    
  }
}

if((alternate=='Greater_than') | (alternate=='Less_than')){
  if(p_value<alpha){
    print(paste('p_value of the test = ',p_value,'  significance level = ',alpha))
    print("p_value is less than the significance level ")
    print('Null Hypothesis rejected')
    
  }else{
    print(paste('p_value of the test = ',p_value,'  significance level= ',alpha))
    print("p_value of the test is greater than the significance level  ")
    print("We failed to reject the Null Hypothesis")
    
  }
}
## Testing Null hypothesis by computing confidence Interval
if (alternate=='Not_equal_to'){
  CI_upper=(xbar-ybar)+ qt(1-alpha/2,r)*sqrt((Sx/n)+(Sy/m))
  CI_lower=(xbar-ybar)- qt(1-alpha/2,r)*sqrt((Sx/n)+(Sy/m))
  if ((CI_lower<0) & (CI_upper>0)){
    print(paste('Confidence interval : CI_lower = ', CI_lower, ' CI_upper = ', CI_upper))
    print("This confidence Interval contains the null hypothesis value 0 for the difference in mean of X and Y, we accept the null hypothesis that the two population mean are equal")
  }else{
    print(paste('Confidence interval :  CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("This confidence Interval does not  contains the null hypothesis value 0 for the difference in mean of sample X and Y, we reject the null hypothesis that the two population mean are equal")
    
  }
}
if ((alternate=="Greater_than") | (alternate=="Less_than")){
  CI_upper=(xbar-ybar)+ qt(1-alpha,r)*sqrt((Sx/n)+(Sy/m))
  CI_lower=(xbar-ybar)- qt(1-alpha,r)*sqrt((Sx/n)+(Sy/m))
  if (((CI_lower<0) & (CI_upper<0)) | ((CI_lower>0) & (CI_upper>0)) ){
    print(paste('Confidence interval :  CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("Since this confidence interval does not contains  0 so we reject the null hypothesis")
    
  }else{
    print(paste('Confidence interval : CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("Since this confidence interval contains 0 , We failed to reject the Null Hypothesis")
  }
}
}

x=c(2.9, 14.9, 1.0, 12.6, 9.4, 7.6, 3.6, 3.1, 2.7, 4.8, 3.4, 7.1, 7.2)
y=c(7.8, 4.2, 2.4, 12.9, 17.3, 10.4, 5.9, 4.9, 5.1, 8.4, 10.8, 23.4, 9.7)
alpha=0.05
alternate="Less_than"
two_sample_welsch_t_test(x,y,alpha,alternate)
