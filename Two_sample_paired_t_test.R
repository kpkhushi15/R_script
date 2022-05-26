##  Two sample t test for dependent samples (paired t test)

two_sample_paired_t_test=function(x,y,alpha,alternate){
n=length(x)
d=x-y
dbar=mean(x-y)
s=(d-dbar)^2
sd=sqrt(sum(s)/(n-1))
t=dbar/(sd/sqrt(n))
t=round(t,3)
print("Paired t test : ")
print(paste("sample size of X and Y :  ", n))
t_critical=0
## Testing Null Hypothesis by comparing estimated t_statistics with t_critical

if (alternate=='Not_equal_to'){
  t_critical=qt(1-(alpha/2),n-1)
  t_critical=round(t_critical,3)
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
  t_critical=qt(1-alpha,n-1)
  t_critical=round(t_critical,3)
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
  t_critical=qt(alpha,n-1)
  t_critical=round(t_critical,3)
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
  p_value=1-pt(t,n-1)
  p_value=round(p_value,3)
}else{
  p_value=pt(t,n-1)
  p_value=round(p_value,3)
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
  CI_upper=dbar+ qt(1-alpha/2,n-1)*(sd/sqrt(n))
  CI_lower=dbar- qt(1-alpha/2,n-1)*(sd/sqrt(n))
  if ((CI_lower<0) & (CI_upper>0)){
    print(paste('Confidence interval : CI_lower = ', CI_lower, ' CI_upper = ', CI_upper))
    print("This confidence Interval contains the null hypothesis value 0 for the difference in mean of X and Y, we accept the null hypothesis that the two population mean are equal")
  }else{
    print(paste('Confidence interval :  CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("This confidence Interval does not  contains the null hypothesis value 0 for the difference in mean of X and Y, we reject the null hypothesis that the two population mean are equal")
    
  }
}
if ((alternate=="Greater_than") | (alternate=="Less_than")){
  CI_upper=dbar + qt(1-alpha,n-1)*(sd/sqrt(n))
  CI_lower=dbar- qt(1-alpha,n-1)*(sd/sqrt(n))
  if (((CI_lower<0) & (CI_upper<0)) | ((CI_lower>0) & (CI_upper>0)) ){
    print(paste('Confidence interval :  CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("Since this confidence interval does not contains  0 so we reject the null hypothesis")
    
  }else{
    print(paste('Confidence interval : CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("Since this confidence interval contains 0 , We failed to reject the Null Hypothesis")
  }
}
}

x=c(24.97,24.76,23.80,22.84,24.07,22.93,23.41,22.10,23.08, 23.59,22.38,22.91,22.08,22.46,21.34,22.76,21.82,21.80,22.26,21.36,22.98,23.08)

y=c(23.98,23.63,23.61,23.58,23.57,22.83,22.75,22.74,22.70,22.62,22.39,22.34,21.95,21.73,21.68,21.66,21.68,21.58,21.57,21.35,23.17,22.53)
alpha=0.05
alternate="Greater_than"
two_sample_paired_t_test(x,y,alpha,alternate)
