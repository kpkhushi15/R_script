## Two sample t-test for difference between two population means

##Two sample t test for independent samples of unknown and equal population variances

two_sample_t_test=function(x,y,alpha,alternate){
  
  xbar=mean(x)
  ybar=mean(y)
  n=length(x)
  m=length(y)
  Sx=var(x)
  Sx
  Sy=var(y)
  df=n+m-2
  Sp=sqrt((((n-1)*Sx)+((m-1)*Sy))/(n+m-2))
  Sp
  print("Two sample t-test with Unknown and equal popualtion variance: ")
  print(paste("sample size of X :  ", n,"  sample size of Y : ",m))
  t=(xbar-ybar)/(Sp*sqrt((1/n)+(1/m)))
  t=round(t,3)
  t
  t_critical=0
  
  
 ## Testing Null Hypothesis by comparing estimated t_statistics with t_critical
  
  if (alternate=='Not_equal_to'){
    t_critical=qt(1-(alpha/2),n+m-2)
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
    t_critical=qt(1-alpha,n+m-2)
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
    t_critical=qt(alpha,n+m-2)
    if(t<t_critical){
      print(paste('t= ',t, "  t_critical=  ",t_critical))
      print("Estimated t is out of the given range ")
      print(paste("Null Hypothesis rejected at significance level of : ",alpha))
      
     
      
    }else{
      print(paste('t = ',t,' t_critical =', t_critical))
      print("Estimated t is within the given range ")
      print(paste("We failed to reject the Null Hypothesis at significance level of :  ",alpha))
      
      
    }
  }
  
  ### Testing Null Hypothesis by computing p-value 
  ## compute the p_value
  if (t>0){
    p_value=1-pt(t,n+m-2)
    
  }else{
    p_value=pt(t,n+m-2)
    
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
      print(paste('Null Hypothesis rejected at significance level of ',alpha))
      
    }else{
      print(paste('p_value of the test = ',p_value,'  significance level= ',alpha))
      print("p_value of the test is greater than the significance level  ")
      print("We failed to reject the Null Hypothesis")
      
    }
  }
  ## Testing Null hypothesis by computing confidence Interval
  if (alternate=='Not_equal_to'){
    CI_upper=(xbar-ybar)+ qt(1-alpha/2,n+m-2)*Sp*sqrt((1/n)+(1/m))
    CI_upper=round(CI_upper,3)
    CI_lower=(xbar-ybar)- qt(1-alpha/2,n+m-2)*Sp*sqrt((1/n)+(1/m))
    CI_lower=round(CI_lower,3)
    if ((CI_lower<0) & (CI_upper>0)){
      print(paste('Confidence interval : CI_lower = ', CI_lower, ' CI_upper = ', CI_upper))
      print("This confidence Interval contains the null hypothesis value 0 for the difference in mean of X and Y, we accept the null hypothesis that the two population mean are equal")
    }else{
      print(paste('Confidence interval :  CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
      print("This confidence Interval does not  contains the null hypothesis value 0 for the difference in mean of X and Y, we reject the null hypothesis that the two population mean are equal")
      
    }
  }
  if ((alternate=="Greater_than") | (alternate=="Less_than")){
    CI_upper=(xbar-ybar)+ qt(1-alpha,n+m-2)*Sp*sqrt((1/n)+(1/m))
    CI_upper=round(CI_upper,3)
    CI_lower=(xbar-ybar)- qt(1-alpha,n+m-2)*Sp*sqrt((1/n)+(1/m))
    CI_lower=round(CI_lower,3)
    if (((CI_lower<0) & (CI_upper<0)) | ((CI_lower>0) & (CI_upper>0)) ){
      print(paste('Confidence interval :  CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
      print("Since this confidence interval does not contains  0 so we reject the null hypothesis")
      
    }else{
      print(paste('Confidence interval : CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
      print("Since this confidence interval contains 0 , We failed to reject the Null Hypothesis")
    }
  }
  
  
  
  
}


# Example:

x=c(1.71, 1.65, 1.68, 1.69, 1.73, 1.50, 1.60, 1.73, 1.68, 1.71, 1.65, 1.70, 1.62, 1.66, 1.66, 1.69)
y=c(1.77, 1.68, 1.76, 1.73, 1.73, 1.79, 1.74, 1.70, 1.79, 1.73, 1.71, 1.79, 1.76, 1.77, 1.74, 1.80)
alpha=0.05
alternate='Less_than'
two_sample_t_test(x,y,alpha,alternate)
