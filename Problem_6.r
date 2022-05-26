### Hypothesis Testing the ratio of two population variances

two_sample_variance=function(x,y,alpha,alternate){
  n=length(x)
  m=length(y)
  sigma_X= sd(x)
  sigma_Y=sd(y)
  print(paste("sample sizes are  x :  ", n,"  y : ",m))
  print(paste("Smaple variances are :", round(sigma_X^2,2), "  ", round(sigma_Y^2,2)))

## Hypothesis testing by computing F_statistics

if (alternate=="Not_equal_to"){
  VR=sigma_X^2/sigma_Y^2
  VR=round(VR,2)
  f_critical=qf(1-alpha/2,n-1,m-1)
  f_critical=round(f_critical,3)
  if (VR>=f_critical){
    print(paste("Variance Ratio  : ", VR, "   F_critical value : ", f_critical))
    print("Variance ratio is greater than or equal to F_critical value")
    print("Null hypothesis is rejected")
  }else{
    print(paste("Variance Ratio  : ", VR, "F_critical value : ", f_critical))
    print("Variance ratio is less than to F_critical value and hence we failed to reject the null hypothesis")
  }
}

if (alternate=="Greater_than"){
  VR=sigma_X^2/sigma_Y^2
  VR=round(VR,2)
  
  f_critical=qf(1-alpha,n-1,m-1)
  f_critical=round(f_critical,2)
  
  if (VR>=f_critical){
    print(paste("Variance Ratio  : ", VR, "F_critical value : ", f_critical))
    print("Variance ratio is greater than or equal to F_critical value")
    print("Null hypothesis is rejected")
  }else{
    print(paste("Variance Ratio  : ", VR, "F_critical value : ", f_critical))
    print("Variance ratio is less than to F_critical value and hence we failed to reject the null hypothesis")
  }
  
}

if (alternate=="Less_than"){
  VR=sigma_Y^2/sigma_X^2
  VR=rounf(VR,2)
  
  f_critical=qf(1-alpha,m-1,n-1)
  f_critical=round(f_critical,2)
  
  if (VR>=f_critical){
    print(paste("Variance Ratio  : ", VR, "F_critical value : ", f_critical))
    print("Variance ratio is greater than or equal to F_critical value")
    print("Null hypothesis is rejected")
  }else{
    print(paste("Variance Ratio  : ", VR, "F_critical value : ", f_critical))
    print("Variance ratio is less than to F_critical value and hence we failed to reject the null hypothesis")
  }
  
}

## Hypothesis testing by computing Confidence interval
if (alternate=="Not_equal_to"){
  CI_upper=(sigma_X^2/sigma_Y^2)/qf(alpha/2,n-1,m-1)
  CI_upper=round(CI_upper,3)
  CI_lower=(sigma_X^2/sigma_Y^2)/qf(1-alpha/2,n-1,m-1)
  CI_lower=round(CI_lower,3)
  if((CI_lower<1) & (CI_upper>1)){
    print(paste("Confidence Interval for the population variances are  CI_lower :  ", CI_lower,"  CI_upper:  ", CI_upper))
    print(paste("Since the interval ",CI_lower, CI_upper," includes 1, we are able to conclude that the two population variances may be equal"))
  }else{
    print(paste("Confidence Interval for the population variances are  CI_lower :  ", CI_lower,"  CI_upper:  ", CI_upper))
    print(paste("Since the interval ",CI_lower, CI_upper,"  does not includes 1, we are able to conclude that the two population variances are not equal"))
    
  }
}

if (alternate=="Greater_than"){
  CI_upper=(sigma_X^2/sigma_Y^2)/qf(alpha,n-1,m-1)
  CI_upper=round(CI_upper,3)
  CI_lower=(sigma_X^2/sigma_Y^2)/qf(1-alpha,n-1,m-1)
  CI_lower=round(CI_lower,3)
  if((CI_lower<1) & (CI_upper>1)){
    print(paste("Confidence Interval for the population variances are  CI_lower :  ", CI_lower,"  CI_upper:  ", CI_upper))
    print(paste("Since the interval ",CI_lower, CI_upper," includes 1, we are able to conclude that the two population variances may be equal"))
  }else{
    print(paste("Confidence Interval for the population variances are  CI_lower :  ", CI_lower,"  CI_upper:  ", CI_upper))
    print(paste("Since the interval ",CI_lower, CI_upper,"  does not includes 1, we are able to conclude that the two population variances are not equal"))
    
  }
}

if (alternate=="Less_than"){
  CI_upper=(sigma_Y^2/sigma_X^2)/qf(alpha,m-1,n-1)
  CI_upper=round(CI_upper,3)
  CI_lower=(sigma_Y^2/sigma_X^2)/qf(1-alpha,m-1,n-1)
  CI_lower=round(CI_lower,3)
  if((CI_lower<1) & (CI_upper>1)){
    print(paste("Confidence Interval for the population variances are  CI_lower :  ", CI_lower,"  CI_upper:  ", CI_upper))
    print(paste("Since the interval ",CI_lower, CI_upper," includes 1, we are able to conclude that the two population variances may be equal"))
  }else{
    print(paste("Confidence Interval for the population variances are  CI_lower :  ", CI_lower,"  CI_upper:  ", CI_upper))
    print(paste("Since the interval ",CI_lower, CI_upper,"  does not includes 1, we are able to conclude that the two population variances are not equal"))
    
  }
}
}

# Example:
y=c(6.3, 2.0, 2.3, 0.5, 1.9, 3.2, 4.1, 4.0, 6.2, 6.1, 3.5, 1.3, 1.7, 4.5, 6.3, 6.2)

x=c(13.7, 20.6, 15.9, 28.4, 29.4, 18.4, 21.1, 3.0, 26.2, 13.0)

alternate="Not_equal_to"
alpha=0.05
two_sample_variance(x,y,alpha,alternate)
