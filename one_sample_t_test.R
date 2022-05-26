## Hypothesis testing- One sample t-test


one_sample_t_test=function(x,mu,alternate,alpha){
  # calculate Z-statistics
  xbar=mean(x)
  sigma=sd(x)
  n=length(x)
  t=(xbar-mu)/(sigma/sqrt(n))
  t=round(t,2)
  
  t_critical=0
  
  #HYpothesis testing using rejection region
  if (alternate=="Not_equal_to"){
    t_critical=qt(1-alpha/2,n-1) 
    t_critical=round(t_critical,2)
    if ((t<(-t_critical)) | (t>t_critical)){
      print(paste("t_critical : ",t_critical,"  t_statistics : ", t))
      print(paste("The two sided null hypothesis is rejected to a significance level of :", alpha)) 
    }else{
      print(paste("t_critical : ",t_critical,"  t_statistics : ", t))
      print(paste("We failed to reject the null hypothesis at significance level of :", alpha))
      
    }
  }
  
  if (alternate=="Greater_than"){
    t_critical=qt(1-alpha,n-1)
    t_critical=round(t_critical,2)
    if (t>t_critical){
      print(paste("t_critical : ",t_critical,"  t_statixtics : ", t))
      print(paste("we reject the null hypothesis at significance level of : ", alpha))
    }else{
      print(paste("t_critical : ",t_critical,"  t_statistics : ", t))
      print(paste("we  failed to reject the null hypothesis at significance level of : ", alpha))
    }
  }
  
  if (alternate=="Less_than"){
    t_critical=qt(1-alpha,n-1)
    t_critical=round(t_critical,2)
    if (t<(-t_critical)){
      print(paste("t_critical : ",t_critical,"  t_statistics : ", t))
      print(paste("we reject the null hypothesis at significance level of : ", alpha))
    }else{
      print(paste("t_critical : ",t_critical,"  t_statistics : ", t))
      print(paste("we  failed to reject the null hypothesis at significance level of : ", alpha))
    }
  }
  
  
  # Hypothesis testing by computing p_value
  if (t>0){
    p_value=1-pt(t,n-1)
    p_value=round(p_value,3)
  }else{
    p_value=pt(t,n-1)
    p_value=round(p_value,3)
  }
  if (alternate=="Not_equal_to"){
    if (p_value<alpha/2){
      print(paste("p_value: ",p_value, " significance level :", alpha))
      print(paste("p_value of the test statistics is less than " , alpha/2, "   we reject the null hypothesis at significance level of : ", alpha))
    }else{
      print(paste("p_value: ",p_value, " significance level :", alpha))
      print(paste("p_value of the test statistics is greater than " , alpha/2, "   we failed to reject the null hypothesis at significance level of : ", alpha))
    }
  }
  
  if((alternate=="Greater_than")|(alternate=="Less_than")){
    if (p_value<alpha){
      print(paste("p_value: ",p_value, " significance level :", alpha))
      print(paste("p_value of the test statistics is less than " , alpha, "   we reject the null hypothesis at significance level of : ", alpha))
    }else{
      print(paste("p_value: ",p_value, " significance level :", alpha))
      print(paste("p_value of the test statistics is greater than " , alpha, "   we failed to reject the null hypothesis at significance level of : ", alpha))
      
    }
    
  }
  
  
  ## Hypothesis testing by computing confidence interval
  
  if (alternate=="Not_equal_to"){
    CI_upper=xbar+qt(1-alpha/2,n-1)*(sigma/sqrt(n))
    CI_upper=round(CI_upper,3)
    CI_lower=xbar-qt(1-alpha/2,n-1)*(sigma/sqrt(n))
    CI_lower=round(CI_lower,3)
    if ((mu>CI_lower) & (mu<CI_upper)){
      print(paste("CI_lower : ", CI_lower, "  CI_upper :", CI_upper))
      print(paste("Confidence interval contains the mean value : ", mu , " and hence we failed to reject the null hypothesis"))
    }else{
      print(paste("CI_lower : ", CI_lower, "  CI_upper :", CI_upper))
      print(paste("Confidence interval  does not contains the mean value : ", mu , " and hence we reject the null hypothesis"))
    }
  }
  
  if((alternate=="Greater_than")| (alternate=="Less_than")){
    CI_upper=xbar+qt(1-alpha,n-1)*(sigma/sqrt(n))
    CI_upper=round(CI_upper,3)
    CI_lower=xbar-qt(1-alpha,n-1)*(sigma/sqrt(n))
    CI_lower=round(CI_lower,3)
    if (((mu<CI_lower)&(mu<CI_upper))| ((mu>CI_lower) & (mu>CI_upper))){
      print(paste("CI_lower : ", CI_lower, "  CI_upper :", CI_upper))
      print(paste("Confidence interval  does not contains the mean value : ", mu , " and hence we reject the null hypothesis"))
    }else{
      print(paste("CI_lower : ", CI_lower, "  CI_upper :", CI_upper))
      print(paste("Confidence interval contains the mean value : ", mu , " and hence we failed to reject the null hypothesis"))
    }
  }
  
}

### Illustrate with example:

x=c(96.0, 104.0,99.1, 97.6, 99.4, 92.8,105.6,97.2,96.8,92.1,100.6,101.5,100.7,97.3, 99.6,105.9)
mu=100
alternate="Not_equal_to"
alpha=0.05
one_sample_t_test(x,mu,alternate,alpha)