#  Hypothesis testing : Single population mean when variance is known

one_sample_z_test=function(x,mu,sigma,alternate,alpha){
# calculate Z-statistics
xbar=mean(x)
n=length(x)
z=(xbar-mu)/(sigma/sqrt(n))
z=round(z,2)

z_critical=0

#HYpothesis testing using rejection region 

if (alternate=="Not_equal_to"){
  z_critical=qnorm(1-alpha/2) 
  z_critical=round(z_critical,2)
  if ((z<(-z_critical)) | (z>z_critical)){
    print(paste("Z_critical : ",z_critical,"  Z_statistics : ", z))
    print(paste("The two sided null hypothesis is rejected to a significance level of :", alpha)) 
  }else{
    print(paste("Z_critical : ",z_critical,"  Z_statistics : ", z))
    print(paste("We failed to reject the null hypothesis at significance level of :", alpha))
      
    }
}

if (alternate=="Greater_than"){
  z_critical=qnorm(1-alpha)
  z_critical=round(z_critical,2)
  if (z>z_critical){
    print(paste("Z_critical : ",z_critical,"  Z_statixtics : ", z))
    print(paste("we reject the null hypothesis at significance level of : ", alpha))
  }else{
    print(paste("Z_critical : ",z_critical,"  Z_statistics : ", z))
    print(paste("we  failed to reject the null hypothesis at significance level of : ", alpha))
    }
}

if (alternate=="Less_than"){
  z_critical=qnorm(alpha)
  if (z<z_critical){
    print(paste("Z_critical : ",z_critical,"  Z_statistics : ", z))
    print(paste("we reject the null hypothesis at significance level of : ", alpha))
  }else{
    print(paste("Z_critical : ",z_critical,"  Z_statistics : ", z))
    print(paste("we  failed to reject the null hypothesis at significance level of : ", alpha))
  }
  }


# Hypothesis testing by computing p_value
if (z>0){
  p_value=1-pnorm(z)
  p_value=round(p_value,3)
}else{
  p_value=pnorm(z)
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
  CI_upper=xbar+qnorm(1-alpha/2)*(sigma/sqrt(n))
  CI_upper=round(CI_upper,3)
  CI_lower=xbar-qnorm(1-alpha/2)*(sigma/sqrt(n))
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
  CI_upper=xbar+qnorm(1-alpha)*(sigma/sqrt(n))
  CI_upper=round(CI_upper,3)
  CI_lower=xbar-qnorm(1-alpha)*(sigma/sqrt(n))
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


x=c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9, 137.4, 145.6, 135.6, 135.4, 121.5)
sigma=14.5 
mu=124.6
alternate="Not_equal_to"
alpha=0.05
one_sample_z_test(x,mu,sigma,alternate,alpha)
