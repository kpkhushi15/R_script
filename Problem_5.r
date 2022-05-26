#### Hypothesis testing the difference between the two sample proportion 


two_sample_proportion_test=function(n1,n2,y1,y2,alpha,alternate){
  
  ps1=y1/n1
  ps1=round(ps1,3)
  ps2=y2/n2
  ps2=round(ps2,3)
  print(paste("Alternate Hypothesis is : ",alternate))
  print(paste("Sample proportions : ",ps1, "  ", ps2))


##### Calculate z-statistics for the test

z=(ps1-ps2)/sqrt(((ps1*(1-ps1))/n1)+((ps2*(1-ps2))/n2))
z=round(z,2)
z_critical=0

### Testing the null hypothesis using rejection area
if (alternate=="Not_equal_to"){
  z_critical=qnorm(1-alpha/2)
  z_critical=round(z_critical,2)
  if ( (z<(-z_critical)) | (z>z_critical)){
    print(paste("Z_critical : ",z_critical,"  Z_statistics : ", z))
    print(paste("The computed z statistics is in rejection region, the two sided null hypothesis is rejected to a significance level of :", alpha))
  }else{
    print(paste("Z_critical : ",z_critical,"  Z_statistics : ", z))
    print(paste("Z_statistics is in the acceptance region,we failed to reject the null hypothesis at significance level of :", alpha))
    
  }
}
if (alternate=="Greater_than"){
  z_critical=qnorm(1-alpha)
  z_critical=round(z_critical,2)
  if (z>z_critical){
    print(paste("Z_critical : ",z_critical,"  Z_statixtics : ", z))
    print(paste("Since the computed z_statistics is in the rejection region, we reject the null hypothesis at significance level of : ", alpha))
  }else{
    print(paste("Z_critical : ",z_critical,"  Z_statistics : ", z))
    print(paste("Since the computed z_statistics is in the acceptance region, we failed to reject the null hypothesis at significance level of : ", alpha))
  }
}

if (alternate=="Less_than"){
  z_critical=qnorm(alpha)
  z_critical=round(z_critical,2)
  if (z<z_critical){
    print(paste("Z_critical : ",z_critical,"  Z_statixtics : ", z))
    print(paste("Since the computed z_statistics is in the rejection region, we reject the null hypothesis at significance level of : ", alpha))
  }else{
    print(paste("Z_critical : ",z_critical,"  Z_statistics : ", z))
    print(paste("Since the computed z_statistics is in the acceptance region, we failed to reject the null hypothesis at significance level of : ", alpha))
  }
}


## Testing the null hypothesis by computing the p_value for the observation
if (z>0){
  p_value=1-pnorm(z)
  p_value=round(p_value,3)
}else{
  p_value=pnorm(z)
  p_value=round(p_value,3)
}
if (alternate=="Not_equal_to"){
  if (p_value<alpha/2){
    print(paste("p_value : ",p_value, " significance level :", alpha))
    print(paste("p_value of the test statistics is less than " , alpha/2, "   we reject the null hypothesis at significance level of : ", alpha))
  }else{
    print(paste("p_value : ",p_value, " significance level :", alpha))
    print(paste("p_value of the test statistics is greater than " , alpha/2, "   we failed to reject the null hypothesis at significance level of : ", alpha))
  }
}

if((alternate=="Greater_than")|(alternate=="Less_than")){
  if (p_value<alpha){
    print(paste("p_value : ",p_value, " significance level :", alpha))
    print(paste("p_value of the test statistics is less than " , alpha, "   we reject the null hypothesis at significance level of : ", alpha))
  }else{
    print(paste("p_value : ",p_value, " significance level :", alpha))
    print(paste("p_value of the test statistics is greater than " , alpha, "   we failed to reject the null hypothesis at significance level of : ", alpha))
    
  }
  
}
## Hypothesis testing by computing confidence interval

if (alternate=="Not_equal_to"){
  CI_upper=(ps1-ps2)+qnorm(1-alpha/2)*sqrt(((ps1*(1-ps1))/n1)+((ps2*(1-ps2))/n2))
  CI_upper=round(CI_upper,3)
  CI_lower=(ps1-ps2)-qnorm(1-alpha/2)*sqrt(((ps1*(1-ps1))/n1)+((ps2*(1-ps2))/n2))
  CI_lower=round(CI_lower,3)
  if ((CI_lower<0) & (CI_upper>0)){
    print(paste('Confidence interval : CI_lower = ', CI_lower, ' CI_upper = ', CI_upper))
    print("This confidence Interval contains the null hypothesis value 0 for the difference in proportion  ps1 and ps2, we accept the null hypothesis that the two population proportions are equal")
  }else{
    print(paste('Confidence interval :  CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("This confidence Interval does not  contains the null hypothesis value 0 for the difference in proportions ps1 and ps2, we reject the null hypothesis that the two population proportions are equal")
    
  }
}

if((alternate=="Greater_than")| (alternate=="Less_than")){
  CI_upper=(ps1-ps2)+qnorm(1-alpha)*sqrt(((ps1*(1-ps1))/n1)+((ps2*(1-ps2))/n2))
  CI_upper=round(CI_upper,3)
  CI_lower=(ps1-ps2)-qnorm(1-alpha)*sqrt(((ps1*(1-ps1))/n1)+((ps2*(1-ps2))/n2))
  CI_lower=round(CI_lower,3)
  if (((CI_lower<0) & (CI_upper<0)) | ((CI_lower>0) & (CI_upper>0)) ){
    print(paste('Confidence interval :  CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("This confidence Interval does not  contains the null hypothesis value 0 for the difference in proportions ps1 and ps2, we reject the null hypothesis that the two population proportions are equal")
    
  }else{
    print(paste('Confidence interval : CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("Since the confidence interval contains 0 for the difference in proportion of ps1 and ps2, We failed to reject the Null Hypothesis")
  }
}
}


## Example:
n1=894
y1=124
n2=700
y2=70
alpha=0.05
alternate="Not_equal_to"
two_sample_proportion_test(n1,n2,y1,y2,0.05,"Not_equal_to")