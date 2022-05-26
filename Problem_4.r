#  Hypothesis testing a population proportion

one_sample_z_test=function(y,n,p,alternate,alpha){
# calculate Z-statistics
ps=y/n
z=(ps-p)/sqrt((p*(1-p))/n)
z=round(z,2)
print("Hypothesis testing a population proportion")

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


}

y=219
n=1124
p=0.2
alternate="Not_equal_to"
alpha=0.05
one_sample_z_test(y,n,p,alternate,alpha)
