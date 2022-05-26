## Two sample z test for the difference between two population means

#### function for two sample z-test

two_sample_z_test=function(x,y,sigma_X,sigma_Y,alpha,alternate){

xbar=mean(x)
ybar=mean(y)
n=length(x)
m=length(y)
print("Two sample z test : ")
print(paste("sample size of X :  ", n,"  sample size of Y : ",m))
z=(xbar-ybar)/sqrt((sigma_X^2/n)+(sigma_Y^2/m))
z=round(z,2)

z_critical=0


## Testing Null Hypothesis by comparing estimated z_statistics with z_critical

if (alternate=='Not_equal_to'){
  z_critical=qnorm(1-(alpha/2))
  z_critical=round(z_critical,2)
  if((z<(-z_critical)) | (z>z_critical)){
    print(paste('Z = ',z,'  Z_critical = ',z_critical))
    print("Since this is two sided test and z is out of the range of z_critical")
    print(paste("Null Hypothesis Rejected at significance level of : ",alpha/2))
    
  }else{
    print(paste('Z = ',z,'  Z_critical = ', z_critical))
    print("Z is within the given range of z_critical ")
    print(paste("We failed to reject the Null Hypothesis at significance level of : ", alpha/2))
    
    
    
  }
}

if (alternate=='Greater_than'){
  z_critical=qnorm(1-alpha)
  z_critical=round(z_critical,2)
  if(z>z_critical){
    print(paste('Z = ',z,'  Z_critical = ', z_critical))
    print("Z is greater than the Z_critical ")
    print(paste("Null Hypothesis Rejected at significance level of : ",alpha))
    
    
  }else{
    print(paste('Z= ',z, "  Z_critical=  ",z_critical))
    print("Z is less than the Z_critical ")
    print(paste("We failed to reject the Null hypothesis at significance level of : ", alpha))

    
  }
}

if (alternate=='Less_than'){
  z_critical=qnorm(alpha)
  z_critical=round(z_critical,2)
  if(z<z_critical){
    print(paste('Z= ',z, "  Z_critical=  ",z_critical))
    print("Z is less than the Z_critical ")
    print(paste("Null Hypothesis rejected at significance level of : ",alpha))
    
    
    
  }else{
    print(paste('Z = ',z,' Z_critical =', z_critical))
    print("Z is greater than the Z_critical ")
    print(paste("We failed to reject the Null Hypothesis at significance level of :  ",alpha))
    
  }
}

### Testing Null Hypothesis by computing p-value 
## compute the p_value
 if (z>0){
   p_value=1-pnorm(z)
   p_value=round(p_value,4)
 }else{
  p_value=pnorm(z)
  p_value=round(p_value,4)
 }

## Null Hypothesis says that population mean of both X and Y are equal
if (alternate=='Not_equal_to'){
  if (p_value<alpha/2){
    print(paste('p_value of the test = ',p_value,'  and  signficance level  = ',alpha/2))
    print("p_value is less than the given significance level  ")
    print('Null Hypothesis rejected ')
    
  }else{
    print(paste('p_value of the test = ',p_value,'  sifnificance level = ',alpha/2))
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

## comupte confidence interval

if (alternate=='Not_equal_to'){
  CI_upper=(xbar-ybar)+ qnorm(1-alpha/2)*sqrt((sigma_X^2/n)+(sigma_Y^2/m))
  CI_upper=round(CI_upper,3)
  CI_lower=(xbar-ybar)- qnorm(1-alpha/2)*sqrt((sigma_X^2/n)+(sigma_Y^2/m))
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
  CI_upper=(xbar-ybar)+ qnorm(1-alpha)*sqrt((sigma_X^2/n)+(sigma_Y^2/m))
  CI_upper=round(CI_upper,3)
  CI_lower=(xbar-ybar)- qnorm(1-alpha)*sqrt((sigma_X^2/n)+(sigma_Y^2/m))
  CI_lower=round(CI_lower,3)
  if (((CI_lower<0) & (CI_upper<0)) | ((CI_lower>0) & (CI_upper>0)) ){
    print(paste('Confidence interval :  CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("Since this confidence interval does not contains  0 so we reject the null hypothesis")
    
  }else{
    print(paste('Confidence interval : CI_lower = ', CI_lower, '  CI_upper = ', CI_upper))
    print("We failed to reject the Null Hypothesis")
  }
}




}


x=c( 258.0, 271.5, 189.1, 216.5, 237.2, 222.0, 231.3, 181.7, 220.0, 179.3, 238.1, 217.7,
     246.2, 241.5, 233.8, 222.3, 199.2, 167.9, 216.2, 240.4, 235.3, 187.0, 233.7, 214.7,
     174.6, 246.3, 185.7, 207.0, 244.3, 237.7, 245.2, 228.3, 201.8, 218.3, 242.7, 213.8,
     231.9, 257.3, 208.4, 250.7, 198.3, 206.7, 259.7, 253.3, 200.3, 196.6, 210.6, 257.6,
     173.5, 267.5, 167.2, 227.1, 172.1, 197.6, 256.9, 203.7, 195.1, 237.4, 210.2, 208.8,
     218.0, 205.1, 241.1, 216.8, 223.6, 191.0, 225.9, 215.1, 233.1, 243.0)

y=c( 221.0, 213.0, 199.3, 211.2, 225.2, 229.1, 253.9, 194.6, 243.0, 221.9, 230.9, 221.1,
     206.7, 217.2, 215.8, 203.0, 234.0, 196.3, 235.8, 234.3, 244.7, 248.8, 200.5, 232.0,
     233.3, 220.6, 289.2, 244.9, 230.8, 182.9, 199.3, 263.2, 220.6, 266.7, 258.0, 243.9,
     178.1, 200.7, 270.2, 224.4, 222.4, 234.6, 296.7, 202.3, 277.9, 204.3, 221.1, 257.0,
     243.4, 239.4, 230.0, 263.5, 241.3, 216.6, 227.9, 230.1, 230.5, 188.6, 289.3, 234.4,
     267.5, 256.0, 246.5, 210.5, 270.6, 295.5, 195.8, 235.3, 245.4, 245.4)

sigma_X=24.6
sigma_Y=27.8
alpha=0.05
alternate='Less_than'
two_sample_z_test(x,y,sigma_X,sigma_Y,alpha,alternate)
