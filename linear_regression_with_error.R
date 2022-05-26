#### Linear Regression  fitted line with error

linear_regression_with_error=function(x,y,errors){
  
errors_square=errors**2

## Y_mean
y_mean=mean(y)

## Calculate the value of delta, b_zero and b_one
delta=(sum(1/errors_square)*sum((x**2)/errors_square))-(sum(x/errors_square)*sum(x/errors_square))
b_zero=((sum(y/errors_square)*sum((x**2)/errors_square))-(sum(x/errors_square)*sum(x*y/errors_square)))/delta
b_one=((sum(1/errors_square)*sum((x*y)/errors_square))-(sum(x/errors_square)*sum(y/errors_square)))/delta
print(paste("b_zero : ",b_zero))
print(paste("b_one : ",b_one))


# Expression for the fitted line
y_expected=b_zero+(b_one*x)

## Compute R_square
y_mean=mean(y)
r_square=(sum((y_expected-y_mean)**2))/(sum((y-y_mean)**2))
print(paste("R_square : ", r_square))


## Fitted value
for (i in x){
  print(paste("fitted value for the fixed value of x",x[i]," : ",round(y_expected[i],2)))
}

#### plot the results
plot(x,y,col="red")
lines(x,y_expected,col="blue")
arrows(x,y-errors,x,y+errors,length=0.05,angle=90,code=3)

# Residual plot
residual_value=(y-y_expected)
plot(y_expected,residual_value)

}

x = c(1,2,3,4,5,6,7,8,9,10)
y= c(16.5, 23.3, 35.5, 45.8, 57.9, 68.0, 73.4, 89.0, 97.9, 107.0)
errors = c(3.0, 4.1, 5.0, 5.5, 6.0, 6.2, 1.0, 1.0, 1.0, 1.0)
linear_regression_with_error(x,y,errors)
