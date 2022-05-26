
################ Linear Regression- straight line fit without errors ######################

linear_regression_without_error=function(x,y){
sum_x_square=sum(x**2)
sum_xy=sum(x*y)
sum_x=sum(x)
sum_y=sum(y)

n=length(x)
print(paste("Number of data points : ", n))


delta=(n*sum_x_square)-(sum_x**2)
b_zero=((sum_y*sum_x_square)-(sum_x*sum_xy))/delta
b_one=((n*sum_xy)-(sum_x*sum_y))/delta
print(paste("b_zero : ",round(b_zero,3)))
print(paste("b_one : ",round(b_one,3)))

# Expression for the fitted line
y_expected=b_zero+b_one*x


## Compute R_square
y_mean=mean(y)
r_square=(sum((y_expected-y_mean)**2))/(sum((y-y_mean)**2))
print(paste("R_square : ", round(r_square,3)))

## Fitted value
for (i in 1:length(x)){
  print(paste("fitted value for the fixed value of x",x[i]," : ",round(y_expected[i],2)))
}


#### plot the results
plot(x,y,col="red")
lines(x,y_expected,col="blue")
}


# Define the data
x=c(0, 50, 75, 100, 150 ,200)
y=c(1.72, 2.11, 2.36 ,2.56, 3.05 ,3.42)

# call the function

linear_regression_without_error(x,y)
r_square
