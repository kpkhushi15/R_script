##### One Way ANOVA 


x1=c(13.75, 13.00 ,14.25, 12.00 ,12.15)
x2=c(13.75 ,14.50 ,12.75 ,14.50, 13.25)
x3=c(12.75 ,12.50 ,12.00, 14.00, 13.00)
x4=c(16.75, 14.25, 14.50 ,15.50, 13.75)

## convert the data to dataframe
data=data.frame(x1,x2,x3,x4)

### convert dataframe into stack format
stacked_data=stack(data)

## change the column name
names(stacked_data)=c("Length","flower_sample")
stacked_data


## ANOVA function

resp=aov(Length~flower_sample,data=stacked_data)

## summary of the anova result

print(summary(resp))

## Boxplot
boxplot(stacked_data$Length~stacked_data$flower_sample,col="blue")
