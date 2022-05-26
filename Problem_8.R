data = read.csv('Prob_8_data.csv', header = TRUE)
data
res = aov(value~Gasolene_Type + Car_Type, data)
summary(res)
