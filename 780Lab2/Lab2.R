setwd("/Users/Dhan/Documents/ISTE-780/Lab2")
Auto<-read.csv("Auto.csv")
sapply(Auto, is.factor)
#Produce a scatterplot matrix, which includes all of the variables in the data set.
plot(Auto)
names(Auto)
#Compute the matrix of correlations between the variables using the function cor() .
#You will need to exclude the name variable, which is qualitative.
cor(subset(Auto,select = -name))
#Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except name as the predictors. Use the summary() function to print the results.
lm.fit<-lm(mpg~.-name, data=Auto)
summary(lm.fit)
#i. Is there a relationship between the predictors and the response?
#Answer: We see that from the p-value and the corresponding F-statistic there is an evidence of relationship between the predictor and the response
#ii. Which predictors appear to have a statistically significant relationship to the response?
#Answer: We can check this by checking the p-value and corresponding T-statistic . We observe that all the predictors are signififcant except for cylinders , horsepower and acceleration
#iii. What does the coefficient for the year variable suggest?
#Answer: The coefficient of year variable suggests that with the average increase of 1 year is an increase of 0.75 in mpg from the model summary
#Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?
par(mfrow=c(2,2))
plot(lm.fit)
#Answer: From the residual vs Fitted values plot we observe that there is mild non-linerity in the data. The plot Standardized residuals vs Leverage  has one highest leverage point in the plot and presence of some outliers.
#Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant?
lm.fit1<-lm(mpg~.+acceleration:cylinders+acceleration*horsepower+acceleration*displacement+origin*year+weight*cylinders+displacement*horsepower+displacement*cylinders-name,data = Auto)
summary(lm.fit1)
#Answer: We see that the interaction between cylinders and weight , year and origin are statistically significant while other interactions are not. 
#Try a few different transformations of the variables, such as log(X), X0.5, X2. Comment on your findings.
par(mfrow=c(2,2))
plot(log(Auto$horsepower),Auto$mpg)
plot(sqrt(Auto$horsepower),Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)
#Answer: Log transformation has the most linear looking plot of all the transformations


# Question 2 
library(ISLR)
data("Carseats")
fix(Carseats)
attach(Carseats)
plot(Carseats)
#Fit a multiple regression model to predict Sales using Price, Urban, and US.
carseat<-lm(Sales~Price+Urban+US, data = Carseats)
summary(carseat)
#Provide an interpretation of each coefficient in the model. Be careful—some ofthe variables in the model are qualitative!
#Answer: Interpreation from the co-efficients says that with  price increase in $1 of sales there is decrease in 54.49 units in sale while other predictors are fixed. 
#        On an average the unit sales in Urban area is 21.91 units lesser than in the rurual area keeping other predictors fixed
#       On an average the sales in US territoy is 1200.73 units higher than non-US keeping other predictor fixed
attach(Carseats) 
contrasts(Urban)
contrasts(US)
#Write out the model in equation form, being careful to handle the qualitative variables properly.
#Answer:Sales=13.0434689+(−0.0544588)×Price+(−0.0219162)×Urban+(1.2005727)×US+ε where Urban is 1 for urban areas and 0 for other and US is 1 for US territory 0 for other
#For which of the predictors can you reject the null hypothesis H0: βj = 0?
#Answer: We can reject the null hypothesis for variables Price and US 
##On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is evidence of association with the outcome.
carseat1<-lm(Sales~Price+US, data = Carseats)
summary(carseat1)

#How well do the models in (a) and (e) fit the data?
# The smaller model fits better than the bigger model when we consider the r^2 values , f-statsitics and p-values. The likelihood of the model having outliers is less compared to the bigger model
par(mfrow=c(2,2))
plot(carseat1)
#Using the model from (e), obtain 95% confidence intervals for the coefficient(s).
confint(carseat1)