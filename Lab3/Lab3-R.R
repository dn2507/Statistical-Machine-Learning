library(ISLR)
data("Weekly")
fix(Weekly)
cor(Weekly[,-9])
attach(Weekly)
pairs(Weekly)
plot(Volume)
# From the plot we see that there is a high correaltion between Volume and Year.There is an exponential relationship where volume increases exponentially as a function of year. There seems to be little correlation between lag and other lags.
fit.log<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family=binomial, data = Weekly)
summary(fit.log)
contrasts(Weekly$Direction)
# We see that Lag2 is statistically significant compared to other predictors

conf<-predict(fit.log,type = "response")
pred.glm<-rep("Down",length(conf))
pred.glm[conf>0.5]<-"Up"
table(pred.glm, Direction)

# From the confusion marix we see that precentage of the corect predictions on the training data is 56.105 which means that the training error rate 43.89%. For weeks when the market goes up the model is right 92.06% times whereas when the market goes down model is right only 11.15% of the time .
train<-(Year<2009)
weekly.0910<- Weekly[!train,]
direction.0910<-Direction[!train]
fit.log1<-glm(Direction~Lag2,data = Weekly, family = binomial,subset = train)
summary(fit.log1)

conf1<-predict(fit.log1,weekly.0910,type = "response")
pred.glm1<-rep("Down",length(conf1))
pred.glm1[conf1>0.5]<-"Up"
table(pred.glm1,direction.0910)
# From the confusion matrix we see that percentage of correct predictions on test data is 62.5% and the test error rate is 37.5%. When the market goes up the model is 91.80% right , whereas weeks when the market foes down the model prediction is 20.93% right.
library(MASS)
fit.lda<-lda(Direction~Lag2,data = Weekly, subset = train)
fit.lda
pred.lda<-predict(fit.lda, weekly.0910)
table(pred.lda$class,direction.0910)
# From the confusion matrix we see that percentage of correct predictions on the test data is 62.5%, and test error rate is 37.5%. When the market goes up the model is right 91.80 times  , but for weeks when the market goes down the model is right only 20.9% times. the result are almost similar to Logistic regression model.
fit.qda<-qda(Direction~Lag2,data = Weekly, subset = train)
fit.qda
pred.qda<-predict(fit.qda, weekly.0910)
table(pred.qda$class,direction.0910)
# From the results of the confusion matrix we can see that the correct predictions on the test dat is 58.65% and error rate is 41.34%. When weeks the market goes up the model is right 100% of the time and when the market goes down the model is right 0% of the time. We also note that the QDA achieves correctness 58.62% of the times.
library(class)
train.x<-as.matrix(Lag2[train])
test.x<-as.matrix(Lag2[!train])
train.Direction<-Direction[train]
set.seed(1)
pred.knn<-knn(train.x,test.x,train.Direction,k=1)
table(pred.knn,direction.0910)
# From the confusion matrix we conclude that the precentage of correct prediction on the test data is 50%, an test error rate is 50%. The model is right 50.81% of the time when the market goes up, and 48.83% right when the market goes down.

# From comparision of test error rates of all the models we see that logistic regression and LDA have minimum error rate as compared to QDA and KNN.
#Logistic regression using interaction
fit.log2<-glm(Direction~Lag2:Lag1,data = Weekly, family = binomial,subset = train)
summary(fit.log2)

conf2<-predict(fit.log2,weekly.0910,type = "response")
pred.glm2<-rep("Down",length(conf2))
pred.glm2[conf2>0.5]<-"Up"
table(pred.glm2,direction.0910)
mean(pred.glm2==direction.0910)

#LDA using interections 
fit.lda1<-lda(Direction~Lag2:Lag1,data = Weekly, subset = train)
fit.lda1
pred.lda1<-predict(fit.lda1, weekly.0910)
table(pred.lda1$class,direction.0910)
mean(pred.lda1$class==direction.0910)

#QDA using transformation

fit.qda1<-qda(Direction~Lag2+sqrt(abs(Lag2)),data = Weekly, subset = train)
fit.qda1
pred.qda1<-predict(fit.qda1, weekly.0910)
table(pred.qda1$class,direction.0910)
mean(pred.qda1$class==direction.0910)
#KNN with different values 
pred.knn1<-knn(train.x,test.x,train.Direction,k=10)
table(pred.knn1,direction.0910)
mean(pred.knn1==direction.0910)

pred.knn2<-knn(train.x,test.x,train.Direction,k=100)
table(pred.knn2,direction.0910)
mean(pred.knn2==direction.0910)

# Of all the model used on this dataset Logistic regression and LDA has better performances in terms of test error rates
