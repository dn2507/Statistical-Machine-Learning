library(ISLR)
College<-read.csv("/Users/Dhan/Downloads/Lab4/College.csv")
#rm(list = ls())
#fix(College)
set.seed(11)
train=sample(1:dim(College)[1],dim(College)[1]/2)
test=(-train)
College.train<-College[train,][,-1]
College.test<-College[test,][,-1]
fit.lm<-lm(Apps~.,data = College.train)
pred.lm<-predict(fit.lm,College.test)
mean((pred.lm - College.test$Apps)^2)
#test MSE for a linear model is 1538442 i.e 1.538442^6


#ridge regression
library(glmnet)
train.rid<-model.matrix(Apps~.,data = College.train)
test.rid<-model.matrix(Apps~.,data=College.test)
grid<-10^seq(4,-2,length=100)
fit.rid<-glmnet(train.rid,College.train$Apps,alpha = 0,lambda = grid)
#plot(fit.rid)
cv.ridge<-cv.glmnet(train.rid,College.train$Apps,alpha=0,lambda = grid)
plot(cv.ridge)
best.rid<-cv.ridge$lambda.min
best.rid
pred.ridge<-predict(fit.rid, s=best.rid,newx = test.rid)
mean((pred.ridge - College.test$Apps)^2)
# the test MSE is higher for ridge regression than for least sqaures from the value we obatined

fit.lasso<-glmnet(train.rid, College.train$Apps,alpha=1,lambda = grid)
#plot(fit.lasso)
cv.lasso<-cv.glmnet(train.rid,College.train$Apps,alpha=1,lambda = grid)
#plot(cv.lasso)
best.las<-cv.lasso$lambda.min
best.las
pre.lasso<-predict(fit.lasso,s=best.las,newx = test.rid)
mean((pre.lasso - College.test$Apps)^2)
# the test MSE for the given Lasso is higher than ridge regression

predict(fit.lasso,s=best.las,type="coefficients")
test.avg<-mean(College.test$Apps)
lm.r2<-1- mean((pred.lm - College.test$Apps)^2)/mean((test.avg - College.test$Apps)^2)
ridge.r2<-1-mean((pred.ridge - College.test$Apps)^2)/mean((test.avg - College.test$Apps)^2)
lasso.r2<-1-mean((pre.lasso - College.test$Apps)^2)/mean((test.avg - College.test$Apps)^2)
lm.r2
ridge.r2
lasso.r2


#part 2

library(MASS)
library(leaps)
data(Boston)
set.seed(1)
predict.regsubsets<-function(object,newdata,id,...){
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form,newdata)
  coefi<-coef(object,id=id)
  xvars<-names(coefi)
  
  mat[,xvars] %*% coefi
}

k=10
folds<-sample(1:k,nrow(Boston),replace = TRUE)
cv.errors<-matrix(NA,k,13,dimnames = list(NULL,paste(1:13)))
for (j in 1:k) {
  
  best.fit<-regsubsets(crim~., data=Boston[folds!=j, ],nvmax=13)
  
  for (i in 1:13) {
    
    pred<-predict(best.fit, Boston[folds==j,],id=i)
    cv.errors[j,i]<-mean((Boston$crim[folds==j]-pred)^2)
  }
  
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
plot(mean.cv.errors, type = "b", xlab = "Number of variables", ylab = "CV error")
# from the above figure we see that cross-validation selects 12 variable modelwith MSE of 41.03

predictors<-model.matrix(crim~.,Boston)[,-1]
response<-Boston$crim
#lasso

cv.out<-cv.glmnet(predictors,response,alpha=1,type.measure = "mse")
best.lasso=cv.out$lambda.min
best.lasso
plot(cv.out)
lass.model<-glmnet(predictors,response,alpha=1,lambda = grid)
lasso.pred<-predict(lass.model,s=best.lasso,newx = predictors)
error<-mean((lasso.pred - response)^2)
error
predict(lass.model,type="coefficients",s=best.lasso)
# fromt the cross validation which selects a lambda of 0.046 we have test MSE of 40.43
#the model is using only 4 variables to build the model
#ridge regression

cv.out<-cv.glmnet(predictors,response,alpha=0,type.measure = "mse")
best.rid=cv.out$lambda.min
best.rid
plot(cv.out)
rid.model<-glmnet(predictors,response,alpha=0,lambda = grid)
rid.pred<-predict(rid.model,s=best.rid,newx = predictors)
error1<-mean((rid.pred - response)^2)
error1
predict(rid.model,type="coefficients",s=best.rid)
# the Cross-validation estimate for test MSE is 40.77 and the lambda is 0.58

# from the cross-validation iteration we see that lasso performs better compared to all the other models with MSE 40.43.

#No. The model chosen has 12 predictors. not all the predictors are considered


