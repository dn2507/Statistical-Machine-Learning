library(ISLR)
library(tree)
set.seed(1)
train=sample(1:nrow(OJ),800)
traindata=OJ[train,]
testdata=OJ[-train,]
model.tree=tree(Purchase~.,data=traindata)
summary(model.tree)
#the given tree has 8 terminal nodes and training error is 16.5%
model.tree
# When we look at node 11 we see that pricediff >0.195 for 101 observation , with deviance of 139.20. Prediction for that branch is CH.54.45% of the observations take the value of MM and the remaining 45.5% takes the value of CH.  
plot(model.tree)
text(model.tree,pretty=0)
pred.tree <- predict(model.tree, testdata, type = "class")
table(pred.tree, testdata$Purchase)
#test error for the given prediction is 22.55%
model.tree.cv=cv.tree(model.tree,FUN=prune.misclass)
model.tree.cv
#Tree size 2 gives lowest cross-validated classification error rate.
plot(model.tree.cv$size, model.tree.cv$dev, type = "b", xlab = "Tree size", ylab = "Deviance")
prune.tree<- prune.misclass(model.tree, best = 2)
plot(prune.tree)
text(prune.tree, pretty = 0)

pred.tree.train <- predict(model.tree, traindata, type = "class")
table(pred.tree.train, traindata$Purchase)
pred.tree.train.cv <- predict(prune.tree, traindata, type = "class")
table(pred.tree.train.cv, traindata$Purchase)
# training accuarcy for un_pruned tree is 16.8 and for pruned tree is 18.25. The pruned tree has slightly high error rate comapred to the unpruned one.
table(pred.tree, testdata$Purchase)
pred.tree.test.cv <- predict(prune.tree, testdata, type = "class")
table(pred.tree.test.cv, testdata$Purchase)
# test accuracy for the n-pruned tree is 22.5% and test accuracy for pruned tree is 25.9%. Pruned tree has a higher test error rate compared to the unpruned tree
set.seed(1)
train1<-sample(1:nrow(Carseats),nrow(Carseats)/2)
carseats.train<-Carseats[train,]


carseats.train=na.omit(carseats.train)
carseats.test<-Carseats[-train,]
model.carseat<-tree(Sales~., data = carseats.train)
summary(model.carseat)
# Total number of nodes are 18 and only 6 predictors are used to build the tree 
plot(model.carseat)
text(model.carseat,pretty=0)
pred.carseat<-predict(model.carseat, newdata=carseats.test)
mse<-mean((pred.carseat-carseats.test$Sales)^2)
mse
#set.seed(1)

cv.carseats<-cv.tree(model.carseat)
plot(cv.carseats$size,cv.carseats$dev,type="b")
tree.min<-which.min(cv.carseats$dev)
points(tree.min,cv.carseats$dev[tree.min],col="blue",cex=2,pch=20)
# Optimal length of the tree is 7 
prune.carseats<-prune.tree(model.carseat, best=7)
plot(prune.carseats)
text(prune.carseats,pretty=0)
pre.car<-predict(prune.carseats, newdata = carseats.test)
mse.p<-mean((pre.car - carseats.test$Sales)^2)
mse.p
#Pruning of the tree increases the MSE
library(adabag)
library(randomForest)
bag.carseats<-randomForest(Sales~.,data=Carseats,subset=train1,mtry=10,importance=TRUE)
pred.carseat.bagging<-predict(bag.carseats,newdata=carseats.test)
mse.test.bagging<-mean((pred.carseat.bagging - carseats.test$Sales)^2)
mse.test.bagging
# The test MSE for bagging is 1.51
importance(bag.carseats)
# From the given table we see that Price and Shelveloc are two important attributes.

ran.carseats<-randomForest(Sales~.,data=carseats.train, mtry=10, ntry=500,importance = TRUE) 
pred.ran<-predict(ran.carseats, newdata = carseats.test)
mse.rf<-mean((pred.ran - carseats.test$Sales)^2)
mse.rf

ran.carseats1<-randomForest(Sales~., data=carseats.train, mtry=7, ntry=500,importance = TRUE)
pred.ran1<-predict(ran.carseats1, newdata = carseats.test)                       
mse.rf1<-mean((pred.ran1 - carseats.test$Sales)^2)
ran.carseats11<-randomForest(Sales~., data=carseats.train, mtry=5, ntry=500,importance = TRUE)
pred.ran11<-predict(ran.carseats11, newdata = carseats.test)                       
mse.rf11<-mean((pred.ran11 - carseats.test$Sales)^2)
mse.rf1
mse.rf11
# we see observe that as we increase the value of m , MSE decreases.
importance(ran.carseats)
