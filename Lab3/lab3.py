import sklearn as sk
from patsy import dmatrices
from sklearn import metrics
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix
from sklearn.lda import LDA
from sklearn.neighbors import KNeighborsClassifier
from sklearn.qda import QDA
from matplotlib import pyplot as plt
#%matplotlib inline
import numpy as np
import pandas as pd
df=pd.read_csv('/Users/Dhan/Downloads/Lab3/Default.csv')
df.default=np.where(df.default == "Yes", 1, 0)


# cnoerting dataframe into matrices 

y,X = dmatrices('default ~ income + balance', df, return_type = "dataframe")
y = np.ravel(y)
print y

# test and train slipt of data 

X_train= X[1:8001]
X_test=X[8002:]
y_train=y[1:8001]
y_test=y[8002:]
len(X_train)

# fitting logistic regression

logit_1=LogisticRegression()
logit_1=logit_1.fit(X_train,y_train)
logit_1.score(X_train,y_train)
logitpred=logit_1.predict(X_test)
print logitpred
confusion_matrix(y_test, logitpred)
prob=logit_1.predict_proba(X_test)
print prob
print metrics.accuracy_score(y_test,logitpred)
lda1=LDA()
lda1=lda1.fit(X_train,y_train)
lda1.score(X_train,y_train)
ldapredict=lda1.predict(X_test)
print ldapredict
confusion_matrix(y_test, ldapredict)
print metrics.accuracy_score(y_test,ldapredict)


# KNN 
knn1= KNeighborsClassifier(n_neighbors=2)
knn1=knn1.fit(X_train,y_train)
knn1.score(X_train,y_train)
knnpredict=knn1.predict(X_test)
print knnpredict
confusion_matrix(y_test, knnpredict)
print metrics.accuracy_score(y_test,knnpredict)
knn2= KNeighborsClassifier(n_neighbors=10)
knn2=knn2.fit(X_train,y_train)
knn2.score(X_train,y_train)
knnpredict1=knn2.predict(X_test)
print knnpredict1
confusion_matrix(y_test, knnpredict1)
print metrics.accuracy_score(y_test,knnpredict1)


# QDA 

qda1=QDA()
qda1=qda1.fit(X_train,y_train)
qda1.score(X_train,y_train)
qdapredict=qda1.predict(X_test)
print qdapredict
confusion_matrix(y_test, qdapredict)
print metrics.accuracy_score(y_test,qdapredict)

# Strategies to improve the test accuracy 
#We can do algorithm tuning Since machine learning algorithms are driven by parameters, these parameters will influence the outcome of learning.Objective of this is to find optimum value for each parameter to improve accuracy of the model. We should check the impact of each of these parameters on the model.
# By applying ensemble methods such as bagging and boosting we can improve the accuracy of the  model 



