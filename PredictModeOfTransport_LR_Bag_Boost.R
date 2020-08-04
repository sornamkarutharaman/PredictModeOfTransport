
#======================================================
#     PredictModeOfTransport - Logistic Regression
#======================================================

#loading Packages
library(tidyverse)
library(modelr)
library(broom)

#Set seed to constant value
set.seed(123)      

#Fitting a logistic regression model
model_glm <- glm(Transport ~ ., data = trainLR, family='binomial') 
summary(model_glm)

predicted_glm <- predict(model_glm, testLR, type='response')
predicted_glm <- ifelse(predicted_glm > 0.5,1,0)
confusionMatrix(table(predicted_glm, testLR$Transport))

#AUC
aucLR = performance(predicted_glm , "auc")

#======================================================
#     PredictModeOfTransport - Bagging,Boosting
#======================================================

#loading required libraries
library(gbm)          
library(xgboost)      
install.packages('caret')
library(caret) 
#Bagging
library(ipred)
library(rpart)

#Set seed to constant value
set.seed(123)      


Cars_bagging<-bagging(Transport~., data=trainEM,
                      control=rpart.control(maxdepth=5, 
                                            minsplit=4))


predicted_bagging <- predict(Cars.bagging, testEM)
predicted_bagging

confusionMatrix(table(predicted_bagging, testEM$Transport))


#Boosting 
# XGBoost works with matrices that contain all numeric variables
# we also need to split the training data and label

gd_features_train<-as.matrix(trainEM[,1:8])
gd_label_train<-as.matrix(trainEM[,9])
gd_features_test<-as.matrix(testEM[,1:8])
gd_label_test<-as.matrix(testEM[,9])

xgb.fit <- xgboost(
  data = gd_features_train,
  label = gd_label_train,
  eta = 0.001,#this is like shrinkage in the previous algorithm
  max_depth = 3,#Larger the depth, more complex the model; #higher chances of overfitting. There is no standard                      #value for max_depth. Larger data sets require deep trees to #learn the rules from data.
  min_child_weight = 3,#it blocks the potential feature #interactions to prevent overfitting
  nrounds = 10000,#controls the maximum number of iterations. #For classification, it is similar to the number of                       #trees to grow.
  nfold = 5,
  objective = "binary:logistic",  
  verbose = 0,               # silent,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)


predicted_boosting <- predict(xgb.fit, gd_features_test)
predicted_boosting
predicted_boosting = ifelse(predicted_boosting > 0.5 ,1,0)
testEM$Transport

confusionMatrix(table(predicted_boosting, testEM$Transport))
confusionMatrix(table(predicted_glm, testLR$Transport))

