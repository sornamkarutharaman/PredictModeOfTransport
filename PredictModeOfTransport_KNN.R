#======================================================
#     PredictModeOfTransport - KNN
#======================================================

#Install required packages
library(caTools)  
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)
library(readr)

#Set seed to constant value
set.seed(123)      

#Scale
Cars_dataset_KNN[, c("Age","Work_Experience", "Salary", "Distance","Engineer","MBA","license")] <- scale(Cars_dataset_KNN[, c("Age","Work_Experience", "Salary", "Distance","Engineer","MBA","license")])
head(Cars_dataset_KNN)


# Change Category to numeric
Cars_dataset_KNN$Gender <- dummy.code(Cars_dataset_KNN$Gender)


# Split Dataset
split <- sample.split(Cars_dataset_KNN$Transport, SplitRatio = 0.70) 
str(Cars_dataset_KNN)

# creating test and training sets that contain all of the predictors
carsKNN_pred_train <- subset(Cars_dataset_KNN, split == T) 
carsKNN_pred_test <- subset(Cars_dataset_KNN, split == F)
dim(carsKNN_pred_train)
dim(carsKNN_pred_test)
table(carsKNN_pred_train$Transport)
table(carsKNN_pred_test$Transport)
library(class)

Transport_pred_knn <-knn(train = carsKNN_pred_train[,1:8], test = carsKNN_pred_test[,1:8],cl = carsKNN_pred_train$Transport,k = 3, prob = TRUE) 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Transport ~., data = carsKNN_pred_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
plot(knn_fit)

test_pred <- predict(knn_fit, newdata = carsKNN_pred_test)
test_pred

confusionMatrix(test_pred, carsKNN_pred_test$Transport )


#ROCR Curve
library(ROCR)
ROCRpred <- prediction(test_pred, carsKNN_pred_test$Transport)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

