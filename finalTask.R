rm(list=ls())
knitr::opts_chunk$set(echo = TRUE)
set.seed(1967) 
library(knitr)
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(corrplot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(data.table)
library(corrplot)
library(plotly)
library(gbm)

setwd("C:/Users/RAZER/source/R/PracticalMachineLearning/data")
#Reading training and test datasets
data_train <- read.csv("pml-training.csv")
data_quiz <- read.csv("pml-testing.csv")
in_train  <- createDataPartition(data_train$classe, p=0.70, list=FALSE)
train_set <- data_train[ in_train, ]
test_set  <- data_train[-in_train, ]
nzv_var <- nearZeroVar(train_set)
train_set <- train_set[ , -nzv_var]
test_set  <- test_set [ , -nzv_var]
#Clear datasets
na_var <- sapply(train_set, function(x) mean(is.na(x))) > 0.95
train_set <- train_set[ , na_var == FALSE]
test_set  <- test_set [ , na_var == FALSE]
train_set <- train_set[ , -(1:5)]
test_set  <- test_set [ , -(1:5)]

dim(train_set)
dim(test_set)

#Decision Tree Model
set.seed(1967)
fit_DT <- rpart(classe ~ ., data = train_set, method="class")
fancyRpartPlot(fit_DT)
predict_DT <- predict(fit_DT, newdata = test_set, type="class")
conf_matrix_DT <- confusionMatrix(table(predict_DT, test_set$classe))
conf_matrix_DT
plot(conf_matrix_DT$table, col = conf_matrix_DT$byClass, 
     main = paste("Decision Tree Model: Predictive Accuracy =",
                  round(conf_matrix_DT$overall['Accuracy'], 4)))

#Random Forest Model
set.seed(1967)
ctrl_RF <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
fit_RF  <- train(classe ~ ., data = train_set, method = "rf",
                 trControl = ctrl_RF, verbose = FALSE)
fit_RF$finalModel
predict_RF <- predict(fit_RF, newdata = test_set)
conf_matrix_RF <- confusionMatrix(table(predict_RF, test_set$classe))
conf_matrix_RF

plot(fit_RF)
print(fit_RF$bestTune)

#Predictions for testing dataset
cat("Predictions: ", predict(fit_RF, data_quiz))
