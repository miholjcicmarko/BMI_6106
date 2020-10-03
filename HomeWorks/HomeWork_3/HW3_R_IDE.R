data1 = read.csv("data1.csv", header = T)

library(tidyverse)
library(ggplot2)
library(caret)
library(psych)
library(rpart)
library(randomForest)
library(Amelia)
library(mice)
library(e1071)
library(klaR)
library(dplyr)

library(caret)
library(lattice)
set.seed(998)
indxTrain <- createDataPartition(y = data1$classification,p = 0.75,list = FALSE)
training <- data1[indxTrain,] 
testing <- data1[-indxTrain,] 

data_1 <- data1$classification
prop.table(table(data_1)) * 100
prop.table(table(training$classification)) * 100
prop.table(table(testing$classification)) * 100

x = training[,-9]
y = training$classification
y = as.factor(y)
z = testing$classification
z = as.factor(z)

model = train(x,y,'naive_bayes',trControl=trainControl(method='cv',number=10))

Predict <- predict(model,newdata = testing)

confusionMatrix(Predict, z)

X <- varImp(model)
plot(X)

