library(tidyverse)
library(caret)
library(rpart)
library(weka)
air <- read.csv(file = "orginal_7.csv")
air <- air[,2:11]

set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
training <- air[ind==1,1:10]
test <- air[ind==2, 1:10]

#Linear Regression

model <- lm(PM25 ~., data = training)
predictions <- model %>% predict(test)
data.frame( R2 = R2(predictions, test$PM25),
            RMSE = RMSE(predictions, test$PM25),
            MAE = MAE(predictions, test$PM25))


RMSE(predictions, test$PM25)/mean(test$PM25)

# Leave one out cross validation - LOOCV

train.control <- trainControl(method = "LOOCV")
# Train the model
model <- train(PM25 ~., data = air, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)


# K-fold cross-validation
set.seed(123) 
train.control <- trainControl(method = "repeatedcv", number = 10) #method = cv
# Train the model
model <- train(PM25 ~., data = air, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)


# kfold

rdesc <- makeResampleDesc("CV",iters=5L)






cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(PM25 ~ Temperature + Humidity + Wind.Speed..km.h. + Visibility + Pressure + so2 + no2 + Rainfall + PM10, data = training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)



(PM25 ~ Temperature + Humidity + Wind.Speed..km.h. + Visibility + Pressure + so2 + no2 + Rainfall + PM10, data = training, nodesize = 25, ntree = 200)