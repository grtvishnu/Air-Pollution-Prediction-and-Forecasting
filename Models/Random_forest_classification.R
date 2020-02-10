library(randomForest)
library(caTools)


air <- read.csv(file = "classification_good_haz.csv")
#air <- air[,2:11]
str(air)
set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
training <- air[ind==1,1:10]
test <- air[ind==2, 1:10]
names(air)

stevensForest <- randomForest(PM25 ~ Temperature + Humidity + Wind.Speed..km.h. + Visibility + Pressure + so2 + no2 + Rainfall + PM10, data = training, nodesize = 25, ntree = 200)


predictForest <- predict(stevensForest, newdata = test)
table(test$PM25, predictForest)

fit$confusion[, 'class.error']
is.factor(air$PM25)


