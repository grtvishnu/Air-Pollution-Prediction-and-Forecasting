library(randomForest)
library(caret)


air <- read.csv(file = "classification_good_haz.csv")
air$PM25 <- as.numeric(air$PM25)
str(air)

air$PM25 <- as.factor(air$PM25)
table(air$PM25)


set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
training <- air[ind==1, ]
test <- air[ind==2, ]


set.seed(222)
rf <- randomForest(PM25~., data = training)
print(rf)
attributes(rf)


p1 <- predict(rf, training)
confusionMatrix(p1, training$PM25)

  

p2 <- predict(rf, test)
confusionMatrix(p1, training$PM25)

