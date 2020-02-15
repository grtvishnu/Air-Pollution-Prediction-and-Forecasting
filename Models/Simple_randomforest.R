library(randomForest)
library(caret)


air <- read.csv(file = "orginal.csv")
str(air)

set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
training <- air[ind==1, ]
test <- air[ind==2, ]

set.seed(222)
rf <- randomForest(PM25~., data = training,
                   ntree = 300,
                   mtry = 12,
                   importance = T,
                   proximity = T)
print(rf)
attributes(rf)


p1 <- predict(rf, test)
rmse(log(test$PM25),log(p1))

pre <- Prediction_3
act <- testing$PM25
t1 <- cbind(pre,act)
view(t1)

plot(rf)
 
# Tuning
t <- tuneRF(training[, -11], training[, 11],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 300,
       trace = T,
       improve = 0.05)

#Histogram
hist(treesize(rf),
     main = "No of nodes for the trees",
     col = "green")
 
#variable Importence

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10")

importance(rf)
# varUsed(rf)
# # MDSplot(rf, t1)
