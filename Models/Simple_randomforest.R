library(randomForest)
library(caret)
library(Metrics)

#load data
air <- read.csv(file = "orginal.csv")
str(air)

#split data
set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
training <- air[ind==1, ]
test <- air[ind==2, ]
#create model
set.seed(222)
rf <- randomForest(PM25~., data = training,
                   mtry =12,
                   ntree = 300)
#print model
print(rf)
attributes(rf)

#fit models
p1 <- predict(rf, test)

#test accuracy
rmse(log(test$PM25),log(p1))

#accuracy table
pre <- p1
act <- test$PM25
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
