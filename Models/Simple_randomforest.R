library(randomForest)
library(caret)
library(Metrics)
library(tidyverse)

# load data
air <- read_csv(file = "data.csv")
str(air)

# split data
set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
training <- air[ind == 1, ]
test <- air[ind == 2, ]

tic <- print(Sys.time()) 
# create model
set.seed(222)
rf <- randomForest(PM25 ~ .,
                   data = training,
                   mtry = 3,
                   ntree = 500,
                   proximity = F
)
toc <- print(Sys.time())
print(toc-tic)
# print model
summary(rf)
print(rf)
attributes(rf)
rf$importance
# fit models
p1 <- predict(rf, test)

# test accuracy
RMSE(p1, test$PM25)

# accuracy table
pre <- p1
act <- test$PM25
t1 <- cbind(pre, act)
View(t1)
plot(rf, log = "y")


# tuning parameter (Grid search)
oob.err <- double(13)
test.err <- double(13)

# mtry is no of Variables randomly chosen at each split
for (mtry in 1:12)
{
  rf <- randomForest(PM25 ~ .,
                     data = training,
                     mtry = mtry,
                     ntree = 500,
                     proximity = T
  )
  oob.err[mtry] <- rf$mse[400] # Error of all Trees fitted
  
  pred <- predict(rf, test) # Predictions on Test Set for each Tree
  test.err[mtry] <- with(test, mean((PM25 - pred)^2)) # Mean Squared Test Error
  
  cat(mtry, " ") # printing the output to the console
}

test.err

oob.err

matplot(1:mtry, cbind(oob.err, test.err), pch = 19, col = c("red", "blue"), type = "b", ylab = "Mean Squared Error", xlab = "Number of Predictors Considered at each Split")
legend("topright", legend = c("Out of Bag Error", "Test Error"), pch = 19, col = c("red", "blue"))


# Tuning
t <- tuneRF(training[, -10], training[, 10],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = T,
            improve = 0.05
)

# Histogram
hist(treesize(rf),
     main = "No of nodes for the trees",
     col = "green"
)

# variable Importence

varImpPlot(rf,
           sort = T,
           n.var = 7,
           main ="Feature Importance of Random Forest"
)

# Transfer Learning

saveRDS(rf, file = "random_forest.rds")
rf<-readRDS("random_forest.rds")