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
                   mtry =10,
                   ntree = 500,
                   proximity=T)
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

plot(rf, log="y")
 
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


oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
        rf=randomForest(medv ~ . , data = Boston , subset = train,mtry=mtry,ntree=400) 
        oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
        
        pred<-predict(rf,Boston[-train,]) #Predictions on Test Set for each Tree
        test.err[mtry]= with(Boston[-train,], mean( (medv - pred)^2)) #Mean Squared Test Error
        
        cat(mtry," ") #printing the output to the console
        
}
## 1  2  3  4  5  6  7  8  9  10  11  12  13




matplot(1:10 , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))



obb.err
