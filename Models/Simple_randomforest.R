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
rf <- randomForest(AQI~., data = training,
                   mtry =10,
                   ntree = 500,
                   proximity=F)
#print model
print(rf)
attributes(rf)

#fit models
p1 <- predict(rf, test)

#test accuracy
rmse(log(test$AQI),log(p1))

#accuracy table
pre <- p1
act <- test$PM25
t1 <- cbind(pre,act)
view(t1)

plot(rf, log="y")


#fine tuning
oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
        rf <- randomForest(PM25~., data = training,
                           mtry =mtry,
                           ntree = 500,
                           proximity=T)
        oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
        
        pred<-predict(rf, test) #Predictions on Test Set for each Tree
        test.err[mtry]= with(test, mean( (PM25 - pred)^2)) #Mean Squared Test Error
        
        cat(mtry," ") #printing the output to the console
        
}

test.err

oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))


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

a <- importance(rf)
# varUsed(rf)
# # MDSplot(rf, t1)
plot(a)
