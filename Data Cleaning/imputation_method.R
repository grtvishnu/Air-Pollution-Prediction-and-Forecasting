#install.packages("VIM")
library(mice)
library(VIM)
library(MASS) 
library(Metrics)
library(corrplot)
library(randomForest)
library(lars)
library(ggplot2)
library(xgboost)
library(Matrix)
library(methods)
library(caret)
library(tidyverse)


air <- read.csv(file = "chennai18.csv")
air <- air[, 5:10]
str(air)
table(is.na(air))

summary(air)
 

Num_NA<-sapply(air,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(air),Count=Num_NA)

NA_Count

# Missing data
p <- function(x) {sum(is.na(x))/ length(x)*100}
apply(air, 2, p)
md.pattern(air)
md.pairs(air)
marginplot(air[,c('PM25', 'PM10')])

#impute


impute <- mice(air, m=3, seed = 123)
print(impute)  #predictive mean matching (pmm)  polyreg(multi nominal logistic regression)

impute$imp$CO
impute$imp$PM10
impute$imp$AQI



#complete data
newdata <- complete(impute, 3 )

# Distribution of observed/imputed values
stripplot(impute, pch = 20, cex = 1.2)
xyplot(impute, so2 ~ no2 | .imp, pch = 20, cex = 1.4)
write.csv(newdata, "imputed_chennai2018_2.csv")
