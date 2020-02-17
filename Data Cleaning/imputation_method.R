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


air <- read.csv(file = "orginal")
air <- air[, 5:10]
str(air)
table(is.na(c1))

summary(c1)
 

Num_NA<-sapply(c1,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(c1),Count=Num_NA)

NA_Count

# Missing data
p <- function(x) {sum(is.na(x))/ length(x)*100}
apply(c1, 2, p)
md.pattern(c1)
md.pairs(c1)
marginplot(c1[,c('PM25', 'PM10')])

#impute


impute <- mice(c1, m=3, seed = 123)
print(impute)  #predictive mean matching (pmm)  polyreg(multi nominal logistic regression)

impute$imp$PM25
impute$imp$PM10
impute$imp$AQI



#complete data
newdata <- complete(impute, 2 )

# Distribution of observed/imputed values
stripplot(impute, pch = 20, cex = 1.2)
xyplot(impute, so2 ~ no2 | .imp, pch = 20, cex = 1.4)
write.csv(newdata, "new_c6.csv")

