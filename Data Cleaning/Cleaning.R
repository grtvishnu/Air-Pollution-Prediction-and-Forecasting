library(tidyverse)
library(mice)
library(VIM)
air <- read_csv("data.csv")

#replace 0 and -999 to NA
air$PM25 <- replace(air$PM25, air$PM25 <= 0, NA)
air$PM10 <- replace(air$PM10, air$PM10 <= 0, NA)
air$AQI <- replace(air$AQI, air$AQI <= 0, NA)

air$PM25 <- replace(air$PM25, air$PM25 >= 1000, NA)
air$PM10 <- replace(air$PM10, air$PM10 >= 1000, NA)
air$AQI <- replace(air$AQI, air$AQI >= 700, NA)

# find number of NA in the data frame
Num_NA <- sapply(air, function(y) length(which(is.na(y) == T)))
NA_Count <- data.frame(Item = colnames(air), Count = Num_NA)
NA_Count

#method 1
air <- na.omit(air)

#Method 2
impute <- mice(air, m = 5, seed = 123)
print(impute) # predictive mean matching (pmm)  polyreg(multi nominal logistic regression)

impute$imp$PM25
impute$imp$PM10
impute$imp$AQI
impute$imp$Temperature
impute$imp$Rainfall
impute$imp$no2
impute$imp$Wind.Speed..km.h.
impute$imp$Pressure

cleaned_air <- complete(impute, 2) # 1 2 3 4 5

# Distribution of observed/imputed values
stripplot(impute, pch = 20, cex = 1.2)
xyplot(impute, so2 ~ no2 | .imp, pch = 20, cex = 1.4)

