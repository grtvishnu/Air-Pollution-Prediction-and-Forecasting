library(tidyverse)
library(caret)
library(Hmisc)
library(PerformanceAnalytics)
library(corrplot)
library(ccgarch)
library(randomForest)

# read data -----------------------------------------------------------------------------------

kozhi <- read_csv("kozhikodu.csv")
kollam <- read_csv("kollam.csv")
kochi <- read_csv("kochi.csv")
kannur <- read_csv("kannur.csv")

# DataPreprocessing ---------------------------------------------------------------------------


head(kozhi)
kozhi<- kozhi[ ,-1]
org_kozhi<- kozhi %>% 
  spread(parameter,value)

# rm(kozhi)
# kozhi <- org_kozhi
# rm(org_kozhi)

kozhi <- kozhi %>% 
  select(-local,-latitude,-longitude)
kozhi

# Correlation ---------------------------------------------------------------------------------


res<- kozhi %>% 
  select(co:so2) %>% 
  cor()

chart.Correlation(res, histogram = T, pch = 19)
corrplot(res, method="circle")
corrplot(res, method="pie")
corrplot(res, method="color")
corrplot(res, method="number")
corrplot(res, type="upper")
corrplot(res, type="upper", order="hclust")
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = res, col = palette, symm = TRUE)


# Outlier treatment ---------------------------------------------------------------------------

summary(kozhi)
boxplot(kozhi$co)
boxplot(kozhi$o3)
boxplot(kozhi$so2)
boxplot(kozhi$no2)
boxplot(kozhi$pm25)
boxplot(kozhi$pm10)

summary(kozhi$co)
summary(kozhi$no2)
summary(kozhi$o3)
summary(kozhi$pm10)
summary(kozhi$pm25)
summary(kozhi$so2)

#winsering method for treating outliers

#co
bench <- 510 + 1.5*IQR(kozhi$co)
kozhi<- kozhi %>% 
  mutate(co=replace(co, co>1023.75,1023.75)) %>% 
  mutate(co=replace(co, co<=0,50))
summary(kozhi$co)

#o3
bench <- 12.232 + 1.5*IQR(kozhi$o3)
kozhi<- kozhi %>% 
  mutate(o3=replace(o3, o3>bench,bench))
summary(kozhi$o3)

#so2
bench <- 3.130 + 1.5*IQR(kozhi$so2)
kozhi<- kozhi %>% 
  mutate(so2=replace(so2, so2>bench,bench))
summary(kozhi$so2)
boxplot(kozhi$so2)

#no2
bench <- 11.20 + 1.5*IQR(kozhi$no2)
kozhi<- kozhi %>% 
  mutate(no2=replace(no2, no2>bench,bench))
summary(kozhi$no2)
boxplot(kozhi$no2)

#pm25
bench <- 33.00 + 1.5*IQR(kozhi$pm25)
kozhi<- kozhi %>% 
  mutate(pm25=replace(pm25, pm25>bench,bench))
summary(kozhi$pm25)
boxplot(kozhi$pm25)

#pm10
bench <- 54.00 + 1.5*IQR(kozhi$pm10)
kozhi<- kozhi %>% 
  mutate(pm10=replace(pm10, pm10>bench,bench))
summary(kozhi$pm10)
boxplot(kozhi$pm10)

rm(a,res,bench,bench1,palette)


# Model Creation ------------------------------------------------------------------------------

# split data
set.seed(1234)
ind <- sample(2, nrow(kozhi), replace = T, prob = c(.7, .3))
training <- kozhi[ind == 1, ]
test <- kozhi[ind == 2, ]


fm <- as.formula(pm25 ~ co + no2 + pm10 +so2)
model <- lm(fm, training)
summary(model)

pred<- predict(model,newdata = test)
RMSE(pred,test$pm25)



# output file ---------------------------------------------------------------------------------

kozhi1 <- kozhi %>% 
  select(co:so2)

write_csv(kozhi1,"model_data.csv")
