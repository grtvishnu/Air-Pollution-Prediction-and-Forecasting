library(tidyverse)
library(caret)
library(Hmisc)
library(PerformanceAnalytics)
library(corrplot)
library(ccgarch)

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
