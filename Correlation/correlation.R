library(tidyverse)
library(Hmisc)
library(PerformanceAnalytics)
library(corrplot)
library(ccgarch)

# read data 
air <- read_csv(file = "data.csv")
names(air)

# Correlation of one by one features

A <- cor.test(air$PM25, air$Wind.Speed..km.h., method = "pearson")
A
B <- cor.test(air$PM25, air$Pressure, method = "pearson")
B
C <- cor.test(air$PM25, air$no2, method = "pearson")
C
D <- cor.test(air$PM25, air$Rainfall, method = "pearson")
D
E <- cor.test(air$PM25, air$PM10, method = "pearson")
E
G <- cor.test(air$PM25, air$AQI, method = "pearson")
G
H <- cor.test(air$PM25, air$Temperature, method = "pearson")
H

Ai <- cor.test(air$AQI, air$Wind.Speed..km.h., method = "pearson")
Ai
Bi <- cor.test(air$AQI, air$Pressure, method = "pearson")
Bi
Ci <- cor.test(air$AQI, air$no2, method = "pearson")
Ci
Di <- cor.test(air$AQI, air$Rainfall, method = "pearson")
Di
Ei <- cor.test(air$AQI, air$PM10, method = "pearson")
Ei
Gi <- cor.test(air$AQI, air$PM25, method = "pearson")
Gi
Hi <- cor.test(air$AQI, air$Temperature, method = "pearson")
Hi

# Correlation matrix
res <- cor(air)
res

res2 <- rcorr(as.matrix(air))
res2

# Visualization of correlation
chart.Correlation(air, histogram = T, pch = 19)

corrplot(res, method="circle")
corrplot(res, method="pie")
corrplot(res, method="color")
corrplot(res, method="number")
corrplot(res, type="upper")
corrplot(res, type="upper", order="hclust")

palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = res, col = palette, symm = TRUE)
