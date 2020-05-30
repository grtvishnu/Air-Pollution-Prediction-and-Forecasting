# add library and read data
library(tidyverse)
air <- read_csv("data.csv")
summary(air)

# removed the outliers in Pressure 
hist(air$Pressure)
uv <-  quantile(air$Pressure , .1)
air$Pressure[air$Pressure < uv] <- uv
summary(air$Pressure)

# removed the outliers in Rainfall
hist(air$Rainfall)
quantile(air$Rainfall, .1)
vu <- quantile(air$Rainfall, .1)
air$Rainfall[air$Rainfall < vu] <- vu

# removed the outliers in PM25
summary(air$PM25)
hist(air$PM25)
quantile(air$PM25,.99)
lv <- quantile(air$PM25,.99)
air$PM25[air$PM25 > lv] <- lv

# removed the outliers in AQI
summary(air$AQI)
hist(air$AQI)
quantile(air$AQI,.99)
ls <-quantile(air$AQI,.99)
air$AQI[air$AQI > ls] <- ls

# removed the outliers in AQI
summary(air$no2)
quantile(air$no2, .99)
hist(air$no2)
ld <- quantile(air$no2, .99)
air$no2[air$no2 > ld] <- ld

# write into a csv
write_csv(air, "data.csv")


ggplot(classby2,aes(Rainfall))+
  geom_histogram()
