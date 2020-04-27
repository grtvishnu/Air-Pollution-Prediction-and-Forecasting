library(tidyverse)
library(psycho)
library(ggplot2)

# Read
air <- read_csv("data.csv")
str(air)

# Summary
air %>% 
  select(PM25, AQI) %>% 
  summary()

# Histogram pm25
ggplot(data = air, aes(air$PM25))+
  geom_histogram()

hist(air$PM25, col = 'red')
hist(air$AQI, col = 'red')

# Normalization by z score
m <- colMeans(air)
s <- apply(air, 2, sd)
z_air <- scale(air, center = m, scale = s)
z_air <- as.tibble(z_air)
head(z_air)

c2$AQI<- scale(c2$AQI, center = T, scale = T)