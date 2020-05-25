library(tidyverse)
set.seed(1234)
x_vlow <- data.frame("no2"=runif(55,min = 1, max = 50),"pm10"=runif(55,min = 1, max = 25),
                    "o3"=runif(55,min = 1, max = 60),"pm25"=runif(55,min = 1, max = 15))

x_low <- data.frame("no2"=runif(55,min = 50, max = 100),"pm10"=runif(55,min = 25, max = 50),
                    "o3"=runif(55,min = 60, max = 155),"pm25"=runif(55,min = 15, max = 30))

x_medi <- data.frame("no2"=runif(55,min = 100, max = 550),"pm10"=runif(55,min = 50, max = 90),
                    "o3"=runif(55,min = 155, max = 180),"pm25"=runif(55,min = 30, max = 55))

x_high <- data.frame("no2"=runif(55,min = 200, max = 400),"pm10"=runif(55,min = 90, max = 180),
                    "o3"=runif(55,min = 180, max = 240),"pm25"=runif(55,min = 55, max = 110))

x_vhigh <- data.frame("no2"=runif(55,min = 400, max = 600),"pm10"=runif(55,min = 180, max = 270),
                    "o3"=runif(55,min = 240, max = 300),"pm25"=runif(55,min = 110, max = 170))



x_vlow <- x_vlow %>% 
  mutate(AQI="Good")

x_low <- x_low %>% 
  mutate(AQI="Moderate")

x_medi <- x_medi %>% 
  mutate(AQI="Unhealthy")

x_high <- x_high %>% 
  mutate(AQI="Very Unhealthy")

x_vhigh <- x_vhigh %>% 
  mutate(AQI="Hazardous")

x <- rbind(x_vlow,x_low,x_medi,x_high,x_vhigh)


rows <- sample(nrow(x))

x <- x[rows, ]

write_csv(x,"air_qua.csv")
