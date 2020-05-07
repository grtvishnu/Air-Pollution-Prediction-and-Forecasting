library(tidyverse)

air <- read_csv("data.csv")

AQI <- air$AQI

x = c(0,50,100,150,200,300,Inf) 

air$AQI <- cut(AQI,x, 6, labels = c("Good", "Moderate","Unhealthy for sensitive groups",
                       "Unhealthy","Very Unhealthy","Hazardous"), )

write_csv(air,"data_cat.csv")


