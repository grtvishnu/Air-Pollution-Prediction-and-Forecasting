library(tidyverse)
library(lubridate)

c1 <- read_csv("org_ch.csv")
c1 <- c1[, 2:8]
c1 <- c1[, c(7,1,2,3,4,5,6)]

summary(c1)

c1<-c1 %>% 
  mutate(dates = make_datetime(Year, Month, Day))


write.csv(c1, "org_mu.csv")
