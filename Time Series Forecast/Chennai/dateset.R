c1 <- read.csv("orginal_ch.csv")

c1 <- c1[, 2:8]
c1 <- c1[, c(7,1,2,3,4,5,6)]
library(tidyverse)

library(lubridate)
summary(c1)
today()
now()

ymd("2017-01-31")

mdy("January 31st, 2017")

dmy("31-Jan-2017")

c1<-c1 %>% 
  mutate(dates = make_datetime(Year, Month, Day))


write.csv(c1, "org_mu.csv")
