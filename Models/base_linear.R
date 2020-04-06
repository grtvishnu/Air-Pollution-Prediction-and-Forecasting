library(tidyverse)
library(psych)
library(Hmisc)
library(caret)

air %>%
  cor() %>%
  round(2)

cor.test(air$AQI, air$PM25)

air %>%
  as.matrix() %>%
  rcorr()

air <- air %>%
  as.tibble() %>%
  print()

air %>%
  plot()

air %>%
  select(everything()) %>%
  plot()

lm(air$PM25 ~ air$AQI) %>% abline()
fit <- lm(air$AQI ~ air$PM25)
fit
summary(fit2)
confint(fit2)
predict(fit2)

predict(fit, interval = "prediction")

lm.influence(fit2)
influence.measures(fit2)

fit1 <- lm(PM25 ~ ., data = air)

summary(fit1)

fit2 <- lm(PM25 ~ Temperature + Wind.Speed..km.h. + Pressure + no2 + Rainfall + PM10 + AQI, data = air)
ypred <- predict(fit2, newdata = test)
summary(fit2)

RMSE(ypred, test$AQI)
