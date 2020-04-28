library(tidyverse)
library(psych)
library(Hmisc)
library(caret)
library(MLmetrics)

air <-read_csv("data.csv")

set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
train <- air[ind == 1, ]
test <- air[ind == 2, ]

# Correlation
air %>%
  cor() %>%
  round(2)

cor.test(air$AQI, air$PM25)
#Correlation matrix
air %>%
  as.matrix() %>%
  rcorr()

air <- air %>%
  as.tibble() %>%
  print()
#Scatter plot
air %>%
  plot()
tic <- print(Sys.time()) 
# model 1
lm(PM25 ~ AQI, data = train) %>% abline()
fit <- lm(AQI ~PM25, data = train)
ypred <- predict(fit, newdata = test)
RMSE(ypred, test$AQI )
toc <- print(Sys.time())
print(toc-tic)
fit
summary(fit)
confint(fit)
predict(fit)
predict(fit, interval = "prediction")
lm.influence(fit)
influence.measures(fit)

#Model 2
fit1 <- lm(PM25 ~ ., data = air)

summary(fit1)
confint(fit1)
predict(fit1)
ypred <- predict(fit1, newdata = test)
RMSE(ypred, test$PM25)
