setwd("C:/Users/optra/Desktop/air_pollution/Data/Orginal_datas")
pollu.full <- read.csv(file = "orginal_3.csv", stringsAsFactors = FALSE, header = TRUE)


#pollu.full$PM10 <- scale(pollu.full$PM10)
pollu.full$scalePM25 <- scale(pollu.full$PM25)
var(pollu.full$PM25)
sd(pollu.full$PM25)

y <- pollu.full$scalePM25 - mean(pollu.full$scalePM25 / sd(pollu.full$scalePM25))

y

mean(y)
sd(y)
var(y)
pollu.full$normPM25 <- y

View(y)


pollu.full$PM25


mean(pollu.full$PM25)
var(pollu.full$PM25)
sd(pollu.full$PM25)
