m16 <- read.csv("mu16.csv")
m17 <- read.csv("mu17.csv")
m18 <- read.csv("mu18.csv")
m19 <- read.csv("mu19.csv")
m20 <- read.csv("mu20.csv")

library(gtools)


mu <-smartbind(m16,m17,m18,m19,m20)


write.csv(mu, "orginal_ko.csv")

