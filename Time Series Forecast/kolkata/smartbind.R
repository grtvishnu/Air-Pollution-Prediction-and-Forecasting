k16 <- read.csv("ko16.csv")
k17 <- read.csv("ko17.csv")
k18 <- read.csv("ko18.csv")
k19 <- read.csv("ko19.csv")
k20 <- read.csv("ko20.csv")

library(gtools)


ko <-smartbind(k16,k17,k18,k19,k20)


write.csv(ko, "orginal_ko.csv")

