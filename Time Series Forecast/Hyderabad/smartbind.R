h16 <- read.csv("hy16.csv")
h17 <- read.csv("hy17.csv")
h18 <- read.csv("hy18.csv")
h19 <- read.csv("hy19.csv")
h20 <- read.csv("hy20.csv")

library(gtools)


hy <-smartbind(h16,h17,h18,h19,h20)


write.csv(hy, "org_hy.csv")
