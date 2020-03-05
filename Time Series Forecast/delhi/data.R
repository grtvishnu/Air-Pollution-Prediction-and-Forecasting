d1 <- read.csv("dh16.csv")
d2 <- read.csv("dh17.csv")
d3 <- read.csv("dh18.csv")
d4 <- read.csv("dh19.csv")
d5 <- read.csv("dh20.csv")


library(gtools)

dh<- smartbind(d1,d2,d3,d4,d5)


write.csv(dh, "org_dh.csv")


