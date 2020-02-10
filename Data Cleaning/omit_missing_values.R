# setwd("C:/Users/optra/Desktop/pollution")
# pollu.full <- read.csv(file = "new_orginal.csv")
#                str(pollu.full)
# View(pollu.full)
# head(pollu.full)
# summary(pollu.full)
# install.packages("ggplot2")
# 
# dim(pollu.full)
# 
# names(pollu.full)
# 
# library(dplyr)
# glimpse(pollu.full)
# 
# summary(pollu.full)
# 
# hist(pollu.full$X_wdire)
# 
# #install.packages("VIM")
# library(VIM)
# ?kNN()
# 
# pollu.find <- kNN(pollu.full, variable = c("SO_2","PM10"), k=6)
# summary(pollu.find)
# 
# pollu.impute <- kNN(pollu.full)
# summary(pollu.impute)
# 
# head(pollu.impute)
# 
# pollu.impute <- subset(pollu.impute, select =T:PM.2.5 )
# head(pollu.impute)
# 
# write.csv(pollu.impute, file = "new_orginal.csv")
# 
# #install.packages("keras")
# 
# library(keras)



setwd("C:/Users/optra/Desktop/air_pollution/Data/Orginal_datas")
air_pollu <- read.csv(file = "orginal_4.csv")
is.na(air_pollu)
table(is.na(air_pollu))
sum(is.na(air_pollu))
View(air_pollu)
nona_airpollu<-na.omit(air_pollu) 
mean(air_pollu$so2, na.rm = TRUE)

write.csv(nona_airpollu,"orginal_4.csv")



