c1 <- read.csv(file = "c6.csv")
#View(c4)
str(c1)

table(is.na(c1))
summary(c1)

# c1$PM25<- replace(c1$PM25, c1$PM25 <0, NA )
# c1$PM10<- replace(c1$PM10, c1$PM10 <0, NA )
# c1$AQI<- replace(c1$AQI, c1$AQI <0, NA )

c1$PM25<- replace(c1$PM25, c1$PM25 <=0, NA )
c1$PM10<- replace(c1$PM10, c1$PM10 <=0, NA )
c1$AQI<- replace(c1$AQI, c1$AQI <=0, NA )

c1$PM25<- replace(c1$PM25, c1$PM25 >=1000, NA )
c1$PM10<- replace(c1$PM10, c1$PM10 >=1000, NA )
c1$AQI<- replace(c1$AQI, c1$AQI  >=250, NA )


summary(c1)

write.csv(c1, "new_c6.csv")
