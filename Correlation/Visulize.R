library(tidyverse)
library(hrbrthemes)
require(gridExtra)
# Histogram
a <- ggplot(data = air, aes(x= Temperature))+
  geom_histogram()

b <- ggplot(data = air, aes(x= Wind.Speed..km.h.))+
  geom_histogram()

c <- ggplot(data = air, aes(x= Pressure))+
  geom_histogram()

d <- ggplot(data = air, aes(x= no2))+
  geom_histogram()

e <- ggplot(data = air, aes(x= Rainfall))+
  geom_histogram()

f <- ggplot(data = air, aes(x= PM10))+
  geom_histogram(binwidth = 10)+
  xlim(0,200)

g <- ggplot(data = air, aes(x= AQI))+
  geom_histogram()

h <- ggplot(data = air, aes(x= PM25))+
  geom_histogram()

grid.arrange(a,b,c,d,e,f,g,h, ncol=4)

hist(air$Temperature, breaks=30 , xlim=c(0,60) , col=rgb(1,0,0,0.5) , xlab="height" , ylab="nbr of plants" , main="" )
hist(air$Wind.Speed..km.h., breaks=30 , xlim=c(0,60) , col=rgb(0,0,1,0.5) , xlab="height" , ylab="" , main="")

p <- ggplot(air, aes(x=x) ) +
  # Top
  geom_density( aes(x = Temperature, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=4.5, y=0.25, label="variable1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = Wind.Speed..km.h., y = -..density..), fill= "#404080") +
  geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
  theme_ipsum() +
  xlab("value of x")
p


grid.arrange(a,b,c,d,e,f,g,h, ncol=4)
