library(tidyverse)
library(hrbrthemes)
require(gridExtra)
library(cowplot)
library(tidyquant)
#Prediction Data
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



# Forecast Data
air_c <- read_csv("org_ch.csv")
air_d <- read_csv("org_dh.csv")
air_h <- read_csv("org_hy.csv")
air_k <- read_csv("org_ko.csv")
air_m <- read_csv("org_mu.csv")

#box plot and outliers
ggplot(data = air_c, aes(x=AQI))+
  geom_boxplot(position = "dodge2", outlier.color = 'red', outlier.shape = 2) +
  coord_flip() + geom_vline(xintercept = 100) + geom_vline(xintercept = 200, col = 'red')

ggplot(data = air_d, aes(x=AQI))+
  geom_boxplot(position = "dodge2", outlier.color = 'red', outlier.shape = 2) +
  coord_flip() + geom_vline(xintercept = 100) + geom_vline(xintercept = 200, col = 'red')

ggplot(data = air_h, aes(x=AQI))+
  geom_boxplot(position = "dodge2", outlier.color = 'red', outlier.shape = 2) +
  coord_flip() + geom_vline(xintercept = 100) + geom_vline(xintercept = 200, col = 'red')

ggplot(data = air_k, aes(x=AQI))+
  geom_boxplot(position = "dodge2", outlier.color = 'red', outlier.shape = 2) +
  coord_flip() + geom_vline(xintercept = 100) + geom_vline(xintercept = 200, col = 'red')

ggplot(data = air_m, aes(x=AQI))+
  geom_boxplot(position = "dodge2", outlier.color = 'red', outlier.shape = 2) +
  coord_flip() + geom_vline(xintercept = 100) + geom_vline(xintercept = 200, col = 'red')


#Histogram
hist(air_c$AQI, col = 'green', breaks = 100)
hist(air_d$AQI, col = 'green', breaks = 100)
hist(air_h$AQI, col = 'green', breaks = 100)
hist(air_k$AQI, col = 'green', breaks = 100)
hist(air_m$AQI, col = 'green', breaks = 100)


p1 <- air_c %>%
  ggplot(aes(dates, AQI)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "Chennai"
  )
p2 <- air_c %>%
  ggplot(aes(dates, AQI)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "With Line",
    caption = "datasets::AirQuality.AQI"
  )
p_title_c <- ggdraw() + 
  draw_label("AQI", size = 18, fontface = "bold", colour = palette_light()[[1]])
plot_grid(p_title_c, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))


p3 <- air_d %>%
  ggplot(aes(dates, AQI)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "Delhi"
  )
p4 <- air_d %>%
  ggplot(aes(dates, AQI)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "With Line",
    caption = "datasets::AirQuality.AQI"
  )
p_title_d <- ggdraw() + 
  draw_label("AQI", size = 18, fontface = "bold", colour = palette_light()[[1]])
plot_grid(p_title_d, p3, p4, ncol = 1, rel_heights = c(0.1, 1, 1))


p5 <- air_h %>%
  ggplot(aes(dates, AQI)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "Hydrabad"
  )
p6 <- air_h %>%
  ggplot(aes(dates, AQI)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "With Line",
    caption = "datasets::AirQuality.AQI"
  )
p_title_h <- ggdraw() + 
  draw_label("AQI", size = 18, fontface = "bold", colour = palette_light()[[1]])
plot_grid(p_title_h, p5, p6, ncol = 1, rel_heights = c(0.1, 1, 1))


p7 <- air_k %>%
  ggplot(aes(dates, AQI)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "Kolkata"
  )
p8 <- air_k %>%
  ggplot(aes(dates, AQI)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "With Line",
    caption = "datasets::AirQuality.AQI"
  )
p_title_k <- ggdraw() + 
  draw_label("AQI", size = 18, fontface = "bold", colour = palette_light()[[1]])
plot_grid(p_title_k, p7, p8, ncol = 1, rel_heights = c(0.1, 1, 1))


p9 <- air_m %>%
  ggplot(aes(dates, AQI)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "Mumbai"
  )
p0 <- air_m %>%
  ggplot(aes(dates, AQI)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "With Line",
    caption = "datasets::AirQuality.AQI"
  )
p_title_m <- ggdraw() + 
  draw_label("AQI", size = 18, fontface = "bold", colour = palette_light()[[1]])
plot_grid(p_title_m, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))

  
grid.arrange(p1,p3,p5,p7,p9, ncol=2)

