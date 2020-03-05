library(ggplot2)
library(prophet)
library(tidyverse)
library(tidyquant)
library(cowplot)

#read and remove columns from file
c1 <- read.csv("org_ch.csv")
c1 <- c1[, c(2,8)]
View(c1)

# Visualization of data
qplot(dates, AQI, data = c1)

#store date to ds and log of AQi to y
ds <- c1$dates
y <- c1$AQI

# Create a datframe with it
df <- data.frame(ds, y)

#plot log data to see more seasonality
qplot(ds, y, data = df)

#advanced Visualization
p1 <- c1 %>%
  ggplot(aes(dates, AQI)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "From 2016 to 2020 (Full Data Set)"
  )
p2 <- df %>%
  ggplot(aes(ds, y)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Use Log To Show Cycle",
    caption = "datasets::AirQuality.AQI"
  )
p_title <- ggdraw() + 
  draw_label("AQI", size = 18, fontface = "bold", colour = palette_light()[[1]])

plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))

#create model m using prophet fuction
m <- prophet(df)

#create forecasting variable
future <- make_future_dataframe(m, periods = 7)

#analysing the variable
tail(future)

#Forecasting
forecast <- predict(m, future)

#Summary Model
tail(forecast[c('ds', 'yhat' )])

#Check result
exp(4.058228)


#plot forecasting
plot(m, forecast)


#check seasonality and components
prophet_plot_components(m, forecast)



# calculate accuracy(0.4495045)

RMSE(forecast$yhat, y, na.rm = T)
