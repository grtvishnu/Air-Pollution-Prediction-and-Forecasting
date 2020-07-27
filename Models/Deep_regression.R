# Libraries
library(keras)
library(mlbench)
library(tidyverse)
library(magrittr)
library(neuralnet)
library(tensorflow)
library(reticulate)

# Data
air <- read_csv(file = "data.csv")
str(air)

# Neural Network Visualization
n <- neuralnet(PM25 ~ Temperature+ Wind.Speed..km.h.+ Pressure+ no2 + Rainfall + PM10 + AQI,
               data = air,
               hidden = c(10, 5),
               linear.output = F,
               lifesign = "full",
               rep = 1,
)
plot(n,
     col.hidden = "black",
     col.hidden.synapse = "black",
     show.weights = F,
     information = T,
     fill = "white"
)

# Matrix
air <- as.matrix(air)
dimnames(air) <- NULL

# Partition
set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
training <- air[ind == 1, 1:7]
test <- air[ind == 2, 1:7]
trainingtarget <- air[ind == 1, 8]
testtarget <- air[ind == 2, 8]

# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

# Create Model (5 1)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = "relu", input_shape = c(7)) %>%
  layer_dense(units = 1, )

# Compile
model %>% compile(
  loss = "mse",
  optimizer = "rmsprop",
  metrics = "mae"
)

# Fit Model
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2
  )

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget - pred)^2)
plot(testtarget, pred)
plot(mymodel)


# fine model (10, 5)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 10, activation = "relu", input_shape = c(7)) %>%
  layer_dense(units = 5, activation = "relu") %>%
  layer_dense(units = 1, )

# Compile
model %>% compile(
  loss = "mse",
  optimizer = "rmsprop",
  metrics = "mae"
)

# Fit Model
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2
  )

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget - pred)^2)
plot(testtarget, pred)
plot(mymodel)

# ultra fine model (100, 50, 20)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(7)) %>%
  layer_dropout(rate = 0.03) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.02) %>%
  layer_dense(units = 20, activation = "relu") %>%
  layer_dropout(rate = 0.01) %>%
  layer_dense(units = 1, )

# Compile
model %>% compile(
  loss = "mse",
  optimizer = "rmsprop",
  metrics = "mae"
)

# Fit Model
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 35,
      validation_split = 0.2
  )

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
sqrt(mean((testtarget - pred)^2))
plot(testtarget, pred)
plot(mymodel)

#save the model

model %>% save_model_tf("model")
summary(model)

#load model
model <- load_model_tf("model")
summary(model)
