# Libraries
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
library(tensorflow)
library(reticulate)
# Data
air <- read.csv(file = "orginal.csv")
# air <- air[,5:10]
# air$PM25 <- as.numeric(air$PM25)
str(air)

# Neural Network Visualization
n <- neuralnet(PM25 ~ Temperature + Humidity + Wind.Speed..km.h. + Visibility + Pressure + so2 + no2 + Rainfall + PM10 + AQI,
               data = air,
               hidden = c(10, 5),
               linear.output = F,
               lifesign = "full",
               rep = 1,
)
plot(n,
     col.hidden = "darkgreen",
     col.hidden.synapse = "darkgreen",
     show.weights = F,
     information = F,
     fill = "lightblue"
)

# Matrix
air <- as.matrix(air)
dimnames(air) <- NULL

# Partition
set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
training <- air[ind == 1, 1:10]
test <- air[ind == 2, 1:10]
trainingtarget <- air[ind == 1, 11]
testtarget <- air[ind == 2, 11]

# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

# Create Model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = "relu", input_shape = c(10)) %>%
  layer_dense(units = 1, )

# Compile
model %>% compile(
  loss = "mse",
  optimizer = "rmsprop",
  metrics = "mae"
)

model
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
# finemodel
model <- keras_model_sequential()
model %>%
  layer_dense(units = 10, activation = "relu", input_shape = c(10)) %>%
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
plot(mymodel)
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
model %>% summary(test)

mean((testtarget - pred)^2)
plot(testtarget, pred)

# save(mymodel, file = "deepneural.rda")

# ultra finetuning

model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(10)) %>%
  layer_dropout(rate = 0.03) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.02) %>%
  layer_dense(units = 20, activation = "relu") %>%
  layer_dropout(rate = 0.01) %>%
  layer_dense(units = 1, )
summary(model)
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
mean((testtarget - pred)^2)
plot(testtarget, pred)
plot(mymodel)
