library(keras)
library(tensorflow)
library(ggplot2)
library(stats)
library(readr)
library(dplyr)
library(forecast)

#Inspect Data
ggplot(df,aes(ds,y))+geom_line()

#Transform into stationarity
dateID <- df$ds[2:nrow(df)]
diff <- diff(df$y, differencs = 1)

#Create Lagged Dataset
supervised <- as.data.frame(cbind(lag(diff,1), diff))
supervised[is.na(supervised)] <- 0

#split train test
n_ <- round(nrow(df) * .85, digits = 0)
train <- supervised[1:n_, ]
test <- supervised[(n_+1):35502,]
train_id <- dateID[1:n_]
test_id <- dateID[(n_+1):35502]




scale_data <- function(train, test, feature_range = c(0,1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = (x - min(x)) / (max(x) - min(x))
  std_test = (test - min(x)) / (max(x) - min(x))
  
  scaled_train = std_train * (fr_max - fr_min) + fr_min
  scaled_test = std_test * (fr_max - fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
}


#Function to reverse scale data for prediction
reverse_scaling <- function(scaled, scaler, feature_range = c(0,1)) {
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for(i in 1:t) {
    X = (scaled[i] - mins) / (maxs - mins)
    rawValues = X * (max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

Scaled <- scale_data(train, test, c(-1,1))

x_train <- Scaled$scaled_train[,1]
y_train <- Scaled$scaled_train[,2]

x_test <- Scaled$scaled_test[,1]
y_test <- Scaled$scaled_test[,2]

dim(x_train) <- c(length(x_train), 1,1)
X_shape2 <- dim(x_train)[2]
X_shape3 <- dim(x_train)[3]
batch_size <- 1
units <- 100
n_timesteps <- 12
n_predictions <- n_timesteps

build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x) 
    tseries[x:(x + overall_timesteps - 1)]))
}
reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}

model <- keras_model_sequential()
model %>% 
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful = TRUE) %>%
  layer_dense(units = 1)

model %>% 
  compile(loss = 'mean_squared_error',
          optimizer = optimizer_adam(lr = 0.03, decay = 1e-6),
          metrics = c('accuracy')
  )

summary(model)

Epochs = 3   
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}


