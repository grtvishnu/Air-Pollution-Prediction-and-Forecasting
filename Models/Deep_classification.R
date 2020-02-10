library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
library(tensorflow)

air <- read.csv(file = "orginal_6.csv")
str(air)
air <- air[,3:12]
air$PM25 <- as.numeric(air$PM25)
air <- as.matrix(air)
summary(air)


set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(0.7, 0.3))
train <- air[ind==1, 1:10]
test <- air[ind==2, 1:10]
traintarget <- air[ind==1, 1:10]
testtarget <- air[ind==2, 1:10]


trainlabels <- to_categorical(traintarget)
testlabels <- to_categorical(testtarget)


# create model

model <- keras_model_sequential()
model %>%
  layer_dense(units = 8, activation = 'relu', input_shape = c(10)) %>%
  layer_dense(units = 1, activation = 'softmax')


# install_tensorflow()
#compile

model %>% compile(loss ='sparse_categorical_crossentropy', 
                  optimizer = 'adam', 
                  metrics = 'accuracy')



history <- model %>%
  fit(train,
      trainlabels,
      epoch = 200,
      batch_size = 32,
      validation_split = 0.2)

