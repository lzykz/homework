install.packages("keras") 
library(reticulate)
virtualenv_create("r-reticulate")
library(keras)
install_keras(envname = "r-reticulate")
 library(tensorflow)

install_tensorflow(envname = "r-reticulate")


library(keras)
model_keras <- keras_model_sequential()
model_keras %>%
  # First hidden layer
  layer_dense(
    units              = 16,
    kernel_initializer = "uniform",
    activation         = "relu",
    input_shape        = 56) %>%
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  # Second hidden layer
  layer_dense(
    units              = 16,
    kernel_initializer = "uniform",
    activation         = "relu") %>%
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  # Output layer
  layer_dense(
    units              = 1,
    kernel_initializer = "uniform",
    activation         = "sigmoid") %>%
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )
summary(model_keras)

tf$constant("Hellow Tensorflow")
