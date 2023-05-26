#install 'keras' library 
install.packages("keras") 
install.packages("tensorflow.keras")
library(keras) 

# Load the data 
wisc_bc_data <- read.csv("D://Dev/R programming language/wisc_bc_data.csv", header = TRUE) 
insurance_data <- read.csv("D://Dev/R programming language/Insurance.csv", header = TRUE) 

# Split the data into training and validation sets 

    #create a function called "create_train_test" which split data into train and test set
create_train_test <- function(data, size = 0.8, train = TRUE){
  n_row = nrow(data)
  total_number = size * n_row
  total_index <- 1:total_number
  if(train == TRUE){
    return(data[total_index, ])}
  else{ return(data[-total_index,])}}

# data shuffle
wisc_shuffle_index <- sample(1:nrow(wisc_bc_data))
wisc_bc_data = wisc_bc_data[wisc_shuffle_index, ]
dim(wisc_bc_data)

insurance_shuffle_index <- sample(1:nrow(insurance_data))
insurance_data = insurance_data[insurance_shuffle_index, ]
dim(insurance_data)

#split wisc_bc_data dataset into "wisc_bc_train" and "wisc_bc_val"
wisc_bc_train = create_train_test(wisc_bc_data, size = 0.7, train = TRUE)
wisc_bc_val = create_train_test(wisc_bc_data, size = 0.7, train = FALSE)

#split insurance_data dataset into "insurance_train" and "insurance_val"
insurance_train <- create_train_test(insurance_data, size = 0.7, train = TRUE)
insurance_val <- create_train_test(insurance_data, size = 0.7, train = FALSE)


library(tensorflow)
install_tensorflow(method = "auto")
use_condaenv("r-tensorflow")

# Model 1: Building a neural network with two hidden layers 
model1 <- keras_model_sequential()  
model1 %>%  
  layer_dense(units = 31, activation = "relu", input_shape = ncol(wisc_bc_train)-1) %>%  
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 8, activation = "relu") %>% 
  layer_dense(units = 1) 

# Model 2: Building a neural network with one hidden layer 
model2 <- keras_model_sequential()
model2 %>%  
  layer_dense(units = 6, activation = "relu", input_shape = ncol(insurance_train)-1) %>%  
  layer_dense(units = 3, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid") 


# Compile the models 
model1 %>% compile( 
  optimizer = "adam", 
  loss = "mean_squared_error", 
  metrics = c("mean_absolute_error") 
) 

model2 %>% compile( 
  optimizer = "adam", 
  loss = "binary_crossentropy", 
  metrics = c("accuracy") 
) 


# Train the models 
history1 <- model1 %>% fit( 
  wisc_bc_train, wisc_bc_train, 
  epochs = 100, 
  batch_size = 32, 
  validation_data = list(wisc_bc_val[,-2], wisc_bc_val[,2]) 
) 

history2 <- model2 %>% fit( 
  insurance_train, insurance_train, 
  epochs = 100, 
  batch_size = 32, 
  validation_data = list(insurance_val[,-7], insurance_val[,7]) 
) 



# Evaluate the models on the validation data 
# model1's accuracy model1: for wisc_bc dataset
val_acc1 <- model1 %>% evaluate(wisc_bc_val, wisc_bc_val, verbose = 0) 
# model2's accuracy model2: for insurance dataset
val_acc2 <- model2 %>% evaluate(insurance_val, insurance_val, verbose = 0)

