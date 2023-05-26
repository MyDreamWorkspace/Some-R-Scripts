#data load
install.packages("rpart.plot")
library(rpart.plot)
data = read.csv("D://Dev/R programming language/income.csv")
summary(data)
hist(data$educationYears)
#create a function called "create_train_test" which split data into train and test set
create_train_test <- function(data, size = 0.8, train = TRUE){
  n_row = nrow(data)
  total_number = size * n_row
  total_index <- 1:total_number
  if(train == TRUE){
    return(data[total_index, ])}
  else{ return(data[-total_index,])}}

#To shuffle data
shuffle_index <- sample(1:nrow(data))
data = data[shuffle_index, ]

#split data into "train_set" and "test_set"
train_set = create_train_test(data, size = 0.7, train = TRUE)
test_set = create_train_test(data, size = 0.7, train = FALSE)
prop.table(table(data$income))
dim(data)
prop.table(table(train_set$income))
dim(train_set)
prop.table(table(test_set$income))
dim(test_set)

#build decision tree model.
fit <- rpart(income~., train_set, method = 'class')
rpart.plot(fit)

#accuracy calculation function
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, test_set, type = 'class')
  table_mat <- table(test_set$income, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

#accuracy of "fit" model
accuracy_tune(fit)

#improving tree model with control parameters
control <- rpart.control(minsplit = 10, minbucket = round(5 / 3), maxdepth = 3, cp = 0)
tune_fit1 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit1)
control <- rpart.control(minsplit = 50, minbucket = round(5 / 3), maxdepth = 3, cp = 0)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit = 50, minbucket = 10, maxdepth = 20, cp = 0.1)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit = 50, minbucket = 10, maxdepth = 30, cp = 0.1)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit = 50, minbucket = 10, maxdepth = 3, cp = 0.1)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit = 50, minbucket = 10, maxdepth = 3, cp = 0.2)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit = 100, minbucket = 10, maxdepth = 3, cp = 0.1)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit = 50, minbucket = 20, maxdepth = 3, cp = 0.1)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit = 50, minbucket = 20, maxdepth = 10, cp = 0.1)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit = 50, minbucket = 20, maxdepth = 10, cp = 0.11)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit = 50, minbucket = 20, maxdepth = 10, cp = 0.12)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
minsplit = 50
control <- rpart.control(minsplit, minbucket = round(minsplit / 3), cp = 0.01)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit, minbucket = round(minsplit / 3), maxdepth = 30, cp = 0.01)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit, minbucket = round(minsplit / 3), maxdepth = 30, cp = 0.009)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit, minbucket = round(minsplit / 3), maxdepth = 30, cp = 0.001)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit, minbucket = round(minsplit / 3), maxdepth = 30, cp = 0.0001)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit, minbucket = round(minsplit / 3), maxdepth = 30, cp = 0.0005)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit, minbucket = round(minsplit / 3), maxdepth = 30, cp = 0.0004)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit, minbucket = round(minsplit / 3), maxdepth = 30, cp = 0.00055)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
accuracy_tune(tune_fit2)
rpart.plot(tune_fit2)
control <- rpart.control(minsplit, minbucket = round(minsplit / 3), maxdepth = 10, cp = 0.00055)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
rpart.plot(tune_fit2)
accuracy_tune(tune_fit2)
control <- rpart.control(minsplit, minbucket = round(minsplit / 3), maxdepth = 17, cp = 0.00055)
tune_fit2 <- rpart(income~., train_set, method = "class", control = control)
rpart.plot(tune_fit2)
accuracy_tune(tune_fit2)
# the best parameters
control
fancyRpartPlot(tune_fit2)
