
install.packages("rpart.plot")
install.packages("caret")
install.packages("caTools")
install.packages("ISLR")
install.packages("tree")

libs = c("ISLR","tree")
lapply(libs, require, character.only=TRUE)
library(caret)
library(caTools)
library(rpart.plot)
#data load
data = read.csv("D://Dev/R programming language/bank_data.csv")
summary(data)

#preprocess data
data$job = as.factor(data$job)
data$marital = as.factor(data$marital)
data$education = as.factor(data$education)
data$default = as.factor(data$default)
data$housing = as.factor(data$housing)
data$loan = as.factor(data$loan)
data$contact = as.factor(data$contact)
data$month = as.factor(data$month)
data$poutcome = as.factor(data$poutcome)
data$y = as.factor(data$y)
#To shuffle data
shuffle_index <- sample(1:nrow(data))
data = data[shuffle_index, ]

#split data into "train_set" and "test_set"
split_set = sample.split(data, 0.7)
train_set = subset(data, split_set == TRUE)
test_set = subset(data, split_set == FALSE)

prop.table(table(data$income))
dim(data)
prop.table(table(train_set$income))
dim(train_set)
prop.table(table(test_set$income))
dim(test_set)

#build decision cv tree 
set.seed(1234)
tree_model = tree(y~., train_set)
plot(tree_model)
text(tree_model, pretty=0)
#accuracy calculation function
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, test_set, type = 'class')
  table_mat <- table(test_set$y, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

#accuracy of "fit" model without pruning
accuracy_tune(tree_model)

set.seed(3)

cv_tree = cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)
# [1] "size" "dev" "k" "method"
# size of prune trees
# dev - deviance or cv error rate

# plot the size and deviance to see where error is lowest

plot(cv_tree$size, cv_tree$dev, type="b",
     xlab='Tree Size',
     ylab='Error Rate',
     main = 'Cross Validation: Error Vs Size')
#with pruning---------------------------
pruned_model = prune.misclass(tree_model, best = 2)
accuracy_tune(pruned_model)
plot(pruned_model)
text(pruned_model, pretty = 0)
