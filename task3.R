install.packages("gbm")
install.packages("xgboost")
install.packages("kernlab")
library(kernlab)
data(spam)
install.packages("dplyr")
library(dplyr)
install.packages("e1071")
library(e1071)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("ipred")
library(ipred)
install.packages("tidyverse")
library(tidyverse)
install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))
library(caret)
install.packages("caTools")
library(caTools)
install.packages("Metrics")
library(Metrics)
library(magrittr)
dim(spam)
set.seed(678)

#60% of our values are used for constructing the train_set
inTrain <- createDataPartition(y=spam$type, p=0.6, list=FALSE)

train_set <-spam[inTrain,]
test_set <- spam[-inTrain,]

suppressWarnings(modelFit_glm <- train(type ~., data=train_set, method="glm"))
modelFit_glm

#bagging 
bg=bagging(type ~.,data=train_set,control=rpart.control(maxdepth=5, minsplit=10))
test_set$pred=predict(bg,test_set,type="response")
head(test_set$pred)
rmse(test_set$pred,test_set$y)
   
#Random forest
install.packages("randomForest")
library(randomForest)
install.packages("mboost")
library(mboost)
model=randomForest(type ~.,data=train_set,ntree=400,mtry=10,nodesize=30,importance=T)
plot(model)
#tune rf to identify the best mtry
set.seed(109)
trrf=tuneRF(train_set[,-c(1)],type=train_set$type, mtryStart = 10,stepFactor = 1.5,ntree=400,improve = 0.0001,nodesize=10,trace=T,plot=T,doBest = T,importance=T)
summary(trrf)
trrf
rf1=randomForest(type~.,data=train_set,ntree=500,mtry=244,nodesize=10,importance=T)
plot(rf1)
print(rf1)
test_set$pred=predict(rf1,test_set,type="response")
rmse(test_set$pred,test_set$y)
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(test_set$type,test_set$pred)
rsquare
   
#XGboost
library(caTools)
set.seed(5001)
spl=sample.split(bg,SplitRatio = 0.8)
train_set=subset(bg,spl==TRUE)
test_set=subset(bg,spl==FALSE)
set.seed(1233)
install.packages("xgboost")
library(xgboost)
param_list = list(objective = "reg:linear", eta=0.03,gamma = 2, max_depth=10,subsample=0.8,colsample_bytree=0.5)
xg = xgb.DMatrix(data = as.matrix(train_set[,-c(1)]), label= train_set$type) 
te = xgb.DMatrix(data = as.matrix(test_set[,-c(1)]))
set.seed(112) 
xgbcv = xgb.cv(params = param_list,data = xg,nrounds = 500,nfold = 10,print_every_n = 10,early_stopping_rounds = 10,maximize = F)
xgb_m = xgb.train(data = xg, params = param_list, nrounds = 265)
test_set$pred=predict(xgb_m,te,type="vector")
RMSE(test_set$pred,test$type)
   
#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(test_set$type,test_set$pred)
rsquare
     
#GBM
stackgbm <- function(x, y, params, nfolds = 5L, seed = 42, verbose = TRUE) {
  set.seed(seed)
  nrow_x <- nrow(x)
  index_xgb <- sample(rep_len(1L:nfolds, nrow_x))
  index_lgb <- sample(rep_len(1L:nfolds, nrow_x))
  index_cat <- sample(rep_len(1L:nfolds, nrow_x))
   
  model_xgb <- vector("list", nfolds)
  model_lgb <- vector("list", nfolds)
   
  x_glm <- matrix(NA, nrow = nrow_x, ncol = 3L)
  colnames(x_glm) <- c("xgb", "lgb", "cat")
         
  # xgboost
  pb <- progress_bar$new(
  format = "fitting xgboost model [:bar] :percent in :elapsed", total = nfolds, clear = FALSE, width = 60)
         
  x_xgb <- as.matrix(x)
         
  for (i in 1L:nfolds) {
    if (verbose) pb$tick()
    xtrain <- x_xgb[index_xgb != i, , drop = FALSE]
    ytrain <- y[index_xgb != i]
    xtest <- x_xgb[index_xgb == i, , drop = FALSE]
    ytest <- y[index_xgb == i]
    xtrain <- xgb.DMatrix(xtrain, label = ytrain)
    xtest <- xgb.DMatrix(xtest)
    fit <- xgb.train(
      params = list(objective = "binary:logistic", eval_metric = "auc", max_depth = params$xgb.max_depth, eta = params$xgb.learning_rate),
      data = xtrain,
      nrounds = params$xgb.nrounds
    )
    model_xgb[[i]] <- fit
    x_glm[index_xgb == i, "xgb"] <- predict(fit, xtest)
  }
}         
# lightgbm
pb <- progress_bar$new(
format = "fitting lightgbm model [:bar] :percent in :elapsed", total = nfolds, clear = FALSE, width = 60)
x_lgb <- as.matrix(x)
         
for (i in 1L:nfolds) {
   if (verbose) pb$tick()
   xtrain <- x_lgb[index_lgb != i, , drop = FALSE]
   ytrain <- y[index_lgb != i]
   xtest <- x_lgb[index_lgb == i, , drop = FALSE]
   ytest <- y[index_lgb == i]
       
   fit <- lightgbm(
    data = xtrain,
    label = ytrain,
    objective = "binary",
    learning_rate = params$lgb.learning_rate,
    num_iterations = params$lgb.num_iterations,
    max_depth = params$lgb.max_depth,
    num_leaves = 2^params$lgb.max_depth - 1,
    verbose = -1
    )
           
   model_lgb[[i]] <- fit
   x_glm[index_lgb == i, "lgb"] <- predict(fit, xtest)
}

# The best model is LightGBM
       