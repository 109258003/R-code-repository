### Naive Bayes
library(e1071)
train <- read.csv("C:/Users/abab1/Desktop/資料科學/hw7/hw7_train.csv", stringsAsFactors = F)
train$label <- as.factor(train$label)
model <- naiveBayes(formula = label ~ feature1 + feature2 + feature3 + feature4 + feature5 +
               feature6 + feature7 + feature8 + feature9 + feature10 +
               feature11 + feature12 + feature13 + feature14 + feature15 +
               feature16 + feature17 + feature18 + feature19 + feature20, data = train)
resultframe <- data.frame(truth=train$label, pred=predict(model, newdata = train))
tab <- table(resultframe)
train.accuracy <- round(sum(diag(tab))/sum(tab),2)

test <- read.csv("C:/Users/abab1/Desktop/資料科學/hw7/hw7_test.csv")
test.predict <- predict(model, newdata = test)
submit <- data.frame(id=test$id, label=test.predict)
write.csv(submit, file = "C:/Users/abab1/Desktop/資料科學/hw7/naivebayes.csv",
          row.names = F, quote = F)

### Logit
library(class)
train <- read.csv("C:/Users/abab1/Desktop/資料科學/hw7/hw7_train.csv")
train$label <- as.factor(train$label)
y <- "label"
x <- c("feature1","feature2","feature3","feature4","feature5",
       "feature6","feature7","feature8","feature9","feature10",
       "feature11","feature12","feature13","feature14","feature15",
       "feature16","feature17","feature18","feature19","feature20")
fmla <- paste(y, paste(x, collapse="+"), sep="~")

model <- glm(fmla, data = train, family = "binomial")
resultframe <- data.frame(truth=train$label,
                          pred=ifelse(predict(model, newdata = train[,c(3:22)], type = "response") > 0.6,1,-1))
tab <- table(resultframe)
train.accuracy <- round(sum(diag(tab))/sum(tab),2)

test <- read.csv("C:/Users/abab1/Desktop/資料科學/hw7/hw7_test.csv")
test.predict <- ifelse(predict(model, newdata = test[,c(2:21)], type = "response") > 0.6,1,-1)
submit <- data.frame(id=test$id, label=test.predict)
write.csv(submit, file = "C:/Users/abab1/Desktop/資料科學/hw7/logit.csv",
          row.names = F, quote = F)

### RandomForest
library(randomForest)
train <- read.csv("C:/Users/abab1/Desktop/資料科學/hw7/hw7_train.csv")
train$label <- as.factor(train$label)
model <- randomForest(x = train[,c(3:22)], y = train$label, ntree = 1000, 
                      random_state = 0)
resultframe <- data.frame(truth=train$label, pred=predict(model, train[,c(3:22)]))
tab <- table(resultframe)
train.accuracy <- round(sum(diag(tab))/sum(tab),2)

test <- read.csv("C:/Users/abab1/Desktop/資料科學/hw7/hw7_test.csv")
test.predict <- predict(model, newdata = test[,c(2:21)])
submit <- data.frame(id=test$id, label=test.predict)
write.csv(submit, file = "C:/Users/abab1/Desktop/資料科學/hw7/randomforest.csv",
          row.names = F, quote = F)

### SVM
library(e1071)
train <- read.csv("C:/Users/abab1/Desktop/資料科學/hw7/hw7_train.csv", stringsAsFactors = F)
train$label <- as.factor(train$label)
model <- svm(formula = label ~ feature1 + feature2 + feature3 + feature4 + feature5 +
               feature6 + feature7 + feature8 + feature9 + feature10 +
               feature11 + feature12 + feature13 + feature14 + feature15 +
               feature16 + feature17 + feature18 + feature19 + feature20, data = train, kernel = "radial")
resultframe <- data.frame(truth=train$label, pred=predict(model, newdata = train))
tab <- table(resultframe)
train.accuracy <- round(sum(diag(tab))/sum(tab),2)

test <- read.csv("C:/Users/abab1/Desktop/資料科學/hw7/hw7_test.csv")
test.predict <- predict(model, newdata = test)
submit <- data.frame(id=test$id, label=test.predict)
write.csv(submit, file = "C:/Users/abab1/Desktop/資料科學/hw7/radialsvm.csv",
          row.names = F, quote = F)

### xgboost
library(xgboost)
train <- read.csv("C:/Users/abab1/Desktop/資料科學/hw7/hw7_train.csv", stringsAsFactors = F)
train$label <- as.factor(train$label)
true.label <- train$label
train.label <- as.integer(train$label)-1
id <- train$id
train$label = NULL
train$id = NULL
train.matrix <- as.matrix(train)
xgb.train <- xgb.DMatrix(data=train.matrix, label = train.label)

params <- list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softmax",
  eval_metric="mlogloss",
  num_class=as.integer(2)
)

model <- xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train),
  verbose=0
)
prediction <- predict(model,train.matrix,reshape=T)
prediction <- replace(prediction,prediction==0,-1)
resultframe <- data.frame(truth=true.label, pred=prediction)
tab <- table(resultframe)
train.accuracy <- round(sum(diag(tab))/sum(tab),2)

test <- read.csv("C:/Users/abab1/Desktop/資料科學/hw7/hw7_test.csv")
id <- test$id
test$id = NULL
test.matrix <- as.matrix(test)
test.predict <- predict(model,test.matrix,reshape=T)
test.predict <- replace(test.predict,test.predict==0,-1)
submit <- data.frame(id=id, label=test.predict)
write.csv(submit, file = "C:/Users/abab1/Desktop/資料科學/hw7/xgboost.csv",
          row.names = F, quote = F)

resultframe <- as.data.frame(cbind(id=submit$id,label=prediction))
resultframe$label[resultframe$label==0] <- -1

