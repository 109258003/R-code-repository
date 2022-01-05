args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop(print("USAGE: hw5_109258003.R --fold k --train Titanic_Data/train.csv --test Titanic_Data/test.csv --report performance.csv --predict predict.csv"), call.=TRUE)
}

i<-1 
while(i < length(args))
{
  if(args[i] == "--fold"){
    kfold <- args[i+1]
    i<-i+1
  }else if(args[i] == "--train"){
    training <- args[i+1]
    i<-i+1
  }else if(args[i] == "--test"){
    testing <- args[i+1]
    i<-i+1
  }else if(args[i] == "--report"){
    report <- args[i+1]
    i<-i+1
  }else if(args[i] == "--predict"){
    predict <- args[i+1]
    i<-i+1
  }else{
    stop(print(paste("Unknown flag", args[i])), call.=TRUE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("fold       :"      ,kfold))
print(paste("train file :"      ,training))
print(paste("test file:"        ,testing))
print(paste("performance file :",report))
print(paste("predict file :"    ,predict))

library("randomForest")
df <- read.csv(paste(getwd(),'/',training,sep = ''), stringsAsFactors = F)
df$Survived <- as.factor(df$Survived)

# dealing with Age missing value
mean.df <- df
age.mean <- mean(mean.df[,6], na.rm = TRUE)
na.rows <- is.na(mean.df[,6])
mean.df[na.rows,6] <- age.mean
df <- mean.df

# transfer Sex into numeric
df[df == "male"] <- 1
df[df == "female"] <- 0

# kfold <- 5
kfold <- as.numeric(kfold)
CVgroup <- function(k, datasize, seed) {
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k, ceiling(datasize/k))[1:datasize]
  temp <- sample(n, datasize)
  x <- 1:k
  dataseq <- 1:datasize 
  cvlist <- lapply(x, function(x) {dataseq[temp==x]})
  return(cvlist)
}
cvlist <- CVgroup(k = kfold, datasize = nrow(df), seed = 123)

whichfold           <- c()
training.accuracy   <- c()
validation.accuracy <- c()
testing.accuracy    <- c()

for (i in 1:(kfold-1)) {
  train <- df[-cvlist[[c(i,i+1)]],]
  valid <- df[cvlist[[i+1]],]
  test  <- df[cvlist[[i]],]
  
  model <- randomForest(x = train[c(3,5,6,7,8,10)], y = train$Survived, ntree = 500, 
                        random_state = 0)
  resultframe <- data.frame(truth=train$Survived,
                            pred=predict(model, train[c(3,5,6,7,8,10)]))
  tab <- table(resultframe)
  train.accuracy <- round(sum(diag(tab))/sum(tab),2)
  
  model <- randomForest(x = train[c(3,5,6,7,8,10)], y = train$Survived, ntree = 500, 
                        random_state = 0)
  resultframe <- data.frame(truth=valid$Survived,
                            pred=predict(model, valid[c(3,5,6,7,8,10)]))
  tab <- table(resultframe)
  valid.accuracy <- round(sum(diag(tab))/sum(tab),2)
  
  model <- randomForest(x = train[c(3,5,6,7,8,10)], y = train$Survived, ntree = 500, 
                        random_state = 0)
  resultframe <- data.frame(truth=test$Survived,
                            pred=predict(model, test[c(3,5,6,7,8,10)]))
  tab <- table(resultframe)
  test.accuracy <- round(sum(diag(tab))/sum(tab),2)
  
  whichfold <- c(whichfold, paste("fold",i,sep=''))
  training.accuracy <- c(training.accuracy, train.accuracy)
  validation.accuracy <- c(validation.accuracy, valid.accuracy)
  testing.accuracy <- c(testing.accuracy, test.accuracy)
}

train <- df[-cvlist[[c(1,kfold)]],]
valid <- df[cvlist[[1]],]
test  <- df[cvlist[[kfold]],]

model <- randomForest(x = train[c(3,5,6,7,8,10)], y = train$Survived, ntree = 500, 
                      random_state = 0)
resultframe <- data.frame(truth=train$Survived,
                          pred=predict(model, train[c(3,5,6,7,8,10)]))
tab <- table(resultframe)
train.accuracy <- round(sum(diag(tab))/sum(tab),2)

model <- randomForest(x = train[c(3,5,6,7,8,10)], y = train$Survived, ntree = 500, 
                      random_state = 0)
resultframe <- data.frame(truth=valid$Survived,
                          pred=predict(model, valid[c(3,5,6,7,8,10)]))
tab <- table(resultframe)
valid.accuracy <- round(sum(diag(tab))/sum(tab),2)

model <- randomForest(x = train[c(3,5,6,7,8,10)], y = train$Survived, ntree = 500, 
                      random_state = 0)
resultframe <- data.frame(truth=test$Survived,
                          pred=predict(model, test[c(3,5,6,7,8,10)]))
tab <- table(resultframe)
test.accuracy <- round(sum(diag(tab))/sum(tab),2)

whichfold <- c(whichfold, paste("fold",kfold,sep=''))
training.accuracy <- c(training.accuracy, train.accuracy)
validation.accuracy <- c(validation.accuracy, valid.accuracy)
testing.accuracy <- c(testing.accuracy, test.accuracy)
out_data <- data.frame("set"=whichfold, "training"=training.accuracy, 
                       "validation"=validation.accuracy, "testing"=testing.accuracy, 
                       stringsAsFactors=FALSE)

average.accuracy <- sapply(out_data[,c("training","validation","testing")], mean)
out_data <- rbind(out_data,c("ave.", round(average.accuracy,2)))
write.csv(out_data, file=paste(getwd(),'/',report,sep = ''), row.names = F, quote = F)

# predict test data
testing <- read.csv(paste(getwd(),'/',testing,sep = ''), stringsAsFactors = F)

# dealing with Age missing value
mean.testing <- testing
age.mean <- mean(mean.testing[,5], na.rm = TRUE)
na.rows <- is.na(mean.testing[,5])
mean.testing[na.rows,5] <- age.mean
testing <- mean.testing

# dealing with Fare missing value
mean.testing <- testing
fare.mean <- mean(mean.testing[,9], na.rm = TRUE)
na.rows <- is.na(mean.testing[,9])
mean.testing[na.rows,9] <- fare.mean
testing <- mean.testing

# transfer Sex into numeric
testing[testing == "male"] <- 1
testing[testing == "female"] <- 0

model <- randomForest(x = df[c(3,5,6,7,8,10)], y = df$Survived, ntree = 500, 
                      random_state = 0)
testing.predict <- predict(model, testing[c(2,4,5,6,7,9)])
submit <- data.frame(PassengerId = testing$PassengerId, Survived = testing.predict)
write.csv(submit, file=paste(getwd(),'/',predict,sep = ''), row.names = F, quote = F)

print("DONE")




