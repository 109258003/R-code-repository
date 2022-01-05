args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop(print("USAGE: hw6_109258003.R --fold k --train Data/train.csv --test Data/test.csv --report performance.csv --predict predict.csv"), call.=TRUE)
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

library(ROCR)
df <- read.csv(paste(getwd(),'/',training,sep = ''), stringsAsFactors = F)

# dealing with income missing value
mean.df <- df
income.mean <- mean(mean.df[,7], na.rm = TRUE)
na.rows <- is.na(mean.df[,7])
mean.df[na.rows,7] <- income.mean
df <- mean.df

# dealing with dependent missing value
mean.df <- df
dependent.mean <- mean(mean.df[,12], na.rm = TRUE)
na.rows <- is.na(mean.df[,12])
mean.df[na.rows,12] <- dependent.mean
df <- mean.df

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

whichfold      <- c()
training.auc   <- c()
validation.auc <- c()
testing.auc    <- c()

y <- "SeriousDlqin2yrs"
x <- c("RevolvingUtilizationOfUnsecuredLines",
       "age",
       "NumberOfTime30.59DaysPastDueNotWorse",
       "DebtRatio",
       "MonthlyIncome",
       "NumberOfOpenCreditLinesAndLoans",
       "NumberOfTimes90DaysLate",
       "NumberRealEstateLoansOrLines",
       "NumberOfTime60.89DaysPastDueNotWorse",
       "NumberOfDependents")
fmla <- paste(y, paste(x, collapse="+"), sep="~")

for (i in 1:(kfold-1)) {
  train <- df[-cvlist[[c(i,i+1)]],]
  valid <- df[cvlist[[i+1]],]
  test  <- df[cvlist[[i]],]
  model <- glm(fmla, data = train, family = "binomial")
  
  resultframe <- data.frame(truth=train$SeriousDlqin2yrs,
                            pred=predict(model, newdata = train[,c(3,4,5,6,7,8,9,10,11,12)], type = "response"))
  pred_ROCR <- prediction(resultframe$pred, resultframe$truth)
  auc <- performance(pred_ROCR, measure = "auc")
  auc <- auc@y.values[[1]]
  train.auc <- round(auc,2)
  
  resultframe <- data.frame(truth=valid$SeriousDlqin2yrs,
                            pred=predict(model, newdata = valid[,c(3,4,5,6,7,8,9,10,11,12)], type = "response"))
  pred_ROCR <- prediction(resultframe$pred, resultframe$truth)
  auc <- performance(pred_ROCR, measure = "auc")
  auc <- auc@y.values[[1]]
  valid.auc <- round(auc,2)
  
  resultframe <- data.frame(truth=test$SeriousDlqin2yrs,
                            pred=predict(model, newdata = test[,c(3,4,5,6,7,8,9,10,11,12)], type = "response"))
  pred_ROCR <- prediction(resultframe$pred, resultframe$truth)
  auc <- performance(pred_ROCR, measure = "auc")
  auc <- auc@y.values[[1]]
  test.auc <- round(auc,2)
  
  whichfold <- c(whichfold, paste("fold",i,sep=''))
  training.auc <- c(training.auc, train.auc)
  validation.auc <- c(validation.auc, valid.auc)
  testing.auc <- c(testing.auc, test.auc)
}

train <- df[-cvlist[[c(1,kfold)]],]
valid <- df[cvlist[[1]],]
test  <- df[cvlist[[kfold]],]

resultframe <- data.frame(truth=train$SeriousDlqin2yrs,
                          pred=predict(model, newdata = train[,c(3,4,5,6,7,8,9,10,11,12)], type = "response"))
pred_ROCR <- prediction(resultframe$pred, resultframe$truth)
auc <- performance(pred_ROCR, measure = "auc")
auc <- auc@y.values[[1]]
test.auc <- round(auc,2)

resultframe <- data.frame(truth=valid$SeriousDlqin2yrs,
                          pred=predict(model, newdata = valid[,c(3,4,5,6,7,8,9,10,11,12)], type = "response"))
pred_ROCR <- prediction(resultframe$pred, resultframe$truth)
auc <- performance(pred_ROCR, measure = "auc")
auc <- auc@y.values[[1]]
test.auc <- round(auc,2)

resultframe <- data.frame(truth=test$SeriousDlqin2yrs,
                          pred=predict(model, newdata = test[,c(3,4,5,6,7,8,9,10,11,12)], type = "response"))
pred_ROCR <- prediction(resultframe$pred, resultframe$truth)
auc <- performance(pred_ROCR, measure = "auc")
auc <- auc@y.values[[1]]
test.auc <- round(auc,2)

whichfold <- c(whichfold, paste("fold",kfold,sep=''))
training.auc <- c(training.auc, train.auc)
validation.auc <- c(validation.auc, valid.auc)
testing.auc <- c(testing.auc, test.auc)
out_data <- data.frame("set"=whichfold, "training"=training.auc, 
                       "validation"=validation.auc, "testing"=testing.auc, 
                       stringsAsFactors=FALSE)

average.auc <- sapply(out_data[,c("training","validation","testing")], mean)
out_data <- rbind(out_data,c("ave.", round(average.auc,2)))
index <- sapply(out_data[,c("training","validation","testing")], which.max)
out_data <- rbind(out_data,c("highest k", whichfold[index]))

write.csv(out_data, file=paste(getwd(),'/',report,sep = ''), row.names = F, quote = F)

# predict test data
testing <- read.csv(paste(getwd(),'/',testing,sep = ''), stringsAsFactors = F)

# dealing with income missing value
mean.testing <- testing
income.mean <- mean(mean.testing[,7], na.rm = TRUE)
na.rows <- is.na(mean.testing[,7])
mean.testing[na.rows,7] <- income.mean
testing <- mean.testing

# dealing with dependent missing value
mean.testing <- testing
dependent.mean <- mean(mean.testing[,12], na.rm = TRUE)
na.rows <- is.na(mean.testing[,12])
mean.testing[na.rows,12] <- dependent.mean
testing <- mean.testing

model <- glm(fmla, data = df, family = "binomial")
testing.predict <- predict(model, newdata = testing[,c(3,4,5,6,7,8,9,10,11,12)], type = "response")
submit <- data.frame(Id = testing$X, Probability = testing.predict)
write.csv(submit, file=paste(getwd(),'/',predict,sep = ''), row.names = F, quote = F)

print("DONE")




