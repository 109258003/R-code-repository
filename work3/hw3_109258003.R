args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop(print("USAGE: Rscript hw3_109258003.R --fold k --input Archaeal_tfpssm.csv --output performance.csv"), call.=TRUE)
}

i<-1 
while(i < length(args))
{
  if(args[i] == "--fold"){
    kfold <- args[i+1]
    i<-i+1
  }else if(args[i] == "--input"){
    input <- args[i+1]
    i<-i+1
  }else if(args[i] == "--output"){
    output <- args[i+1]
    i<-i+1
  }else{
    stop(print(paste("Unknown flag", args[i])), call.=TRUE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("fold       :", kfold))
print(paste("input file :", input))
print(paste("output file:", output))

library("class")
df <- read.csv(paste(getwd(),'/',input,sep = ''), header = F)
df <- subset(df, select = -c(V1,V5603))
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
  
  model <- knn(train[,2:5601],train[,2:5601],train[,1],k=4)  #training accuracy(1~kfold-1)
  resultframe <- data.frame(truth=train$V2,pred=model)
  tab <- table(resultframe)
  train.accuracy <- round(sum(diag(tab))/sum(tab),2)
  
  model <- knn(train[,2:5601],valid[,2:5601],train[,1],k=4)  #validation accuracy(1~kfold-1)
  resultframe <- data.frame(truth=valid$V2,pred=model)
  tab <- table(resultframe)
  valid.accuracy <- round(sum(diag(tab))/sum(tab),2)
  
  model <- knn(train[,2:5601],test[,2:5601],train[,1],k=4)   #test accuracy(1~kfold-1)
  resultframe <- data.frame(truth=test$V2,pred=model)
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

model <- knn(train[,2:5601],train[,2:5601],train[,1],k=4)  #training accuracy(kfold)
resultframe <- data.frame(truth=train$V2,pred=model)
tab <- table(resultframe)
train.accuracy <- round(sum(diag(tab))/sum(tab),2)

model <- knn(train[,2:5601],valid[,2:5601],train[,1],k=4)  #validation accuracy(kfold)
resultframe <- data.frame(truth=valid$V2,pred=model)
tab <- table(resultframe)
valid.accuracy <- round(sum(diag(tab))/sum(tab),2)

model <- knn(train[,2:5601],test[,2:5601],train[,1],k=4)   #test accuracy(kfold)
resultframe <- data.frame(truth=test$V2,pred=model)
tab <- table(resultframe)
test.accuracy <- round(sum(diag(tab))/sum(tab),2)

whichfold <- c(whichfold, paste("fold",kfold,sep=''))
training.accuracy <- c(training.accuracy, train.accuracy)
validation.accuracy <- c(validation.accuracy, valid.accuracy)
testing.accuracy <- c(testing.accuracy, test.accuracy)
out_data <- data.frame(set=whichfold, training=training.accuracy, validation=validation.accuracy, test=testing.accuracy)

average.accuracy <- sapply(out_data[,c("training","validation","test")], mean)
out_data <- rbind(out_data,c("ave.", round(average.accuracy,2)))
write.csv(out_data, file=paste(getwd(),'/',output,sep = ''), row.names = F, quote = F)
