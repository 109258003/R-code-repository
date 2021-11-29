# query function
query_func<-function(target, i, badthre)
{
  if(target == "bad") {
    cM <- table(truth=i$reference, prediction=i$pred.score>badthre)
    recall <- cM[1,2]/(cM[1,2]+cM[1,1])
    specificity <- cM[2,1]/(cM[2,1]+cM[2,2])
    precision <- cM[1,2]/(cM[1,2]+cM[2,2])
    F1 <- (2*precision*recall)/(precision+recall)
    logLikelihood.model <- sum(ifelse(i$reference=="bad",log(i$pred.score),log(1-i$pred.score)))
    pNull <- sum(ifelse(i$reference=="bad",1,0))/dim(i)[[1]]
    logLikelihood.nullmodel <- sum(ifelse(i$reference=="bad",1,0))*log(pNull) + sum(ifelse(i$reference=="bad",0,1))*log(1-pNull)
    return(data.frame(sensitivity=round(recall,2), 
                      specificity=round(specificity,2),
                      F1=round(F1,2),
                      logLikelihood=round(logLikelihood.model,2),
                      pseudoRsquared=round(1-(logLikelihood.model/logLikelihood.nullmodel),2)))
  }
  else if (target == "good") {
    cM <- table(truth=i$reference, prediction=i$pred.score>badthre)
    recall <- cM[2,1]/(cM[2,1]+cM[2,2])
    specificity <- cM[1,2]/(cM[1,2]+cM[1,1])
    precision <- cM[2,1]/(cM[2,1]+cM[1,1])
    F1 <- (2*precision*recall)/(precision+recall)
    logLikelihood.model <- sum(ifelse(i$reference=="bad",log(i$pred.score),log(1-i$pred.score)))
    pNull <- sum(ifelse(i$reference=="bad",1,0))/dim(i)[[1]]
    logLikelihood.nullmodel <- sum(ifelse(i$reference=="bad",1,0))*log(pNull) + sum(ifelse(i$reference=="bad",0,1))*log(1-pNull)
    return(data.frame(sensitivity=round(recall,2), 
                      specificity=round(specificity,2),
                      F1=round(F1,2),
                      logLikelihood=round(logLikelihood.model,2),
                      pseudoRsquared=round(1-(logLikelihood.model/logLikelihood.nullmodel),2)))
  } else {
    stop(paste("ERROR: unknown query function", target))
  }
}

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_109258003.R --target bad/good --badthre <threshold> --input meth1 meth2 ... methx --output result.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    target <- args[i+1]
    i<-i+1
  }else if(args[i] == "--badthre"){
    badthre <- args[i+1]
      i<-i+1
  }else if(args[i] == "--input"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

ifelse(file.exists(files) == FALSE, stop(print("input file does not exist!")), 
       print("input file exists!"))
print("PROCESS")
print(paste("target     :", target))
print(paste("badthre    :", badthre))
print(paste("input file :", files))
print(paste("output file:", out_f))

# read files
names <- c()
sensitivitys <- c()
specificitys <- c()
F1s <- c()
logLikelihoods <- c()
pseudoRsquareds <- c()
for(file in files)
{
  name <- gsub(".csv", "", basename(file))
  d <- read.csv(paste(getwd(),'/',file,sep = ''), header=T,sep=",")
  sensitivitys    <- c(sensitivitys,    query_func(target, d, badthre)[1,1])
  specificitys    <- c(specificitys,    query_func(target, d, badthre)[1,2])
  F1s             <- c(F1s,             query_func(target, d, badthre)[1,3])
  logLikelihoods  <- c(logLikelihoods,  query_func(target, d, badthre)[1,4])
  pseudoRsquareds <- c(pseudoRsquareds, query_func(target, d, badthre)[1,5])
  names           <- c(names,           name)
}
out_data <- data.frame(method=names, sensitivity=sensitivitys, specificity=specificitys, F1=F1s, logLikelihood=logLikelihoods, pseudoRsquared=pseudoRsquareds, stringsAsFactors = F)
index <- sapply(out_data[,c("sensitivity","specificity","F1","logLikelihood","pseudoRsquared")], which.max)

# output file
out_data <- rbind(out_data,c("max", names[index]))
write.csv(out_data, file=paste(getwd(),'/',out_f,sep = ''), row.names = F, quote = F)
