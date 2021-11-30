args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_109258003.R --input input1.csv --output output1.csv", call.=FALSE)
}

i<-1 
while(i < length(args))
{
  if(args[i] == "--input"){
    input <- args[i+1]
    i<-i+1
  }else if(args[i] == "--output"){
    output <- args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("input file :", input))
print(paste("output file:", output))

name <- gsub(".csv", "", basename(input))
x <- read.csv(paste(getwd(),'/',input,sep = ''), header=T,sep=",")
if (sum(is.na(x)) != 0) {stop(print('input file has missing value!'))
} else {write.csv(data.frame(set=name, weight=round(max(x$weight),2),
                   height=round(max(x$height),2)), paste(getwd(),'/',output,sep = ''), row.names = F, quote = F)}
