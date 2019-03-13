complete <- function(directory, id = 1:332) {
  
  originalwd<-getwd()
  setwd(directory)
  
  l <- length(id)
  pdata <- data.frame(id = numeric(length = l), nobs = numeric(length = l))
  for (i in seq_along(id)) {
    x<- id[i]
    pdata[i,1] <- x
    if(x<10)
    {filename<-paste("00",x,".csv",sep = "")
    } else if (x<100)
    {filename<-paste("0",x,".csv",sep = "")
    } else
    { filename<-paste(x,".csv",sep = "")}
    dfile <- read.csv(filename)
    dcfile <- dfile[complete.cases(dfile),1]
    len <- length(dcfile)
    pdata[i,2] <- len
    
  }
  print(pdata)
  setwd(originalwd)
}