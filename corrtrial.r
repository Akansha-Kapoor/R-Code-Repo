
corr <- function(directory, threshold = 0) {
 ## This is stll not 100% and gives warnings and errors 
  t <- threshold[1]
  pdata <- complete(directory)
  id1 <- numeric(length = nrow(pdata))
    for (i in pdata) {
    a<- pdata[i,2]
    if(a > t){
      id1[i]<- pdata[i,1]
    }else {
      id1[i] <- NA
    }
  }
  
  originalwd<-getwd()
  setwd(directory)
  
  id <- id1[!is.na(id1)]
  allcor <- vector("numeric",length = length(id))
  for (i in seq_along(id)) {
    x<- id[i]
    if(x<10)
    {filename<-paste("00",x,".csv",sep = "")
    } else if (x<100)
    {filename<-paste("0",x,".csv",sep = "")
    } else
    { filename<-paste(x,".csv",sep = "")}
    dfile <- read.csv(filename)
    x <- dfile[complete.cases(dfile),"nitrate"]
    y <- dfile[complete.cases(dfile),"sulfate"]
    allcor[i] <- cor(x,y)
  }
  setwd(originalwd)
  return(allcor)
  
}