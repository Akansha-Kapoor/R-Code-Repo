pollutantmean <- function(directory,pollutant,id=1:332){
  originalwd<-getwd()
  setwd(directory)
  
  meanv<- vector("numeric", length = length(id))
  countv<- vector("numeric", length = length(id))
  
  for (i in seq_along(id)) {
    x<- id[i]
    if(x<10)
    {filename<-paste("00",x,".csv",sep = "")
    } else if (x<100)
    {filename<-paste("0",x,".csv",sep = "")
    } else
    { filename<-paste(x,".csv",sep = "")}
    datap1<-read.csv(filename)[,c(pollutant)]
    datap <- datap1[!is.na(datap1)]
    meanv[i]<-mean(datap)
    countv[i] <- length(datap) 
  }
  j<- is.finite(meanv)
  
  sumv <- meanv[j] * countv[j]
  mean2<- sum(sumv)/sum(countv)
  print(mean2)
  setwd(originalwd)
  
}
