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
    mean1<- mean(datap)
    meanv[i]<-mean1
    n<- length(datap)
    countv[i] <- n 
    }
  sumv <- meanv * countv
  mean2<- sum(sumv)/sum(countv)
  print(mean2)
  setwd(originalwd)
  
}



pollutantmean2 <- function(directory,pollutant,id=1:332){
  originalwd<-getwd()
  setwd(directory)
  
  meanv<- vector("numeric", length = length(id))
  num <- length(id)
  
  for (i in seq_along(id)) {
    x<- id[i]
    if(x<10)
    {filename<-paste("00",x,".csv",sep = "")
    } else if (x<100)
    {filename<-paste("0",x,".csv",sep = "")
    } else
    { filename<-paste(x,".csv",sep = "")}
    print(filename)
  }
  
  setwd(originalwd)
  
}


pollutantmean3 <- function(directory,pollutant,id=1:332){
  originalwd<-getwd()
  setwd(directory)
  
  meanv<- vector("numeric", length = length(id))
  num <- length(id)
  
  for (i in seq_along(id)) {
    x<- id[i]
    if(x<10)
    {filename<-paste("00",x,".csv",sep = "")
    } else if (x<100)
    {filename<-paste("0",x,".csv",sep = "")
    } else
    { filename<-paste(x,".csv",sep = "")}
    datap<-read.csv(filename)[,c(pollutant)]
    mean1<- mean(datap,na.rm = TRUE)
    meanv[i]<-mean1
  }
  mean2<- mean(meanv)
  print(mean2)
  setwd(originalwd)
  
}



pollutantmean4 <- function(directory,pollutant,id=1:332){
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
    mean1<- mean(datap)
    meanv[i]<-mean1
    n<- length(datap)
    countv[i] <- n 
  }
  sumv <- meanv * countv
  mean2<- sum(sumv)/sum(countv)
  print(mean2)
  setwd(originalwd)
  
}