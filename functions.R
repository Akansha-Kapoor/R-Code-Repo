## Function 1
add2 <- function(x,y){
  x+y
}

## Function 2
above10 <-function(x){
  use<-x>10
  x[use]
}

## Function 3
above <- function(x,n=10){
  use<- x>n
  x[use]
}

## Function 4
columnmean<-function(x,removeNA=TRUE){
  nc<- ncol(x)
  means<-numeric(nc)
  for(i in 1:nc){
    means[i]<-mean(x[,i], na.rm = removeNA)
  }
  means
}

## Function 5
test <- function(x=5,y=5){
  while(x>0){
    z<-y
    while (y>0) {
      print(x*y)
      y<- y-1
    }
    x<-x-1
    y<-z
  }
}

## Function 6
test2 <- function(x=5,y=5){
  v<- vector("numeric",x*y)
  i<-1
  while(x>0){
    z<-y
    while (y>0) {
      v[i] <- x*y
      y<- y-1
      i<- i+1
    }
    x<-x-1
    y<-z
  }
  v
}
