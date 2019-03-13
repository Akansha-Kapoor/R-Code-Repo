
best <- function(state,outcome){
  
  datafile <- read.csv("outcome-of-care-measures.csv")
  
  if(!any(state==datafile$State)){
    stop("Invalid State")
  }
  
  if(outcome == "heart attack"){
    outcomecol <- 11
  } else if (outcome == "heart failure"){
    outcomecol <- 17
  }else if (outcome == "pneumonia"){
    outcomecol <- 23
  }else {
    stop("Invalid Outcome")
  }
  
  split1 <- split(datafile, datafile$State)
  split2 <- split1[[state]]
  hosrow <-  which.min(as.numeric(as.character(split2[ ,outcomecol])))
  ## setup value for tie - seems correct right now
  as.character(split2[hosrow,2])
}