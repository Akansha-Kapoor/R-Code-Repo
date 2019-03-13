rankhospital <- function(state, outcome, num = "best") {
  
  
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
  ranking1 <- order(as.numeric(as.character(split2[ ,outcomecol])),as.character(split2[ ,2]))
  ranking <- ranking1[!is.na(ranking1)]
  rhos <- as.character(split2[ranking,2])
  
  if(num=="best"){
    rank <- which.min(as.numeric(as.character(split2[ ,outcomecol])))
    return(as.character(split2[rank,2]))
  }else if (num == "worst"){
    rank <- which.max(as.numeric(as.character(split2[ ,outcomecol])))
    return(as.character(split2[rank,2]))
  }else {
    rank <- as.numeric(num)
    return(rhos[rank])
  }
  if(rank>length(rhos)){
    return(NA)
  }
}
