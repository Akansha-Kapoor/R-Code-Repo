rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  datafile <- read.csv("outcome-of-care-measures.csv")
  
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
  dframe <- data.frame(hospital = 0, state = names(split1))
  i <- length(split1)
  while(i>0){
    split2 <- split1[[i]]
    ranking1 <- order(as.numeric(as.character(split2[ ,outcomecol])),as.character(split2[ ,2]))
    ranking <- ranking1[!is.na(ranking1)]
    rhos <- as.character(split2[ranking,2])
    
    if(num=="best"){
      rank <- which.min(as.numeric(as.character(split2[ ,outcomecol])))
      r <- as.character(split2[rank,2])
    }else if (num == "worst"){
      rank <- which.max(as.numeric(as.character(split2[ ,outcomecol])))
      r <- as.character(split2[rank,2])
    }else {
      rank <- as.numeric(num)
      r <- (rhos[rank])
    }
    if(rank>length(rhos)){
      r <- NA
    }
    dframe[i,1] <- r
    i <- i-1
  }
  dframe
  
  
}
