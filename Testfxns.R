readinteger <- function()
{ 
  n <- readline(prompt="Enter an integer: ")
  if(!grepl("^[0-9]+$",n))
  {
    return(readinteger())
  }
  return(as.integer(n))
}

#let me try a random number generator

rnum <- function()
{
runif(1)*10
}

#try the one from course

test1 <- function()
{
x <- runif(100)
mean(x)
}
