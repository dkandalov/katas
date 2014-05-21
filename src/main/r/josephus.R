jose <-function(s, r,n) {
y <- 0:(r-1)
 for (i in (r+1):n)
  y <- (y + s) %% i
 return(y)
}