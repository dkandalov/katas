fizzbuzz <- function(n) {
    x <- 1:n
    xx <- as.character(x)
    xx[x%%3==0] <- "Fizz"
    xx[x%%5==0] <- "Buzz"
    xx[x%%15==0] <- "FizzBuzz"
    return(xx)
}

fizzbuzz2 <- function(n) {
    x <- 1:n
    ifelse(x %% 15 == 0, 'FizzBuzz',
           ifelse(x %% 5 == 0, 'Buzz',
                  ifelse(x %% 3 == 0, 'Fizz', x)))
}