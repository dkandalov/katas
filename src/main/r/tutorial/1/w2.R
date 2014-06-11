cube <- function(x, n) {
    x^3
}

# > x <- 1:10
# > if(x > 5) {
# +         x <- 0
# + }
# Warning message:
# In if (x > 5) { :
#   the condition has length > 1 and only the first element will be used

f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}

# x <- 5
# y <- if(x < 3) {
#         NA
# } else {
#         10
# }


