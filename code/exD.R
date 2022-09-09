# Exercise D: Standard Error Function
# Sample Solution

my_se <- function(x) {
     s <- my_sd(x) # or use sd here
     n <- length(x)
     se <- s / sqrt(n)
     
     return(se)
}


# test it !!
numeric_vector <- c(1,5,4,3)
my_se(numeric_vector)


