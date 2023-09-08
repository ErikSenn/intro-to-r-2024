## Exercise C: Standard Deviation Function
# Sample Solution

my_sd <- function(x) {
     
     x_bar <- mean(x) # or use my_mean
     sqrd_deviations <- (x - x_bar)^2
     sum_sqrd_dev <- sum(sqrd_deviations) # or use my_sum
     n <- length(x)
     
     sd_x <- sqrt(sum_sqrd_dev/(n-1))
     
     return(sd_x)
}


# test it !!
numeric_vector <- c(1,5,4,3)
my_sd(numeric_vector)

# compare it with the built-in function sum()
sd(numeric_vector)


