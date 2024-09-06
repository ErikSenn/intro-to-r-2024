# Exercise E: T-test
# Sample Solution

my_ttest <- function(x, mu) {
     x_bar <- mean(x) # or use my_mean(x) here
     s <-  sd(x) # or use my_sd(x) here
     n <- length(x)
     se <- s / sqrt(n) 
     
     t <- (x_bar - mu) / se
     
     return(t)
}


# test it !!
numeric_vector <- c(1,5,4,3)

my_ttest(x = numeric_vector, mu = 3 )

# compare it with the built-in function 
t.test(x = numeric_vector, mu = 3 )
