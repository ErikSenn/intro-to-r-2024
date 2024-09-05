# Exercise A: Write a Sum Function
# Sample Solution

my_sum <- 
  function(x){
    
    # initiate total
    total_sum <- 0
    # number of iterations
    n <- length(x)
    # start loop
    for (i in 1:n) {
      total_sum <- total_sum + x[i]
    }
    
    return(total_sum)
    
  }


# test it !!
numeric_vector <- c(1,5,4,3)
my_sum(numeric_vector)

# compare it with the built-in function sum()
sum(numeric_vector)


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


# Exercise E: T-test
# Sample Solution

my_ttest <- function(x, mu) {
  x_bar <- mean(x) # or use my_mean(x) here
  s <-  sd(x) # or use my_sd(x) here
  
  # if no standard deviation: return Nan for test
  if (is.nan(s)){
    return(NaN)
  } else {

  n <- length(x)
  se <- s / sqrt(n) 
  t <- (x_bar - mu) / se
  
  return(t)
  
  }
}


# test it !!
numeric_vector <- c(1,5,4,3)

my_ttest(x = numeric_vector, mu = 3 )

# compare it with the built-in function 
t.test(x = numeric_vector, mu = 3 )




# EX A new
## Aim: Some univariate descriptives that we code ourselves.

# Create a vector of data for testing
vector <- 1:10 # sort for seq(1,n,1)




# Components: functions that take the vector as input and return a descriptive stat on that.

# Create a small dataset.
n <- 100
x1 <- 1:n
x2 <- rnorm(n,0.01,2) # normally distributed with provided mean and standard deviation
x3 <- rep(2, n)
matrix <- cbind(x1,x2,x3)

# compute summary statistics
results <- data.frame(mean = rep(NA, ncol(matrix)), 
                      standard_deviation = rep(NA, ncol(matrix)),
                      standard_error_mean =rep(NA, ncol(matrix)),
                      t_value = rep(NA, ncol(matrix))
                      )

for (col in 1:ncol(matrix)){
  results[col,1] <- my_sum(matrix[,col])/nrow(matrix)
  results[col,2] <- my_sd(matrix[,col])
  results[col,3] <- my_se(matrix[,col])
  results[col,4] <- my_ttest(results[col,1],0)
}

results

