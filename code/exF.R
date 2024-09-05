# Exercise F: Descriptive Table (*)

# Previous functions


my_sum <- 
  function(x){
    
    if (!(class(x)=="numeric" | class(x)=="integer")) { # or: !is.numeric(x)
      
      warning("Wrong input! This function only accepts numeric or integer values!")
      
      x <- as.numeric(x)
    }
    
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

my_sd <- function(x) {
  
  x_bar <- mean(x) # or use my_mean
  sqrd_deviations <- (x - x_bar)^2
  sum_sqrd_dev <- sum(sqrd_deviations) # or use my_sum
  n <- length(x)
  
  sd_x <- sqrt(sum_sqrd_dev/(n-1))
  
  return(sd_x)
}

my_se <- function(x) {
  s <- my_sd(x) # or use sd here
  n <- length(x)
  se <- s / sqrt(n)
  
  return(se)
}

my_ttest <- function(x, mu) {
  n <- length(x)
  x_bar <- my_sum(x)/n # or use my_mean(x) here
  s <-  my_sd(x) # or use my_sd(x) here
  se <- my_se(x) 
  
  t <- (x_bar - mu) / se
  
  return(t)
}

# 1) Manually define a small dataset (loading data we will do in PART III)
## x1 is a constant (e.g. use rep())
## x2 is a vector 1,2,3, ..., n (e.g. use seq())
## x3 is generated from a uniform distribution from -1 to 1. (e.g. use runif)

## combine the dataset in a matrix or tibble/dataframe (e.g. use cbind)
n <- 100 # n
x1 <- rep(1, n) # constant
x2 <- 1:n # short for seq(from=1, to=n, by = 1) which gives c(1,2,3,..., n)
x3 <- runif(n,-1,1) # uniformly distributed with provided mean and standard deviation
matrix <-  cbind(x1,x2,x3) # combine vectors to matrix

# 2) Compute the summary values for each variable in the dataset and store them in a table with x'es as rows and summary statistic as columns.
# Use a loop (or function) to avoid repeating similar codes!
results <- data.frame(mean = rep(NA, ncol(matrix)), 
                      standard_deviation = rep(NA, ncol(matrix)),
                      standard_error_mean =rep(NA, ncol(matrix)),
                      t_value = rep(NA, ncol(matrix))
                      )
rownames(results) <- c("x1","x2","x3")
for (col in 1:ncol(matrix)){
  results[col,1] <- my_sum(matrix[,col])/length(matrix[,col])
  results[col,2] <- my_sd(matrix[,col])
  results[col,3] <- my_se(matrix[,col])
  results[col,4] <- my_ttest(matrix[,col],0)
}

print(results)

