# Exercise K: Matrix Fibonacci
# Sample Solution

library(expm)

fibo <- function(n) {
  # Define the Fibonacci matrix
  Fmat <- matrix(c(1, 1, 1, 0), nrow = 2)
  # Raise the matrix Fmat to the nth power
  res <- Fmat %^% (n-1)
  return(res[1]) # Return the first element only
}
