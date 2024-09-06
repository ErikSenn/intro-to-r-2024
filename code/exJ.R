# Exercise J: Fibonacci Sequence
# Sample Solution

# initialize the empty vector
fibonacci_sequence <- c(rep(NA, 30))

# fill in the starting values of the series
fibonacci_sequence[1] <- 0
fibonacci_sequence[2] <- 1

# create the for-loop for the remainding Fibonacci numbers
for (i in 3:30) {
  fibonacci_sequence[i] <- fibonacci_sequence[i-1] + fibonacci_sequence[i-2]
}

fibonacci_sequence