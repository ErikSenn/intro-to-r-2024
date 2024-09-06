# Exercise I: Prime numbers
# Sample Solution

# Define a function to see if a number is prime
is_prime <- function(x) {
  if (x == 1) {
    return(FALSE)
  } else if (x == 2) {
    return(TRUE)
  } else {
    for (i in 2:floor(sqrt(x))) {
      if ((x %% i) == 0) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

get_prime_numbers_up_to <- function(n) {
  primes <- c()
  for (i in 1:n) {
    if (is_prime(i)) {
      primes <- append(primes, i)
    }
  }
  return(primes)
}