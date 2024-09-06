# Exercise H: Multiples of 3 and 5
# Sample Solution

N <- 100
# Quick mathematical solution
3 * sum(1:floor(N/3)) + 5 * sum(1:floor(N/5)) - 15 * sum(1:floor(N/15))

# Loop solution
total <- 0
for (i in 1:N) {
  # Check if i is divisible by 3
  if ((i %% 3) == 0) {
    total <- total + i
    # If not, check if it is divisible by 5
  } else if ((i %% 5) == 0) {
    total <- total + i
  }
}
total
