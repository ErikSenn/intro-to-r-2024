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
