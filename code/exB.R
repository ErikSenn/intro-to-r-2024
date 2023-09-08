# Exercise B: Control Statements
# Sample Solution

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


# test it !!
numeric_vector <- c(1,5,4,3)
char_numbers <- c("1", "5", "4", "3")
char_vector <- c("a", "b", "c")

my_sum(numeric_vector)
my_sum(char_numbers)
my_sum(char_vector)

# compare it with the built-in function sum()
sum(numeric_vector)
sum(char_numbers)
sum(char_vector)

