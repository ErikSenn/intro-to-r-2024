# Exercise G: LM Function
# Sample Solution

# Implement you own "lm" function that returns the estimated coefficients for a linear regression model using OLS.
# Test your function by comparing to the "lm" function.
# Hint: Remember the closed form matrix solution to OLS: beta_hat = (X'X)^(-1) X'y.
# The "solve" function might help!

own_ols <- function(X,y){
  solve(t(X)%*%X)%*%t(X)%*%y # (X'X)^(-1) X'y
}

# input data for function as matrix
X = as.matrix( swiss[,c("Education","Agriculture","Examination")])
X = cbind(1,X) #add constant
y = swiss$Fertility

own_ols(X,y)

# compare to lm
lm(Fertility ~ Education + Agriculture  +Examination, data = swiss) 