# Required Libraries
library(ggplot2)
library(lmtest)
library(sandwich)

# 1. Load the "swiss" dataset into R, print the last 3 observations of the dataset, 
# and create summary statistics for all variables.
data(swiss)
print(tail(n = 3, swiss))
print(summary(swiss))

# 2. Create new variables "UrbanizationRate" and "FertilityCategory".
# "Urbanization" should be the inverse of "Agriculture".
# "FertilityCategory" is a factor variable, if Fertility is between 0 and 60, 
# it is low, between 60 and 80 it is medium, and between 80 and 100 it is high.
# Hint: you can use the function "cut". Use "?cut" for help.
swiss$UrbanizationRate <- 100 - swiss$Agriculture
swiss$FertilityCategory <- cut(swiss$Fertility, breaks=c(0,60,80,100), labels=c("Low", "Medium", "High"))

# 3. Visualization
# a) Create a histogram of the Fertility variable, fill the bars dark blue
# and change the theme. 
# Hint: use "geom_histogram"
ggplot(swiss, aes(x = Fertility)) +
  geom_histogram(binwidth=5, fill="darkblue", color="black", alpha=0.7) +
  ggtitle("Histogram of Fertility") +
  theme_minimal()

# b) Create a boxplot of the variable "Education".
# Hint: use "geom_boxplot"
ggplot(swiss, aes(y = Education)) +
  geom_boxplot(fill="lightblue") +
  ggtitle("Boxplot of Education")

# c) Create a scatter plot of Education and Fertility and add a regression line.
ggplot(swiss, aes(x = Education, y = Fertility)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Scatter plot of Education vs. Fertility with Regression Line")

# d) Create scatter plots between "Agriculture" and "Examination" for each
# of the three Fertility categories we created earlier.
# Hint: use "facet_wrap"
ggplot(swiss, aes(x = Agriculture, y = Examination)) +
  geom_point() +
  facet_wrap(~ FertilityCategory) +
  ggtitle("Scatter plot of Agriculture vs. Examination across Fertility Categories")

# 4. Linear Regression
# a) Regress "Education", "Agriculture", and "Examination" on "Fertility"
model <- lm(Fertility ~ Education + Agriculture + Examination, data=swiss)
summary(model)

# b) Regress the logarithm of "UrbanizationRate" on "Fertility"
model_log <- lm(Fertility ~ log(UrbanizationRate), data=swiss)
summary(model_log)

# (*)5. Implement you own "lm" function that returns the estimated coefficients for a linear regression model using OLS.
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


# (*)6. Change the standard errors to account for heteroscedasticity (white standard errors).
# Hint: Use the packages "lmtest" and "sandwich".
robust_test <- coeftest(model, vcov = vcovHC(model, type = "HC0"))
print(robust_test)
