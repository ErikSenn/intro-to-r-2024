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
swiss$UrbanizationRate <- 100 - swiss$Agriculture
swiss$FertilityCategory <- cut(swiss$Fertility, breaks=c(0,60,80,100), labels=c("Low", "Medium", "High"))

# 3. Visualization
# a) Histogram of the Fertility variable
ggplot(swiss, aes(x = Fertility)) +
  geom_histogram(binwidth=5, fill="darkblue", color="black", alpha=0.7) +
  ggtitle("Histogram of Fertility") +
  theme_minimal()

# b) Boxplot of the variable "Education"
ggplot(swiss, aes(y = Education)) +
  geom_boxplot(fill="lightblue") +
  ggtitle("Boxplot of Education")

# c) Scatter plot of Education and Fertility with a regression line
ggplot(swiss, aes(x = Education, y = Fertility)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Scatter plot of Education vs. Fertility with Regression Line")

# d) Scatter plots for "Agriculture" vs "Examination" across Fertility Categories
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

# 5. Implement Your Own OLS function

own_ols <- function(X,y){
  solve(t(X)%*%X)%*%t(X)%*%y # (X'X)^(-1) X'y
}

# input data as matrix
X = as.matrix( swiss[,c("Agriculture","Examination")])
X = cbind(1,X) #add constant
y = swiss$Fertility

own_ols(X,y)

# compare to lm
lm(Fertility ~ Agriculture  +Examination, data = swiss) 


# 6. Use heteroscedasticity-robust standard errors
robust_test <- coeftest(model, vcov = vcovHC(model, type = "HC0"))
print(robust_test)
