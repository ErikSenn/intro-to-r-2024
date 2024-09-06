# Exercise L: Working with data
# Sample Solution

# Required Libraries
library(ggplot2)

# 1. Load the dataset swiss.csv or call data(swiss). 
data(swiss)

# 2. Print the last 3 observations of the dataset, and create summary statistics 
# for all variables.
print(tail(n = 3, swiss))
print(summary(swiss))

# 3. Create new variables "UrbanizationRate" and "FertilityCategory".
# "Urbanization" should be the inverse of "Agriculture".
# "FertilityCategory" is a factor variable, if Fertility is between 0 and 60, 
# it is low, between 60 and 80 it is medium, and between 80 and 100 it is high.
# Hint: you can use the function "cut". Use "?cut" for help.
swiss$UrbanizationRate <- 100 - swiss$Agriculture
swiss$FertilityCategory <- cut(swiss$Fertility, breaks=c(0, 60, 80, 100), labels=c("Low", "Medium", "High"))

# 4. Visualization
# Create a histogram of the Fertility variable, fill the bars dark blue
# and change the theme. 
# Hint: use "geom_histogram"
ggplot(swiss, aes(x = Fertility)) +
  geom_histogram(binwidth=5, fill="darkblue", color="black", alpha=0.7) +
  ggtitle("Histogram of Fertility") +
  theme_minimal()

# 5. Create a boxplot of the variable "Education".
# Hint: use "geom_boxplot"
ggplot(swiss, aes(y = Education)) +
  geom_boxplot(fill="lightblue") +
  ggtitle("Boxplot of Education")

# 6. Create a scatter plot of Education and Fertility and add a regression line.
ggplot(swiss, aes(x = Education, y = Fertility)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Scatter plot of Education vs. Fertility with Regression Line")

# 7. Create scatter plots between "Agriculture" and "Examination" for each
# of the three Fertility categories we created earlier.
# Hint: use "facet_wrap"
ggplot(swiss, aes(x = Agriculture, y = Examination)) +
  geom_point() +
  facet_wrap(~ FertilityCategory) +
  ggtitle("Scatter plot of Agriculture vs. Examination across Fertility Categories")
