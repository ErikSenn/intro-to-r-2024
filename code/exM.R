# Exercise M: Working with data using dplyr
# Sample Solution

# Required Libraries
library(dplyr)

# 1. Call data(swiss). Then, use `dplyr` function `mutate` to create new variables 
# within the data frame called UrbanizationRate and FertilityCategory. 
# UrbanizationRate should be the inverse of Agriculture. FertilityCategory is a 
# factor variable: if Fertility is between 0 and 60, it is "low", between 60 and 
# 80 it is "medium", and between 80 and 100 it is "high". Hint: Use `case_when` 
# for categorization.
data(swiss)
swiss <- swiss |> 
  mutate(UrbanizationRate = 100 - Agriculture,
         FertilityCategory = case_when(
           Fertility <= 60 ~ "Low",
           Fertility > 60 & Fertility <= 80 ~ "Medium",
           Fertility > 80 ~ "High"
         ) |> as.factor())

# 2. Filter the dataset to include only observations where Fertility is greater 
# than 70, and arrange the results by Education in descending order.
swiss |> 
  filter(Fertility > 70) |> 
  arrange(desc(Education))

# 3. Group the data by FertilityCategory and calculate the average Education for 
# each category by using `group_by` and `summarize`.
swiss |> 
  group_by(FertilityCategory) |> 
  summarize(AvgEducation = mean(Education, na.rm = TRUE))

# 4. Find the top 3 observations with the highest Fertility values. Display only 
# Fertility, and Education columns for these observations.
swiss |> 
  arrange(desc(Fertility)) |> 
  head(3) |> 
  select(Fertility, Education)

# 5. Create a new variable EducationToFertilityRatio that represents the ratio 
# of Education to Fertility. Show all observations with ratios between 0.1 and 0.2.
swiss |> 
  mutate(EducationToFertilityRatio = Education / Fertility) |> 
  arrange(desc(EducationToFertilityRatio)) |> 
  filter(EducationToFertilityRatio > 0.1 & EducationToFertilityRatio < 0.2)

# 6. Calculate the median Fertility and standard deviation of Education for each 
# FertilityCategory, and then create a new data frame containing these statistics.
swiss_stats <- swiss |> 
  group_by(FertilityCategory) |> 
  summarize(MedianFertility = median(Fertility, na.rm = TRUE),
            SdEducation = sd(Education, na.rm = TRUE))

swiss_stats

# 7. Using the `n_distinct` function, determine the number of distinct values in 
# Education and FertilityCategory.
swiss |> 
  summarize(DistinctEducation = n_distinct(Education),
            DistinctFertilityCategory = n_distinct(FertilityCategory))
