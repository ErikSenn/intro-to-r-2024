# Exercise H: Accounting for Heteroscedasticity
# Sample Solution

# Change the standard errors to account for heteroscedasticity (white standard errors).
# Hint: Use the packages "lmtest" and "sandwich".

robust_test <- coeftest(model, vcov = vcovHC(model, type = "HC0"))
print(robust_test)