# Enter Wages data in 2 vectors using function c()
mathematicians <- c(36, 40, 46, 54, 57, 58, 59, 60, 62, 63)
accountants <- c(37, 37, 42, 44, 46, 48, 54, 56, 59, 60, 60, 64, 64)

# Check which variance is higher
cat("\n Variance of accountants is", var(accountants))
cat("\n Variance of mathematicians is", var(mathematicians))

# Calculate sample sizes
n <- length(mathematicians)
m <- length(accountants)

# Compute F-statistics
# Numerator is population with largest variance
F <- var(accountants)/var(mathematicians)

# Compute p-value
p_value <- 2 - 2 * pf(F, df1 = m - 1, df2 = n - 1)

# Print p-value
cat("\n The p-value for two-sided F-test is", p_value)