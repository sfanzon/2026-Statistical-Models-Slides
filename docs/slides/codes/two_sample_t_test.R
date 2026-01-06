# Enter Wages data in 2 vectors using function c()
mathematicians <- c(36, 40, 46, 54, 57, 58, 59, 60, 62, 63)
accountants <- c(37, 37, 42, 44, 46, 48, 54, 56, 59, 60, 60, 64, 64)


# Perform two-sample t-test with null hypothesis mu_X = mu_Y
# Specify that populations have same variance
# Store result of t.test in answer
answer <- t.test(mathematicians, accountants, var.equal = TRUE)

# Print answer
print(answer)