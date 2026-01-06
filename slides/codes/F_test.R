# Enter Wages data in 2 vectors using function c()

mathematicians <- c(36, 40, 46, 54, 57, 58, 59, 60, 62, 63)
accountants <- c(37, 37, 42, 44, 46, 48, 54, 56, 59, 60, 60, 64, 64)


# Perform two-sided F-test using var.test
# Store result and print

ans <- var.test(accountants, mathematicians)
print(ans)