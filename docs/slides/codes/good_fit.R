# Enter counts and null hypothesis probabilities
counts <- c(2162, 738, 228, 2876)
null.p <- c(1/3, 1/8, 1/24, 1/2)

# Perform goodness-of-fit test
# Store the output and print

ans <- chisq.test(counts, p = null.p)
print(ans)