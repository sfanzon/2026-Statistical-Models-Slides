# Enter counts and null hypothesis probabilities
counts <- c(13, 17, 9, 17, 18, 26)
null_probs <- rep(1/6, 6)

# Perform goodness-of-fit test
ans <- chisq.test(counts, p = null_probs)

# Print answer
print(ans)