set.seed(21)

# Enter the data
ceo13 <- c(3.2, 3.8, 2.6, 3.5, 7.0, 20.4, 7.5, 3.4, 5.0, 6.0)
ceo12 <- c(23.5, 6.4, 11.1, 3.8, 8.9, 4.8, 23.8, 3.0, 2.9, 3.2)

# Compare variances: in the example ceo12 has larger variance
# Larger variance has to be at numerator

# Calculate observed F-statistic (samples are swapped)
F.obs <- var(ceo12) / var(ceo13)

# Compute sample size (samples are swapped)
n <- length(ceo12)
m <- length(ceo13)

# Combine the centered samples
combined.centered <- c(ceo12 - mean(ceo12), ceo13 - mean(ceo13))

# Bootstrap the F-statistic B = 10,000 times
B <- 10000

F.boot <- replicate(B, {
                    # Single bootstrap sample
                    ceo12.boot <-sample(combined.centered, n, replace = T)
                    ceo13.boot <- sample(combined.centered, m, replace = T)
                    
                    # Return single bootstrap F-statistic
                    var(ceo12.boot) / var(ceo13.boot)
})

# Count number of extreme statistics for two-sided test
extreme <- sum ( (F.boot > F.obs) | (F.boot < 1/F.obs) )

# Compute the p-value
p <- extreme / B

# Print
cat("The bootstrap p-value is:", p)