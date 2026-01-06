set.seed(21)

# Enter the data
ceo13 <- c(3.2, 3.8, 2.6, 3.5, 7.0, 20.4, 7.5, 3.4, 5.0, 6.0)
ceo12 <- c(23.5, 6.4, 11.1, 3.8, 8.9, 4.8, 23.8, 3.0, 2.9, 3.2)

# Calculate observed t-statistic
t.obs <- t.test(ceo13, ceo12, var.equal = T)$statistic

# Compute sample size
n <- length(ceo13)
m <- length(ceo12)

# Combine the samples
combined <- c(ceo13, ceo12)

# Bootstrap the t-statistic B = 10,000 times
B <- 10000

t.boot <- replicate(B, {
                    # Single bootstrap sample
                    ceo13.boot <- sample(combined, n, replace = T)
                    ceo12.boot <- sample(combined, m, replace = T)
                    
                    # Return single bootstrap t-statistic
                    t.test(ceo13.boot, ceo12.boot, var.equal = T)$statistic
                    })

# Count number of extreme statistics for two-sided test
extreme <- sum ( abs( t.boot ) > abs (t.obs) )

# Compute the p-value
p <- extreme / B

# Print
cat("The bootstrap p-value is:", p)