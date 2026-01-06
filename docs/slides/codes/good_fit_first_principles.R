# Enter counts and null hypothesis probabilities
counts <- c(35, 40 , 25)
null.p <- c(0.35, 0.35, 0.3)

# Compute total counts
m <- sum(counts)

# Compute degrees of freedom
degrees <- length(counts) - 1

# Compute expected counts
exp.counts <- m * null.p

# Print expected counts with a message
cat("The expected counts are:", exp.counts)

# Check if the expected counts are larger than 5
if (all(exp.counts >= 5)) {
  cat("Expected counts are larger than 5.", 
      "\nThe chi-squared approximation is valid!")
} else {
  cat("Warning, low expected counts.",
      "\nMonte Carlo simulation must be used.")
}

# Compute chi-squared statistic
chi.squared <- sum( (counts - exp.counts)^2 / exp.counts )

# Compute p-value
p_value <- 1 - pchisq(chi.squared, df = degrees)

# Print p-value
cat("The p-value is:", p_value)