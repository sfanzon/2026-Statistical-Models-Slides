# Enter counts and null hypothesis probabilities
counts <- c(32, 15, 9, 4)
null.p <- c(0.5, 0.3, 0.15, 0.05)

# Compute total counts
m <- sum(counts)

# Compute expected counts
exp.counts <- m * null.p

# Compute the observed chi-squared statistic
obs.chi.squared <- sum( (counts - exp.counts)^2 / exp.counts )

# Number of Monte Carlo simulations
N <- 100000

# Initialize counter for extreme statistics
number.extreme.stats <- 0

# Perform Monte Carlo simulations

for (i in 1:N) {
  # Simulate multinomial counts under null hypothesis
  sim.counts <- rmultinom(1, m, null.p)
  
  # Compute chi-squared statistic for the simulated counts
  sim.chi.squared <- sum( (sim.counts - exp.counts )^2 / exp.counts)
  
  # If simulated chi-squared statistic is extreme, increase counter
  if (sim.chi.squared >= obs.chi.squared) {
    number.extreme.stats <- number.extreme.stats + 1
  }
}

# Compute Monte Carlo p-value
monte.carlo.p.value <- number.extreme.stats / N

# Perform chi-squared test using built-in R function
ans <- chisq.test(counts, p = null.p, simulate.p.value = T)

# Extract p-value from chi-squared test result
R.p.value <- ans$p.value

# Print p-values for comparison
cat("\nOur Monte Carlo p-value is:", monte.carlo.p.value)
cat("\nR Monte Carlo p-value is:", R.p.value)