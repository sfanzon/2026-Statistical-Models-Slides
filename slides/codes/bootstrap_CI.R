# Enter original sample and compute size
x <- c(11.4, 13.1, 14.7, 14.7, 15.0, 15.5, 15.6, 15.9, 16.0, 16.8)
n <- length(x)

# Bootstrap sample mean B = 10,000 times
B <- 10000

xbar.star <- replicate(B, mean( sample(x, n, replace = TRUE) ))

# Compute 95% CI from bootstrap samples
alpha <- 0.05
boot.CI <- quantile(xbar.star, probs = c(alpha/2, 1-alpha/2))

# Compute 95% t-statistic CI
t.stat.CI <- t.test(x)$conf.int

# Print results
cat("Bootstrap Confidence Interval (95%):", boot.CI)
cat("\nt-statistic Confidence Interval (95%):", t.stat.CI)