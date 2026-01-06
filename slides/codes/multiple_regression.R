# Store vector y
y <- c(3.1, 1.9, 1.7, 1.6, 3.2, 2.7, 2.6, 2.9, 4.7, 3.8)

# Store design matrix Z as long vector
Z <- c(1, 113, 1950,
       1, 123, 1951,
       1, 127, 1952,
       1, 138, 1953,
       1, 130, 1954,
       1, 146, 1955,
       1, 151, 1956,
       1, 152, 1957,
       1, 141, 1958,
       1, 159, 1959)

# Make Z into a matrix with 3 columns
Z <- matrix(Z, ncol = 3, byrow = TRUE)

# Calculate m1
m1 <- t(Z) %*% Z

# Calculate m2
m2 <- solve(m1)

# Calculate m3
m3 <- t(Z) %*% y

# Calculate estimator beta
beta <- m2 %*% m3

# Print estimator beta
cat("The estimator is", beta)