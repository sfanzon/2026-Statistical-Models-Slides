# Divorces data
year <- c(1, 2, 3, 4, 5, 6,7, 8, 9, 10, 15, 20, 25, 30)
percent <- c(3.51, 9.5, 8.91, 9.35, 8.18, 6.43, 5.31, 
             5.07, 3.65, 3.8, 2.83, 1.51, 1.27, 0.49)

# Scatter plot of data
plot(year, percent, xlab = "Years of marriage", 
     ylab = "Risk of divorce", 
     pch = 16)

# Fit linear model
linear <- lm(percent ~ year)

# Print summary
print(summary(linear))

# Fit quadratic model
quadratic <- lm(percent ~ year + I( year^2 ))

# Print summary
print(summary(quadratic))

# Plot Linear Vs Quadratic
polynomial <- Vectorize(function(x, ps) {
  n <- length(ps)
  sum(ps*x^(1:n-1))
}, "x")
curve(polynomial(x, coef(linear)), add=TRUE, col = "red")
curve(polynomial(x, coef(quadratic)), add=TRUE, col = "blue", lty = 2)
legend("topright", legend = c("Linear", "Quadratic"), 
       col = c("red", "blue"), lty = c(1,2))