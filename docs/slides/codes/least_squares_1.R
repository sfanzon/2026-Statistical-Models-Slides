# Input blood pressure changes data into data-frame
changes <- data.frame(drug_A = c(1.9, 0.8, 1.1, 0.1, -0.1, 
                                 4.4, 4.6, 1.6, 5.5, 3.4),
                      drug_B = c(0.7, -1.0, -0.2, -1.2, -0.1, 
                                 3.4, 0.0, 0.8, 3.7, 2.0)
                     )

# Assign data-frame columns to vectors x and y
x <- changes$drug_A
y <- changes$drug_B

# Compute averages xbar and ybar
xbar <- mean(x)
ybar <- mean(y)

# Compute covariances S_xx and S_xy
S_xx <- var(x)
S_xy <- cov(x,y)

# Compute least-square line coefficients
beta <- S_xy / S_xx
alpha <- ybar - beta * xbar

# Plot the data
plot(x, y, xlab = "", ylab = "", pch = 16, cex = 2)

# Add labels
mtext("Drug A reaction x_i", side = 1, line = 3, cex = 2.1)
mtext("Drug B reaction y_i", side = 2, line = 2.5, cex = 2.1)

# Overlay least-squares line
abline(a = alpha, b = beta, col = "red", lwd = 3)

# Add legend
legend("topleft", 
       legend = c("Data", "Least-Squares Line"), 
       col = c("black", "red"), 
       pch = c(16, NA), 
       lty = c(NA, 1), 
       lwd = c(NA, 3))