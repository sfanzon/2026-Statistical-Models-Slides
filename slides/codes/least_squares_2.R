# Input blood pressure changes data into data-frame
changes <- data.frame(drug_A = c(1.9, 0.8, 1.1, 0.1, -0.1, 
                                 4.4, 4.6, 1.6, 5.5, 3.4),
                      drug_B = c(0.7, -1.0, -0.2, -1.2, -0.1, 
                                 3.4, 0.0, 0.8, 3.7, 2.0)
                     )

# Fit least-squares line
least_squares <- lm(formula = drug_B ~ drug_A, data = changes) 

# Plot data 
plot(changes$drug_A, changes$drug_B, pch = 16, cex = 2)

# Plot least-squares line
abline(least_squares, col = "red", lwd = 3)