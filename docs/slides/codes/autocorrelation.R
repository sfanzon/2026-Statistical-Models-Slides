# Remember to save data on Desktop

# Change working directory to Desktop
# In MacOS type
# setwd("~/Desktop")

# In Windows type
# setwd("C:/Users/YourUsername/Desktop")

# Load dataset
prices <- read.table(file = "stock_gold.txt",
                    header = TRUE)

# Store data-frame into 2 vectors
stock.price <- prices[ , 1]
gold.price <- prices[ , 2]

# Fit regression model
model <- lm(gold.price ~ stock.price)

# Compute residuals
residuals <- model$resid

# Time-series plot of residuals
plot(residuals,
     xlab = "Time", 
     ylab = "Residuals",
     pch = 16,
     cex = 2)

# Add y = 0 line for reference
abline(0, 0, col = "red", lwd = 4)

# Compute length of residuals
length(residuals)

# Generate lagged version of residuals
residuals.lag.0 <- residuals[2:33]
residuals.lag.1 <-residuals[1:32]

# Plot Original residuals 
# Vs Lagged residuals
plot(residuals.lag.1, 
     residuals.lag.0,
     xlab = "Residuals Lag 1", 
     ylab = "Residuals Lag 0",
     pch = 16,
     cex = 2)