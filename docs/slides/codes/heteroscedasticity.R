# Remember to save data on Desktop

# Change working directory to Desktop
# In MacOS type
# setwd("~/Desktop")

# In Windows type
# setwd("C:/Users/YourUsername/Desktop")

# Load dataset on Stock Vs Gold prices
prices <- read.table(file = "stock_gold.txt",
                    header = TRUE)

# Store data-frame into 2 vectors
stock.price <- prices[ , 1]
gold.price <- prices[ , 2]

# Fit regression model
model <- lm(gold.price ~ stock.price)

# Scatter plot
plot(stock.price, 
     gold.price, 
     xlab = "Stock Price", 
     ylab = "Gold Price",
     pch = 16,
     cex = 2) 

# Plot regression line
abline(model, col = "red", lwd = 4)

# Compute residuals
residuals <- model$resid

# Histogram of residuals
hist(residuals,
xlab = "Residuals",
ylab = "Frequency",
col = "skyblue")

# Compute fitted values
fitted <- model$fitted

# Plot the residual graph
plot(fitted, residuals,
     xlab = "Fitted Values", 
     ylab = "Residuals",
     pch = 16,
     cex = 2)

# Add y = 0 line for reference
abline(0, 0, col = "red", lwd = 4)

# Transform data via log(y)
log.gold.price <- log(gold.price)

# Fit linear model with log(y) data
log.model <- lm(log.gold.price ~ stock.price)

# Compute residuals for log model
log.residuals <- log.model$resid

# Compute fitted values for log model
log.fitted <- log.model$fitted

# Plot residual graph for transformed model
plot(log.fitted, log.residuals,
     xlab = "Fitted Values", 
     ylab = "Residuals",
     pch = 16, 
     cex = 2)

# Add y = 0 line for reference
abline(0, 0, col = "red", lwd = 4)

# Histogram of residuals for transformed model
hist(log.residuals,
xlab = "Residuals",
ylab = "Frequency",
col = "skyblue")

# Create dataframe for predictors
predictors <- data.frame(stock.price = c(1.2, 2, 3.1, 4, 6))

# Make predictions
log.predictions <- predict(log.model, predictors, inter = "pred")

# Print predictions
print(log.predictions)

# Rescale log predictions
predictions <- exp(log.predictions)

print(predictions)