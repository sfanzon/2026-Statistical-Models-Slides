# Remember to save data on Desktop

# Change working directory to Desktop
# In MacOS type
# setwd("~/Desktop")

# In Windows type
# setwd("C:/Users/YourUsername/Desktop")

# Read the data
prices <- read.table(file = "stock_gold.txt",
                     header = TRUE
                     )

# Store data into vectors
stock.price <- prices$stock_price
gold.price <- prices$gold_price

# Fit simple linear regression model
fit.model <- lm(gold.price ~ stock.price)

# Print result to screen
print(summary(fit.model))

# Plot the data
plot(stock.price, gold.price, 
     xlab = "Stock Price", 
     ylab= "Gold Price",
     pch = 16)

# Plot the regression line in red
abline(fit.model, col = "red", lwd = 3)