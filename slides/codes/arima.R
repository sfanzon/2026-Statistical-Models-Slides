# Remember to save data on Desktop

# Change working directory to Desktop
# In MacOS type
# setwd("~/Desktop")

# In Windows type
# setwd("C:/Users/YourUsername/Desktop")

# Load dataset
prices <- read.table(file = "gold_stock.txt",
                    header = TRUE)

# Store data-frame into 2 vectors
gold.price <- prices$gold_price
stock.price <- prices$stock_price

# Fit AR(1) model
arima(gold.price, xreg = stock.price, order = c(1,0,0))

# Compute degrees of freedom
df <- length(gold.price) - 3

# Save estimated coefficients and e.s.e.
coeff <- c(0.5578, 3.9406, -0.0487)
ese <- c(0.1545, 0.5675, 0.0247)

# Compute t-statistics
t <- coeff / ese

# Compute p-values
p.value <- 2 * (1 - pt(abs(t), df))

# Print p-values
cat("The p-values for parameters phi, alpha, beta, are, respectively", p.value)