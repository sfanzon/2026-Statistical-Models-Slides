# Remember to save data on Desktop

# Change working directory to Desktop
# In MacOS type
# setwd("~/Desktop")

# In Windows type
# setwd("C:/Users/YourUsername/Desktop")

# Read data file
longley <- read.table(file = "longley.txt", header = TRUE)

# Store columns in vectors
x2 <- longley[ , 1]        # GNP Deflator
x3 <- longley[ , 2]        # GNP
x4 <- longley[ , 3]        # Unemployed
x5 <- longley[ , 4]        # Armed Forces
x6 <- longley[ , 5]        # Population
x7 <- longley[ , 6]        # Year
y <- longley[ , 7]         # Employed

# Fit Model 1 and Model 2
model.1 <- lm(y ~ x4 + x5 + x7)
model.2 <- lm(y ~ x2 + x3 + x4 + x5 + x6 + x7)

# F-test for Model Selection
ans <- anova(model.1, model.2)

print(ans)