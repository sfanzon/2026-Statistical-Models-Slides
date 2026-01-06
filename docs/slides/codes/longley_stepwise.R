# Remember to save data on Desktop

# Change working directory to Desktop
# In MacOS type
# setwd("~/Desktop")

# In Windows type
# setwd("C:/Users/YourUsername/Desktop")

# Read data file
longley <- read.table(file = "longley.txt",
                      header = TRUE)

# Store columns in vectors
x2 <- longley[ , 1]        # GNP Deflator
x3 <- longley[ , 2]        # GNP
x4 <- longley[ , 3]        # Unemployed
x5 <- longley[ , 4]        # Armed Forces
x6 <- longley[ , 5]        # Population
x7 <- longley[ , 6]        # Year
y <- longley[ , 7]         # Employed

# Fit the null Model
null.model <- lm(y ~ 1)

# Fit the full Model
full.model <- lm(y ~ x2 + x3 + x4 + x5 + x6 + x7)

# Forward selection
best.model.1 <- step(null.model, 
                    direction = "forward", 
                    scope = formula(full.model))

# Print best model forward selection
cat("\n\nThe best model using Forward Selection is\n\n")
print(summary(best.model.1))

# Backward selection
best.model.2 <- step(full.model, 
                    direction = "backward")

# Print best model forward selection
cat("\n\nThe best model using Backward Selection is\n\n")
print(summary(best.model.2))

# Stepwise selection
best.model.3 <- step(null.model, 
                    direction = "both",
                    scope = formula(full.model))

# Print best model stepwise selection is
cat("\n\nThe best model using Stepwise Selection is\n\n")
print(summary(best.model.3))