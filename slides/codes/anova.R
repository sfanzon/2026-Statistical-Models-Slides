# Remember to save data on Desktop

# Change working directory to Desktop
# In MacOS type
# setwd("~/Desktop")

# In Windows type
# setwd("C:/Users/YourUsername/Desktop")

# Load dataset
d <- read.table(file = "fridge_sales.txt", header = TRUE)

# Convert d$quarter into a factor
d$quarter <- factor(d$quarter)

# Produce boxplot to compare sales in each quarter
boxplot(fridge.sales ~ quarter, data = d,
        main = "Quarterly Fridge sales", ylab = "Fridge sales")

# Perform ANOVA F-test
anova <- aov(fridge.sales ~ quarter, data = d)

# Print output
print(summary(anova))

# Fit dummy variable regression model
# We drop q1 to avoid dummy variable trap
dummy <- lm (fridge.sales ~ q2 + q3 + q4, data = d)

print(summary(dummy))