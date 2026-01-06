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

# Fit ANCOVA model
ancova <- lm(fridge.sales ~ quarter + durable.goods.sales, data = d)

print(summary(ancova))

# Fit simple linear model
linear <- lm(fridge.sales ~ durable.goods.sales, data = d)

# F-test for Model Selections
anova(linear, ancova)