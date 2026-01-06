# Enter data
y <- c(70, 65, 90, 95, 110, 115, 120, 140, 155, 150)
x2 <- c(80, 100, 120, 140, 160, 180, 200, 220, 240, 260)
x3 <- c(810, 1009, 1273, 1425, 1633, 1876, 2052, 2201, 2435, 2686)

# Fit model
model <- lm(y ~ x2 + x3)

# We want to display only part of summary
# First capture the output into a vector
temp <- capture.output(summary(model))

# Then print only the lines of interest
cat(paste(temp[9:20], collapse = "\n"))

# Compute correlation
correlation <- cor(x2, x3)

# Print correlation
cat("\nCorrelation between X2 and X3 variables is", correlation)

# Klein's rule of thumb: fit X2 vs X3
klein <- lm(x2 ~ x3)

temp.klein <- capture.output(summary(klein))
cat("\n\n")
cat(paste(temp.klein[9:19], collapse = "\n"))

# Fit expenditure as function of income
model.1 <- lm(y ~ x2)

# Fit expenditure as function of wealth
model.2 <- lm(y ~ x3)

temp.1 <- capture.output(summary(model.1))
temp.2 <- capture.output(summary(model.2))

cat("\n\n")
cat(paste(temp.1[9:19], collapse = "\n"))
cat("\n\n")
cat(paste(temp.2[9:19], collapse = "\n"))