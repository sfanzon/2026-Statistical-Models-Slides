# Divorces data
year <- c(1, 2, 3, 4, 5, 6,7, 8, 9, 10, 15, 20, 25, 30)
percent <- c(3.51, 9.5, 8.91, 9.35, 8.18, 6.43, 5.31, 
             5.07, 3.65, 3.8, 2.83, 1.51, 1.27, 0.49)

# Fit null model
null.model <- lm(percent ~ 1)

# Fit full model 
# This is the order 6 polynomial model
full.model <- lm(percent ~ year  + I( year^2 ) + I( year^3 ) + 
                                   I( year^4 ) + I( year^5 ) +
                                   I( year^6 ))

# Run stepwise regression
best.model <- step(null.model, 
                   direction = "both", 
                   scope = formula(full.model))

print(summary(best.model))