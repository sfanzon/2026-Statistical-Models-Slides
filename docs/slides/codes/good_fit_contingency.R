# Store matrix of counts into single R vector
counts <- c(27, 9, 15,
            54, 25, 24,
            84, 32, 28, 
            91, 37, 40,
            11, 10, 8,
            61, 12, 28)

# Store matrix of null probabilities into single R vector
# To avoid copy pasting, first store probabilities of each Manager
manager.null.p <- c(51/1788, 103/1788, 144/1788, 
                    168/1788, 29/1788, 101/1788)

# Repeat each entry 3 times 
null.p <- rep(manager.null.p, c(3, 3, 3, 3, 3, 3))

# Perform goodness-of-fit test
ans <- chisq.test(counts, p = null.p)

# Print answer
print(ans)