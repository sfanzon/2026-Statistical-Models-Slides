# Store each row into and R vector
row_1 <- c(2180, 871)
row_2 <- c(1820, 1400)
row_3 <- c(1703, 614)

# Assemble the rows into an R matrix
counts <- rbind(row_1, row_2, row_3)

# Perform chi-squared test of independence
ans <- chisq.test(counts)

# Print answer
print(ans)