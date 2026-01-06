N <- 10000      # We do 100000 iterations and print every 10000
total <- 0      # Counts total number of points
in_circle <- 0  # Counts points falling in circle

for (j in 1:10) {   # This loop is for printing message every N iterations
  for (i in 1:N) {  # Loop in which points are counted
    x <- runif(1, -1, 1); y <- runif(1, -1, 1);  # sample point (x,y)
    if (x^2 + y^2 <= 1) {    
      in_circle <- in_circle + 1  # If (x,y) in circle increase counter
    }
    total <- total + 1  # Increase total counter (in any case)
  }
  
  pi_approx <- ( in_circle / total ) * 4  # Compute approximate area

  cat(sprintf("After %8d iterations pi is %.08f, error is %.08f\n",
     (j * N), pi_approx, abs(pi_approx - pi)))
}