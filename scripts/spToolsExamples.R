# spTools
## Examples of the spTools Package

### Source StatPsych and spTools

source("https://raw.githubusercontent.com/dgbonett/statpsych/main/R/statpsych1.R")
source("http://raw.githubusercontent.com/cwendorf/spTools/main/source-spTools.R")

### Fix the Tukey Pairwise Comparison Output

m <- c(33.5, 37.9, 38.0, 44.1)
sd <- c(3.84, 3.84, 3.65, 4.98)
n <- c(10, 10, 10, 10)

ci.tukey(.05, m, sd, n)
ci.tukey(.05, m, sd, n) |> sp.tukey() # Fixes column output

### CIs for a Set of Sample Means

ci.mean.vec(alpha = .05, m = c(5.2, 6.1), sd = c(1.1, 1.3), n = c(30, 28)) # Vectorized Version

### Vectorize Two Group Mean Difference

ci.mean2(.05, 15.4, 10.3, 2.67, 2.15, 30, 20)
ci.mean2.vec(.05, c(15.4, 10.3), c(2.67, 2.15), c(30, 20)) # Vectorized Version

ci.stdmean2(.05, 35.1, 26.7, 7.32, 6.98, 30, 30)
ci.stdmean2.vec(.05, c(35.1, 26.7), c(7.32, 6.98), c(30, 30)) # Vectorized Version

### Vectorize Paired Samples Mean Difference

ci.mean.ps(.05, 58.2, 51.4, 7.43, 8.92, .537, 30)
ci.mean.ps.vec(.05, c(58.2, 51.4), c(7.43, 8.92), .537, 30) # Vectorized Version

ci.stdmean.ps(.05, 110.4, 102.1, 15.3, 14.6, .75, 25)
ci.stdmean.ps.vec(.05, c(110.4, 102.1), c(15.3, 14.6), .75, 25) # Vectorized Version

### Extend Repeated Measures Mean Contrast

m <- c(33.5, 37.9, 38.0, 44.1)
sd <- c(3.84, 3.84, 3.65, 4.98)
q <- c(.5, .5, -.5, -.5)

ci.lc.mean.ws(.05, m, sd, .672, 20, q) # Not available in statpsych
ci.lc.stdmean.ws(.05, m, sd, .672, 20, q) # Relies on single average correlation

m <- c(5.2, 6.1, 7.3)
sd <- c(1.1, 1.2, 1.4)
R <- matrix(c(
   1, 0.8, 0.6,
   0.8, 1, 0.7,
   0.6, 0.7, 1
 ), 3, 3, byrow = TRUE)
n <- c(30, 30, 30)
q <- c(1, 0, -1)

ci.lc.mean.ws(0.05, m, sd, R, n, q) # Permits full correlation matrix
ci.lc.mean.ws(0.05, m, sd, 0.60, n, q) # Relies on single average correlation

# Infer Missing t, df, and p value Columns

ci.mean(0.05, 24.5, 3.65, 40)
ci.mean(0.05, 24.5, 3.65, 40) |> sp.infer() # Adds additional columns
