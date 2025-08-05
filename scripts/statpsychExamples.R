# spTools
## Examples of statpsych Functions

### Source StatPsych

source("https://raw.githubusercontent.com/dgbonett/statpsych/main/R/statpsych1.R")

### One Sample Mean

data <- c(48, 52, 49, 51, 50, 47, 53, 50, 49, 52)
conf_level <- .95

ci.mean(alpha = 1 - conf_level, m = mean(data), sd = sd(data), n = length(data))
ci.stdmean(alpha = 1 - conf_level, m = mean(data), sd = sd(data), n = length(data), 47.5)

m <- 50.1
sd <- 1.911951
n <- 10
conf_level <- 0.95

ci.mean(alpha = 1 - conf_level, m = m, sd = sd, n = n)
ci.stdmean(alpha = 1 - conf_level, m = m, sd = sd, n = n, 47.5)

### Two Group Mean Difference

ci.mean2(.05, 15.4, 10.3, 2.67, 2.15, 30, 20)
ci.stdmean2(.05, 15.4, 10.3, 2.67, 2.15, 30, 20)

### Paired Samples Mean Difference

ci.mean.ps(.05, 110.4, 102.1, 15.3, 14.6, .75, 25)
ci.stdmean.ps(.05, 110.4, 102.1, 15.3, 14.6, .75, 25)

### Between Subjects Mean Contrast

m <- c(33.5, 37.9, 38.0, 44.1)
sd <- c(3.84, 3.84, 3.65, 4.98)
n <- c(10, 10, 10, 10)
v <- c(.5, .5, -.5, -.5)

ci.lc.mean.bs(.05, m, sd, n, v)
ci.lc.stdmean.bs(.05, m, sd, n, v)

### Within Subjects Mean Contrast

m <- c(33.5, 37.9, 38.0, 44.1)
sd <- c(3.84, 3.84, 3.65, 4.98)
q <- c(.5, .5, -.5, -.5)

# No unstandardized version is available in statpsych
ci.lc.stdmean.ws(.05, m, sd, .672, 20, q)

### 2x2 Between Subjects Factorial Design

y11 <- c(14, 15, 11, 7, 16, 12, 15, 16, 10, 9)
y12 <- c(18, 24, 14, 18, 22, 21, 16, 17, 14, 13)
y21 <- c(16, 11, 10, 17, 13, 18, 12, 16, 6, 15)
y22 <- c(18, 17, 11, 9, 9, 13, 18, 15, 14, 11)

ci.2x2.mean.bs(.05, y11, y12, y21, y22)
ci.2x2.stdmean.bs(.05, y11, y12, y21, y22)

### 2x2 Within Subjects Factorial Design

y11 <- c(21, 39, 32, 29, 27, 17, 27, 21, 28, 17, 12, 27)
y12 <- c(20, 36, 33, 27, 28, 14, 30, 20, 27, 15, 11, 22)
y21 <- c(21, 36, 30, 27, 28, 15, 27, 18, 29, 16, 11, 22)
y22 <- c(18, 34, 29, 28, 28, 17, 27, 21, 26, 16, 14, 23)

ci.2x2.mean.ws(.05, y11, y12, y21, y22)
ci.2x2.stdmean.ws(.05, y11, y12, y21, y22)

### 2x2 Mixed Factorial Design

y11 <- c(18, 19, 20, 17, 20, 16)
y12 <- c(19, 18, 19, 20, 17, 16)
y21 <- c(19, 16, 16, 14, 16, 18)
y22 <- c(16, 10, 12,  9, 13, 15)

ci.2x2.mean.mixed(.05, y11, y12, y21, y22)
ci.2x2.stdmean.mixed(.05, y11, y12, y21, y22)
