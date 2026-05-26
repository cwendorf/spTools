# spTools
## Extra Statistical Functions not in statpsych

#' Confidence Interval for a Within-Subjects Linear Contrast of Means
#'
#' Computes a confidence interval for a linear contrast of means
#' from within-subject (repeated measures) data, accounting for the variance-covariance
#' structure given by standard deviations and either a correlation matrix or a single
#' average correlation value.
#'
#' @param alpha Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
#' @param m Numeric vector of length J. Means of the J conditions.
#' @param s Numeric vector of length J. Standard deviations of the J conditions.
#' @param R Either a scalar (average correlation among all condition pairs) or a J x J
#' correlation matrix among conditions.
#' @param n Integer scalar or vector of length J. Sample size (number of subjects).
#' If a vector is provided, all elements must be equal.
#' @param q Numeric vector of length J. Contrast weights.
#'
#' @return A 1-row matrix with named columns:
#' \describe{
#'   \item{Estimate}{Linear contrast estimate.}
#'   \item{SE}{Standard error of the contrast estimate.}
#'   \item{df}{Degrees of freedom (n - 1).}
#'   \item{LL}{Lower limit of the confidence interval.}
#'   \item{UL}{Upper limit of the confidence interval.}
#' }
#'
#' @details
#' If \code{R} is a scalar, it is treated as the common correlation among all pairs of
#' conditions and is expanded into a J x J matrix with 1's on the diagonal and the
#' specified correlation on the off-diagonal. If a matrix is provided, it must be a
#' symmetric J x J correlation matrix.
#'
#' The function computes the variance-covariance matrix \(V = D R D\), where \(D\)
#' is a diagonal matrix of standard deviations. The variance of the contrast is
#' calculated as \(v' V v / n\). The confidence interval is constructed using the
#' t-distribution with \(n-1\) degrees of freedom. If \code{n} is a vector, all
#' values must be equal, and the common value will be used.
#'
#' @examples
#' m <- c(5.2, 6.1, 7.3)
#' s <- c(1.1, 1.2, 1.4)
#' R <- matrix(c(
#'   1, 0.8, 0.6,
#'   0.8, 1, 0.7,
#'   0.6, 0.7, 1
#' ), 3, 3, byrow = TRUE)
#' q <- c(-1, 0, 1)
#'
#' # Using a correlation matrix
#' ci.lc.mean.ws(0.05, m, s, R, 30, q)
#'
#' # Using a scalar average correlation
#' ci.lc.mean.ws(0.05, m, s, 0.65, 30, q)
#'
#' # Using vector n (all values equal)
#' ci.lc.mean.ws(0.05, m, s, R, rep(30, 3), q)
#'
#' @export
ci.lc.mean.ws <- function(alpha, m, s, R, n, q) {
  J <- length(m)
  
  # Check that m, s, q have the same length
  if (!all(length(s) == J, length(q) == J)) {
    stop("Lengths of m, s, and q must match.")
  }
  
  # Handle correlation input: matrix or scalar
  if (length(R) == 1) {
    # Single average correlation supplied
    R <- matrix(R, J, J)
    diag(R) <- 1
  } else {
    # Correlation matrix supplied
    if (!all(dim(R) == c(J, J))) {
      stop("R must be a scalar or a J x J correlation matrix.")
    }
  }
  
  # If n is a vector, check all elements are equal and use the common value
  if (length(n) > 1) {
    if (length(n) != J) stop("If n is a vector, it must have length equal to J.")
    if (!all(n == n[1])) stop("All elements of n must be equal.")
    n <- n[1]
  }
  
  # Construct variance-covariance matrix
  D <- diag(s)
  V <- D %*% R %*% D
  
  # Linear contrast estimate and standard error
  Lhat <- sum(q * m)
  var_L <- as.numeric(t(q) %*% V %*% q) / n
  se <- sqrt(var_L)
  
  # Degrees of freedom and confidence limits
  df <- n - 1
  tcrit <- qt(1 - alpha / 2, df)
  LL <- Lhat - tcrit * se
  UL <- Lhat + tcrit * se
  
  # Return a 1-row matrix with named columns
  out <- t(c(Lhat, se, df, LL, UL))
  colnames(out) <- c("Estimate", "SE", "df", "LL", "UL")
  rownames(out) <- ""
  return(out)
}

#' Confidence Intervals for 2x2 Within-Subjects Proportion Effects
#'
#' Computes confidence intervals and tests for the AB interaction effect,
#' main effect of A, main effect of B, simple main effects of A, and
#' simple main effects of B in a 2x2 within-subjects design with
#' dichotomous (0/1) responses.
#'
#' @param alpha Alpha level for 1 - alpha confidence intervals.
#' @param y11 Binary vector of responses at level 1 of A and level 1 of B.
#' @param y12 Binary vector of responses at level 1 of A and level 2 of B.
#' @param y21 Binary vector of responses at level 2 of A and level 1 of B.
#' @param y22 Binary vector of responses at level 2 of A and level 2 of B.
#'
#' @return A 7-row matrix (one row per effect) with columns:
#' \\itemize{
#'   \\item Estimate
#'   \\item SE
#'   \\item z
#'   \\item p
#'   \\item LL
#'   \\item UL
#' }
#'
#' @examples
#' y11 <- c(1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1)
#' y12 <- c(1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1)
#' y21 <- c(1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0)
#' y22 <- c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0)
#' ci.2x2.prop.ws(.05, y11, y12, y21, y22)
#'
#' @export
ci.2x2.prop.ws <- function(alpha, y11, y12, y21, y22) {
  if (length(y11) != length(y12) ||
      length(y11) != length(y21) ||
      length(y11) != length(y22)) {
    stop("all score vectors must have same length")
  }

  y <- cbind(y11, y12, y21, y22)

  if (any(is.na(y))) {
    stop("missing values are not allowed; remove or impute before calling")
  }

  valid <- y %in% c(0, 1)
  if (!all(valid)) {
    stop("all inputs must be binary values coded as 0 or 1")
  }

  n <- nrow(y)
  if (n < 2) {
    stop("at least two paired observations are required")
  }

  zcrit <- stats::qnorm(1 - alpha / 2)

  q1 <- c(1, -1, -1, 1)
  q2 <- c(.5, .5, -.5, -.5)
  q3 <- c(.5, -.5, .5, -.5)
  q4 <- c(1, 0, -1, 0)
  q5 <- c(0, 1, 0, -1)
  q6 <- c(1, -1, 0, 0)
  q7 <- c(0, 0, 1, -1)

  p_hat <- colMeans(y)
  sigma_hat <- stats::cov(y) / n

  one_effect <- function(q) {
    est <- as.numeric(sum(q * p_hat))
    se <- sqrt(as.numeric(t(q) %*% sigma_hat %*% q))

    if (se == 0) {
      z <- NA_real_
      p <- NA_real_
    } else {
      z <- est / se
      p <- 2 * (1 - stats::pnorm(abs(z)))
    }

    ll <- est - zcrit * se
    ul <- est + zcrit * se

    c(est, se, z, p, ll, ul)
  }

  out <- rbind(
    one_effect(q1),
    one_effect(q2),
    one_effect(q3),
    one_effect(q4),
    one_effect(q5),
    one_effect(q6),
    one_effect(q7)
  )

  rownames(out) <- c("AB:", "A:", "B:", "A at b1:", "A at b2:", "B at a1:", "B at a2:")
  colnames(out) <- c("Estimate", "SE", "z", "p", "LL", "UL")

  out
}
