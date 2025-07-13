# spTools
## Calculations Missing from StatPsych


#' Confidence Interval for a Linear Contrast of Means (Within-Subject)
#'
#' Computes a confidence interval for a linear contrast of means
#' from within-subject (repeated measures) data, accounting for the variance-covariance
#' structure given by standard deviations and correlation matrix.
#'
#' @param alpha Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
#' @param m Numeric vector of length J. Means of the J conditions.
#' @param s Numeric vector of length J. Standard deviations of the J conditions.
#' @param R Numeric J x J correlation matrix among conditions.
#' @param v Numeric vector of length J. Contrast weights.
#' @param n Integer scalar or vector of length J. Sample size (number of subjects).
#' If a vector is provided, all elements must be equal.
#'
#' @return A 1-row matrix with named columns:
#' \describe{
#'   \item{Estimate}{Linear contrast estimate \(\sum v_i m_i\).}
#'   \item{SE}{Standard error of the contrast estimate.}
#'   \item{df}{Degrees of freedom (n - 1).}
#'   \item{LL}{Lower limit of the confidence interval.}
#'   \item{UL}{Upper limit of the confidence interval.}
#' }
#'
#' @details
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
#' v <- c(-1, 0, 1)
#'
#' # Using scalar n
#' ci.lc.mean.ws(0.05, m, s, R, v, 30)
#'
#' # Using vector n (all values equal)
#' ci.lc.mean.ws(0.05, m, s, R, v, rep(30, 3))
#'
#' # This will throw an error: unequal n values
#' \dontrun{
#' ci.lc.mean.ws(0.05, m, s, R, v, c(30, 29, 30))
#' }
#'
#' @export
ci.lc.mean.ws <- function(alpha, m, s, R, v, n) {
  J <- length(m)
  if (!all(length(s) == J, length(v) == J, ncol(R) == J, nrow(R) == J)) {
    stop("Lengths of m, s, v must match. R must be a J x J correlation matrix.")
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
  Lhat <- sum(v * m)
  var_L <- as.numeric(t(v) %*% V %*% v) / n
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
