# spTools
## Vectorized Versions

#' Confidence Intervals for a Set of Means with vector input
#'
#' Calculates the confidence interval for a vector of sample means given
#' standard deviations and sample sizes. Returns a matrix with the estimate,
#' standard error, degrees of freedom, and lower and upper limits of the
#' confidence interval.
#'
#' @param alpha Significance level (e.g., 0.05 for a 95\% confidence interval).
#' @param m A named numeric vector of sample means.
#' @param sd A numeric vector of sample standard deviations.
#' @param n A numeric vector of sample sizes (number of observations per group).
#'
#' @return A matrix with rows corresponding to group names (from `means`) and columns:
#' \describe{
#'   \item{Estimate}{The sample mean.}
#'   \item{SE}{Standard error of the mean.}
#'   \item{df}{Degrees of freedom (n - 1).}
#'   \item{LL}{Lower limit of the confidence interval.}
#'   \item{UL}{Upper limit of the confidence interval.}
#' }
#'
#' @examples
#' # Named groups
#' ci.mean.vec(
#'   alpha = 0.05,
#'   m = c(A = 5.2, B = 6.1),
#'   sd = c(1.1, 1.3),
#'   n = c(30, 28)
#' )
#'
#' # Unnamed groups: generic names will be assigned
#' ci.mean.vec(
#'   alpha = 0.05,
#'   m = c(5.2, 6.1),
#'   sd = c(1.1, 1.3),
#'   n = c(30, 28)
#' )
#'
#' @export
ci.mean.vec <- function(alpha, m, sd, n) {
  if (!(length(m) == length(sd) && length(sd) == length(n))) {
    stop("Arguments 'means', 'sds', and 'ns' must have the same length.")
  }

  df <- n - 1
  se <- sd / sqrt(n)
  tcrit <- qt(1 - alpha / 2, df = df)
  moe <- tcrit * se
  ll <- m - moe
  ul <- m + moe

  result <- cbind(
    Estimate = m,
    SE = se,
    df = df,
    LL = ll,
    UL = ul
  )

  rownames(result) <- names(m)
  if (is.null(rownames(result))) {
    rownames(result) <- paste0("Group_", seq_along(m))
  }

  return(result)
}

#' Wrapper for ci.mean2 with vector input
#'
#' @description
#' A wrapper function for ci.mean2 that accepts vectors for means, standard deviations, and sample sizes.
#'
#' @param alpha Alpha level for 1-alpha confidence
#' @param m     Numeric vector of length 2: means for groups 1 and 2
#' @param sd    Numeric vector of length 2: standard deviations for groups 1 and 2
#' @param n     Numeric vector of length 2: sample sizes for groups 1 and 2
#'
#' @return
#' A 2-row matrix identical to the output of ci.mean2
#'
#' @examples
#' ci.mean2.vec(.05, c(15.4, 10.3), c(2.67, 2.15), c(30, 20))
#'
#' @export
ci.mean2.vec <- function(alpha, m, sd, n) {
  if (length(m) != 2 || length(sd) != 2 || length(n) != 2) {
    stop("Arguments 'm', 'sd', and 'n' must be numeric vectors of length 2.")
  }
  ci.mean2(alpha, m1 = m[1], m2 = m[2], sd1 = sd[1], sd2 = sd[2], n1 = n[1], n2 = n[2])
}


#' Wrapper for ci.stdmean2 with vector input
#'
#' @description
#' A wrapper function for ci.stdmean2 that accepts vectors for means, standard deviations, and sample sizes.
#'
#' @param alpha Alpha level for 1-alpha confidence
#' @param m     Numeric vector of length 2: means for groups 1 and 2
#' @param sd    Numeric vector of length 2: standard deviations for groups 1 and 2
#' @param n     Numeric vector of length 2: sample sizes for groups 1 and 2
#'
#' @return
#' A 4-row matrix identical to the output of ci.stdmean2
#'
#' @examples
#' ci.stdmean2.vec(.05, c(35.1, 26.7), c(7.32, 6.98), c(30, 30))
#'
#' @export
ci.stdmean2.vec <- function(alpha, m, sd, n) {
  if (length(m) != 2 || length(sd) != 2 || length(n) != 2) {
    stop("Arguments 'm', 'sd', and 'n' must be numeric vectors of length 2.")
  }
  ci.stdmean2(alpha, m1 = m[1], m2 = m[2], sd1 = sd[1], sd2 = sd[2], n1 = n[1], n2 = n[2])
}


#' Wrapper for ci.mean.ps with vector input
#'
#' @description
#' A wrapper function for ci.mean.ps that accepts vectors for means and standard deviations.
#'
#' @param alpha Alpha level for 1-alpha confidence
#' @param m     Numeric vector of length 2: means for the two measurements
#' @param sd    Numeric vector of length 2: standard deviations for the two measurements
#' @param cor   Estimated correlation between the two measurements
#' @param n     Sample size (scalar)
#'
#' @return
#' A 1-row matrix identical to the output of ci.mean.ps
#'
#' @examples
#' ci.mean.ps.vec(.05, c(58.2, 51.4), c(7.43, 8.92), .537, 30)
#'
#' @export
ci.mean.ps.vec <- function(alpha, m, sd, cor, n) {
  if (length(m) != 2 || length(sd) != 2) {
    stop("Arguments 'm' and 'sd' must be numeric vectors of length 2.")
  }
  if (!is.numeric(cor) || length(cor) != 1) {
    stop("Argument 'cor' must be a single numeric value.")
  }
  if (!is.numeric(n) || length(n) != 1) {
    stop("Argument 'n' must be a single numeric value.")
  }
  ci.mean.ps(alpha, m1 = m[1], m2 = m[2], sd1 = sd[1], sd2 = sd[2], cor = cor, n = n)
}


#' Wrapper for ci.stdmean.ps with vector input
#'
#' @description
#' A wrapper function for ci.stdmean.ps that accepts vectors for means and standard deviations.
#'
#' @param alpha Alpha level for 1-alpha confidence
#' @param m     Numeric vector of length 2: means for the two measurements
#' @param sd    Numeric vector of length 2: standard deviations for the two measurements
#' @param cor   Estimated correlation between the two measurements (scalar)
#' @param n     Sample size (scalar)
#'
#' @return
#' A 3-row matrix identical to the output of ci.stdmean.ps
#'
#' @examples
#' ci.stdmean.ps.vec(.05, c(110.4, 102.1), c(15.3, 14.6), .75, 25)
#'
#' @export
ci.stdmean.ps.vec <- function(alpha, m, sd, cor, n) {
  if (length(m) != 2 || length(sd) != 2) {
    stop("Arguments 'm' and 'sd' must be numeric vectors of length 2.")
  }
  if (!is.numeric(cor) || length(cor) != 1) {
    stop("Argument 'cor' must be a single numeric value.")
  }
  if (!is.numeric(n) || length(n) != 1) {
    stop("Argument 'n' must be a single numeric value.")
  }
  ci.stdmean.ps(alpha, m1 = m[1], m2 = m[2], sd1 = sd[1], sd2 = sd[2], cor = cor, n = n)
}
