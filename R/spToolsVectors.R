# spTools
## Vectorized Versions of statpsych Functions

#' Confidence Intervals for a Set of Means
#'
#' Calculates the confidence interval for a vector of sample means given
#' standard deviations and sample sizes. Returns a matrix with the estimate,
#' standard error, degrees of freedom, and lower and upper limits of the
#' confidence interval.
#'
#' @param alpha Significance level.
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

#' Confidence Interval for Independent Groups Mean Difference
#'
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

#' Confidence Interval for a Independent Groups Standardized Mean Difference
#'
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

#' Confidence Interval for Paired Samples Mean Difference
#'
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

#' Confidence Interval for a Paired Samples Standardized Mean Difference
#'
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

#' Confidence Intervals for a Set of Pearson (or Partial) Correlations
#'
#' A wrapper function for `ci.cor` that accepts vectors of correlations,
#' control-variable counts, and sample sizes.
#'
#' @param alpha Alpha level for 1-alpha confidence.
#' @param cor Numeric vector of estimated Pearson or partial correlations.
#' @param s Numeric vector of control-variable counts (set 0 for Pearson).
#' @param n Numeric vector of sample sizes.
#'
#' @return
#' A matrix with one row per element of `cor` and columns from `ci.cor`:
#' `Estimate`, `SE`, `LL`, and `UL`.
#'
#' @examples
#' ci.cor.vec(
#'   alpha = .05,
#'   cor = c(.60, .70),
#'   s = c(0, 1),
#'   n = c(150, 135)
#' )
#'
#' @export
ci.cor.vec <- function(alpha, cor, s, n) {
  if (!(length(cor) == length(s) && length(s) == length(n))) {
    stop("Arguments 'cor', 's', and 'n' must have the same length.")
  }

  ci_cor <- getExportedValue("statpsych", "ci.cor")

  out <- do.call(
    rbind,
    lapply(seq_along(cor), function(i) {
      ci_cor(alpha = alpha, cor = cor[i], s = s[i], n = n[i])
    })
  )

  rownames(out) <- names(cor)
  if (is.null(rownames(out))) {
    rownames(out) <- paste0("Group_", seq_along(cor))
  }

  return(out)
}

#' Confidence Interval for Independent Groups Pearson Correlation Difference
#'
#' A wrapper function for `ci.cor2` that accepts vectors for group
#' correlations and sample sizes.
#'
#' @param alpha Alpha level for 1-alpha confidence.
#' @param cor Numeric vector of length 2: correlations for groups 1 and 2.
#' @param n Numeric vector of length 2: sample sizes for groups 1 and 2.
#'
#' @return
#' A 1-row matrix identical to the output of `ci.cor2`.
#'
#' @examples
#' ci.cor2.vec(.05, cor = c(.64, .31), n = c(200, 200))
#'
#' @export
ci.cor2.vec <- function(alpha, cor, n) {
  if (length(cor) != 2 || length(n) != 2) {
    stop("Arguments 'cor' and 'n' must be numeric vectors of length 2.")
  }

  ci_cor2 <- getExportedValue("statpsych", "ci.cor2")

  ci_cor2(alpha = alpha, cor1 = cor[1], cor2 = cor[2], n1 = n[1], n2 = n[2])
}

#' Confidence Intervals for a Set of Spearman Correlations
#'
#' A wrapper function for `ci.spear` that accepts lists of paired vectors.
#'
#' @param alpha Alpha level for 1-alpha confidence.
#' @param y List of numeric vectors for y scores.
#' @param x List of numeric vectors for x scores (paired with `y`).
#'
#' @return
#' A matrix with one row per y/x pair and columns from `ci.spear`:
#' `Estimate`, `SE`, `LL`, and `UL`.
#'
#' @examples
#' y_list <- list(
#'   c(21, 4, 9, 12, 35, 18, 10, 22, 24, 1, 6, 8, 13, 16, 19),
#'   c(5, 7, 9, 10, 13, 15, 18, 20, 22, 25)
#' )
#' x_list <- list(
#'   c(67, 28, 30, 28, 52, 40, 25, 37, 44, 10, 14, 20, 28, 40, 51),
#'   c(3, 5, 8, 8, 11, 12, 16, 18, 19, 23)
#' )
#' ci.spear.vec(.05, y = y_list, x = x_list)
#'
#' @export
ci.spear.vec <- function(alpha, y, x) {
  if (!is.list(y) || !is.list(x)) {
    stop("Arguments 'y' and 'x' must be lists of numeric vectors.")
  }
  if (length(y) != length(x)) {
    stop("Arguments 'y' and 'x' must have the same length.")
  }

  ci_spear <- getExportedValue("statpsych", "ci.spear")

  out <- do.call(
    rbind,
    lapply(seq_along(y), function(i) {
      ci_spear(alpha = alpha, y = y[[i]], x = x[[i]])
    })
  )

  rownames(out) <- names(y)
  if (is.null(rownames(out))) {
    rownames(out) <- paste0("Group_", seq_along(y))
  }

  return(out)
}

#' Confidence Intervals for a Set of One-Sample Proportions
#'
#' A wrapper function for `ci.prop` that accepts vectors of frequencies
#' and sample sizes.
#'
#' @param alpha Alpha level for 1-alpha confidence.
#' @param f Numeric vector of frequencies.
#' @param n Numeric vector of sample sizes.
#'
#' @return
#' A matrix with one row per element of `f` and columns from `ci.prop`:
#' `Estimate`, `SE`, `LL`, and `UL`.
#'
#' @examples
#' ci.prop.vec(.05, f = c(120, 95), n = c(300, 250))
#'
#' @export
ci.prop.vec <- function(alpha, f, n) {
  if (length(f) != length(n)) {
    stop("Arguments 'f' and 'n' must have the same length.")
  }

  ci_prop <- getExportedValue("statpsych", "ci.prop")

  out <- do.call(
    rbind,
    lapply(seq_along(f), function(i) {
      ci_prop(alpha = alpha, f = f[i], n = n[i])
    })
  )

  rownames(out) <- names(f)
  if (is.null(rownames(out))) {
    rownames(out) <- paste0("Group_", seq_along(f))
  }

  return(out)
}

#' Confidence Interval for Independent Groups Proportion Difference
#'
#' A wrapper function for `ci.prop2` that accepts vectors for frequencies
#' and sample sizes in two groups.
#'
#' @param alpha Alpha level for 1-alpha confidence.
#' @param f Numeric vector of length 2: frequencies for groups 1 and 2.
#' @param n Numeric vector of length 2: sample sizes for groups 1 and 2.
#'
#' @return
#' A 1-row matrix identical to the output of `ci.prop2`.
#'
#' @examples
#' ci.prop2.vec(.05, f = c(57, 15), n = c(100, 100))
#'
#' @export
ci.prop2.vec <- function(alpha, f, n) {
  if (length(f) != 2 || length(n) != 2) {
    stop("Arguments 'f' and 'n' must be numeric vectors of length 2.")
  }

  ci_prop2 <- getExportedValue("statpsych", "ci.prop2")

  ci_prop2(alpha = alpha, f1 = f[1], f2 = f[2], n1 = n[1], n2 = n[2])
}
