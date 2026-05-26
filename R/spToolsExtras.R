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

#' Confidence Intervals to Compare Two Independent Means and Their Difference
#'
#' Returns confidence intervals for each group mean and a single comparison row
#' for the two-group mean difference.
#'
#' @param alpha Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
#' @param m Numeric vector of length 2. Means for groups 1 and 2.
#' @param sd Numeric vector of length 2. Standard deviations for groups 1 and 2.
#' @param n Numeric vector of length 2. Sample sizes for groups 1 and 2.
#'
#' @return A 3-row matrix with rows for group 1, group 2, and comparison.
#' Group rows contain output from `ci.mean.vec`. The comparison row contains
#' selected columns from `ci.mean2.vec` corresponding to the two-group
#' difference summary.
#'
#' @details
#' The comparison row is computed from `ci.mean2.vec` using reversed group order,
#' then reduced to columns 1, 2, 4, 6, and 7 to align with the group output.
#'
#' @examples
#' ci.mean2.compare(
#'   alpha = .05,
#'   m = c(5.2, 6.1),
#'   sd = c(1.1, 1.3),
#'   n = c(30, 28)
#' )
#'
#' @export
ci.mean2.compare <- function(alpha, m, sd, n) {
  groups <- ci.mean.vec(alpha = alpha, m = m, sd = sd, n = n)
  compare <- ci.mean2.vec(alpha = alpha, m = rev(m), sd = rev(sd), n = rev(n))[1, c(1, 2, 4, 6, 7)]
  results <- rbind(groups, compare)
  rownames(results) <- c(rownames(results)[1], rownames(results)[2], "Comparison")
  return(results)
}

#' Confidence Intervals to Compare Two Paired Means and Their Difference
#'
#' Returns confidence intervals for each paired measurement mean and a single
#' comparison row for the paired mean difference.
#'
#' @param alpha Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
#' @param m Numeric vector of length 2. Means for measurements 1 and 2.
#' @param sd Numeric vector of length 2. Standard deviations for measurements 1 and 2.
#' @param cor Numeric scalar. Estimated correlation between paired measurements.
#' @param n Numeric scalar or numeric vector of length 2. Paired sample size.
#'
#' @return A 3-row matrix with rows for measurement 1, measurement 2, and comparison.
#' Measurement rows contain output from `ci.mean.vec`. The comparison row contains
#' selected columns from `ci.mean.ps.vec` corresponding to the paired-difference
#' summary.
#'
#' @details
#' If `n` is supplied as a scalar, it is expanded to length 2 for the
#' measurement rows.
#'
#' @examples
#' ci.mean.ps.compare(
#'   alpha = .05,
#'   m = c(58.2, 51.4),
#'   sd = c(7.43, 8.92),
#'   cor = .537,
#'   n = 30
#' )
#'
#' @export
ci.mean.ps.compare <- function(alpha, m, sd, cor, n) {
  n_groups <- n
  if (length(n_groups) == 1) {
    n_groups <- rep(n_groups, 2)
  }
  groups <- ci.mean.vec(alpha = alpha, m = m, sd = sd, n = n_groups)
  compare <- ci.mean.ps.vec(alpha = alpha, m = m, sd = sd, cor = cor, n = n_groups[1])[1, c(1, 2, 4, 6, 7)]
  results <- rbind(groups, compare)
  rownames(results) <- c("Measure 1", "Measure 2", "Comparison")
  return(results)
}

#' Confidence Intervals to Compare Two Correlations and Their Difference
#'
#' Returns confidence intervals for each group correlation and a single
#' comparison row for the two-group correlation difference.
#'
#' @param alpha Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
#' @param cor Numeric vector of length 2. Correlations for groups 1 and 2.
#' @param s Numeric scalar or numeric vector of length 2. Number of control
#' variables (set to 0 for Pearson correlations).
#' @param n Numeric vector of length 2. Sample sizes for groups 1 and 2.
#'
#' @return A 3-row matrix with rows for group 1, group 2, and comparison.
#' Group rows contain output from `ci.cor.vec`. The comparison row contains
#' output from the two-group correlation-difference confidence interval.
#'
#' @details
#' If `s` is supplied as a scalar, it is used for both groups. If `s` is
#' supplied as a vector, both values must be equal because the two-group
#' difference method requires a common control-variable count.
#'
#' @examples
#' ci.cor.compare(
#'   alpha = .05,
#'   cor = c(.64, .31),
#'   s = 0,
#'   n = c(200, 200)
#' )
#'
#' @export
ci.cor.compare <- function(alpha, cor, s, n) {
  if (length(cor) != 2 || length(n) != 2) {
    stop("Arguments 'cor' and 'n' must be numeric vectors of length 2.")
  }

  if (length(s) == 1) {
    s <- rep(s, 2)
  }
  if (length(s) != 2) {
    stop("Argument 's' must be a scalar or a numeric vector of length 2.")
  }
  if (s[1] != s[2]) {
    stop("Values in 's' must be equal for a two-group correlation difference.")
  }

  groups <- ci.cor.vec(alpha = alpha, cor = cor, s = s, n = n)

  ci_cor2 <- getExportedValue("statpsych", "ci.cor2")
  compare <- ci_cor2(
    alpha = alpha,
    cor1 = cor[1], cor2 = cor[2],
    n1 = n[1], n2 = n[2],
    s = s[1]
  )

  compare <- compare[1, c("Estimate", "SE", "LL", "UL")]
  results <- rbind(groups, compare)
  rownames(results) <- c("Group 1", "Group 2", "Comparison")
  return(results)
}

#' Confidence Intervals to Compare Two Proportions
#'
#' Returns confidence intervals for each group proportion and comparison rows
#' for the proportion difference, the proportion ratio, or both.
#'
#' @param alpha Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
#' @param f Numeric vector of length 2. Frequencies for groups 1 and 2.
#' @param n Numeric vector of length 2. Sample sizes for groups 1 and 2.
#' @param type Character scalar indicating comparison rows to include:
#' `"difference"`, `"ratio"`, or `"both"`.
#'
#' @return A matrix with rows for group 1, group 2, and one or two comparison
#' rows depending on `type`. Columns are `Estimate`, `SE`, `LL`, and `UL`.
#'
#' @details
#' The difference row is obtained from `ci.prop2.vec`. The ratio row is
#' obtained from `statpsych::ci.ratio.prop2`. Because `ci.ratio.prop2` does
#' not return a standard error column, `SE` is set to `NA` for the ratio row.
#'
#' @examples
#' ci.prop.compare(
#'   alpha = .05,
#'   f = c(57, 15),
#'   n = c(100, 100),
#'   type = "both"
#' )
#'
#' @export
ci.prop.compare <- function(alpha, f, n, type = c("difference", "ratio", "both")) {
  type <- match.arg(type)

  if (length(f) != 2 || length(n) != 2) {
    stop("Arguments 'f' and 'n' must be numeric vectors of length 2.")
  }

  groups <- ci.prop.vec(alpha = alpha, f = f, n = n)
  rownames(groups) <- c("Group 1", "Group 2")

  rows <- list(groups)
  row_names <- rownames(groups)

  if (type %in% c("difference", "both")) {
    diff_row <- ci.prop2.vec(alpha = alpha, f = f, n = n)[1, c("Estimate", "SE", "LL", "UL")]
    rows[[length(rows) + 1]] <- diff_row
    row_names <- c(row_names, "Difference")
  }

  if (type %in% c("ratio", "both")) {
    ci_ratio_prop2 <- getExportedValue("statpsych", "ci.ratio.prop2")
    ratio_out <- ci_ratio_prop2(alpha = alpha, f1 = f[1], f2 = f[2], n1 = n[1], n2 = n[2])
    ratio_row <- c(
      Estimate = as.numeric(ratio_out[1, 3]),
      SE = NA_real_,
      LL = as.numeric(ratio_out[1, 4]),
      UL = as.numeric(ratio_out[1, 5])
    )
    rows[[length(rows) + 1]] <- ratio_row
    row_names <- c(row_names, "Ratio")
  }

  results <- do.call(rbind, rows)
  rownames(results) <- row_names
  return(results)
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
