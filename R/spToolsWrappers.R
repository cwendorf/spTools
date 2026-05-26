# spTools
## Wrapper Functions for Comparison Tables

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

#' Confidence Intervals to Compare Two Dependent Correlations and Their Difference
#'
#' Returns confidence intervals for each correlation and a single comparison
#' row for the dependent-correlation difference.
#'
#' @param alpha Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
#' @param cor Numeric vector of length 2. Correlations for measures 1 and 2.
#' @param cor12 Numeric scalar. Correlation between the two predictor variables
#' associated with `cor[1]` and `cor[2]`.
#' @param n Numeric scalar or numeric vector of length 2. Common sample size.
#' @param s Numeric scalar. Number of control variables for the group-level
#' confidence intervals (set to 0 for Pearson correlations).
#'
#' @return A 3-row matrix with rows for correlation 1, correlation 2, and
#' comparison. Group rows contain output from `ci.cor.vec`. The comparison row
#' contains output from `statpsych::ci.cor.dep`.
#'
#' @details
#' If `n` is supplied as a vector, both values must be equal.
#'
#' @examples
#' ci.cor.dep.compare(
#'   alpha = .05,
#'   cor = c(.396, .179),
#'   cor12 = .088,
#'   n = 166,
#'   s = 0
#' )
#'
#' @export
ci.cor.dep.compare <- function(alpha, cor, cor12, n, s = 0) {
  if (length(cor) != 2) {
    stop("Argument 'cor' must be a numeric vector of length 2.")
  }

  if (length(n) == 1) {
    n <- rep(n, 2)
  }
  if (length(n) != 2 || n[1] != n[2]) {
    stop("Argument 'n' must be a scalar or a length-2 vector with equal values.")
  }

  groups <- ci.cor.vec(alpha = alpha, cor = cor, s = c(s, s), n = n)

  ci_cor_dep <- getExportedValue("statpsych", "ci.cor.dep")
  compare <- ci_cor_dep(alpha = alpha, cor1 = cor[1], cor2 = cor[2], cor12 = cor12, n = n[1])
  compare <- compare[1, c("Estimate", "SE", "LL", "UL")]

  results <- rbind(groups, compare)
  rownames(results) <- c("Measure 1", "Measure 2", "Comparison")
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

#' Confidence Intervals to Compare Two Paired Proportions and Their Difference
#'
#' Returns confidence intervals for each paired-measurement proportion and a
#' single comparison row for the paired proportion difference.
#'
#' @param alpha Numeric scalar. Significance level (e.g., 0.05 for 95% CI).
#' @param f00 Cell count for outcome 0 at measure 1 and outcome 0 at measure 2.
#' @param f01 Cell count for outcome 0 at measure 1 and outcome 1 at measure 2.
#' @param f10 Cell count for outcome 1 at measure 1 and outcome 0 at measure 2.
#' @param f11 Cell count for outcome 1 at measure 1 and outcome 1 at measure 2.
#'
#' @return A 3-row matrix with rows for measure 1, measure 2, and comparison.
#' Group rows contain one-sample proportion confidence intervals for each
#' measure. The comparison row contains selected columns from
#' `statpsych::ci.prop.ps`.
#'
#' @examples
#' ci.prop.ps.compare(
#'   alpha = .05,
#'   f00 = 12,
#'   f01 = 4,
#'   f10 = 26,
#'   f11 = 6
#' )
#'
#' @export
ci.prop.ps.compare <- function(alpha, f00, f01, f10, f11) {
  n <- f00 + f01 + f10 + f11
  if (n <= 0) {
    stop("Total paired sample size must be positive.")
  }

  # Marginal event frequencies for measure 1 and measure 2.
  f1 <- f10 + f11
  f2 <- f01 + f11

  groups <- ci.prop.vec(alpha = alpha, f = c(f1, f2), n = c(n, n))

  ci_prop_ps <- getExportedValue("statpsych", "ci.prop.ps")
  compare <- ci_prop_ps(alpha = alpha, f00 = f00, f01 = f01, f10 = f10, f11 = f11)
  compare <- compare[1, c("Estimate", "SE", "LL", "UL")]

  results <- rbind(groups, compare)
  rownames(results) <- c("Measure 1", "Measure 2", "Comparison")
  return(results)
}
