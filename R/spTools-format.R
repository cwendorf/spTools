# spTools
## Output Functions for Enhancing statpsych

#' Add Hypothesis Test Statistics to Confidence Interval Output
#'
#' Adds hypothesis test statistics to confidence interval output by appending
#' `t`, `df`, and `p` columns to results that contain `Estimate`, `SE`, `LL`,
#' and `UL`.
#' Degrees of freedom can be supplied directly, inferred from `n`, or estimated
#' from confidence interval width when needed.
#'
#' @param results 1-row matrix output from Bonett CI functions (with Estimate, SE, LL, UL)
#' @param df Optional degrees of freedom (if NULL, try to infer from n)
#' @param n Optional sample size to infer df if df is missing
#' @param null_value Hypothesized null value for test statistic (default 0)
#' @param conf_level Confidence level used to infer df from the CI width when df is not supplied
#'
#' @return 1-row matrix with columns:
#' Estimate, SE, t, df, p, LL, UL
#'
#' @examples
#' results <- matrix(
#'   c(10.5, 1.2, 8.1, 12.9),
#'   nrow = 1,
#'   dimnames = list(NULL, c("Estimate", "SE", "LL", "UL"))
#' )
#' ci.add.test(results, n = 30)
#'
#' @export
ci.add.test <- function(results, df = NULL, n = NULL, null_value = 0, conf_level = 0.95) {
  required_cols <- c("Estimate", "SE", "LL", "UL")
  if (!all(required_cols %in% colnames(results))) {
    stop("Input must contain columns: Estimate, SE, LL, UL")
  }

  results <- as.data.frame(results)
  nr <- nrow(results)
  
  est <- as.numeric(results$Estimate)
  se  <- as.numeric(results$SE)
  ll  <- as.numeric(results$LL)
  ul  <- as.numeric(results$UL)

  # If df is NULL but n provided
  if (is.null(df)) {
    if (!is.null(n)) {
      if (length(n) == 1) n <- rep(n, nr)
      df <- n - 1
    } else {
      df <- rep(NA_real_, nr)
    }
  } else {
    if (length(df) == 1) df <- rep(df, nr)
    if (length(df) != nr) stop("Length of df must be 1 or equal to number of rows")
  }

  # Infer df from CI if df is still NA
  for (i in seq_len(nr)) {
    if (is.na(df[i])) {
      t_ci <- abs((ul[i] - est[i]) / se[i])  # inferred t from CI width
      # Solve for df numerically: qt(conf_level/2 + 0.5, df) = t_ci
      f <- function(df_guess) abs(qt(conf_level/2 + 0.5, df = df_guess) - t_ci)
      opt <- optimize(f, interval = c(1, 1e6))
      df[i] <- opt$minimum
    }
  }
  
  if (length(null_value) == 1) null_value <- rep(null_value, nr)
  if (length(null_value) != nr) stop("Length of null_value must be 1 or equal to number of rows")

  stat <- (est - null_value) / se

  p <- numeric(nr)
  for (i in seq_len(nr)) {
    if (is.finite(df[i]) && df[i] > 0) {
      p[i] <- 2 * pt(-abs(stat[i]), df[i])
    } else {
      p[i] <- 2 * pnorm(-abs(stat[i]))
      df[i] <- NA
    }
  }

  out <- cbind(
    Estimate = est,
    SE = se,
    t = stat,
    df = df,
    p = p,
    LL = ll,
    UL = ul
  )
  rownames(out) <- rownames(results)
  return(out)
}

#' Remove Hypothesis Test Statistics from Confidence Interval Output
#'
#' Removes hypothesis test statistics (`t` and `p`) from matrix or data-frame
#' output. This is useful when test statistics were added for computation or
#' checking but should be hidden in final displayed results.
#'
#' @param x A matrix or data frame.
#'
#' @return The same object class with `t` and `p` columns removed when present.
#' If neither column exists, input is returned unchanged.
#'
#' @examples
#' out <- ci.add.test(ci.mean.ps.vec(.05, c(58.2, 51.4), c(7.43, 8.92), .537, 30), n = 30)
#' out |>
#'   ci.drop.test()
#'
#' @export
ci.drop.test <- function(x) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("x must be a matrix or data frame.")
  }

  if (is.null(colnames(x))) {
    return(x)
  }

  keep_cols <- !(tolower(colnames(x)) %in% c("t", "p"))
  x <- x[, keep_cols, drop = FALSE]
  return(x)
}

#' Clean and Reformat Tukey Confidence Interval Output
#'
#' Wrapper around `ci.tukey` that computes Tukey pairwise confidence intervals,
#' then reformats output by removing pair-index columns and assigning readable
#' row names such as `"1 v 2"`.
#'
#' @param alpha Type I error rate used by `ci.tukey`.
#' @param m Vector of group means.
#' @param sd Vector of group standard deviations.
#' @param n Vector of group sample sizes.
#' @param ... Additional arguments passed through to `ci.tukey`.
#'
#' @return A reformatted matrix with row names based on comparison pairs and
#' pair-index columns removed.
#'
#' @examples
#' ci.tukey.reformat(alpha = 0.05, m = c(5, 6, 7), sd = c(2, 2, 2), n = c(10, 10, 10))
#'
#' @export
ci.tukey.reformat <- function(alpha, m, sd, n, ...) {
  ci_tukey <- NULL
  if (exists("ci.tukey", mode = "function", inherits = TRUE)) {
    ci_tukey <- get("ci.tukey", mode = "function", inherits = TRUE)
  } else if (requireNamespace("statpsych", quietly = TRUE)) {
    ci_tukey <- getFromNamespace("ci.tukey", "statpsych")
  } else {
    stop("ci.tukey is not available. Load statpsych or source ci.tukey first.")
  }

  ci_out <- ci_tukey(alpha = alpha, m = m, sd = sd, n = n, ...)

  if (!is.matrix(ci_out) && !is.data.frame(ci_out)) {
    stop("ci.tukey must return a matrix or data frame.")
  }
  if (ncol(ci_out) < 2) {
    stop("ci.tukey output must include pair columns in the first two positions.")
  }

  pairs <- ci_out[, 1:2, drop = FALSE]
  rownames(ci_out) <- apply(pairs, 1, function(x) paste0(x[1], " v ", x[2]))
  ci_out[, -(1:2), drop = FALSE]
}
