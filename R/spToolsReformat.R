# spTools
## Output Functions

#' Add test statistic, df, and p-value to CI output
#'
#' @param results 1-row matrix output from Bonett CI functions (with Estimate, SE, LL, UL)
#' @param df Optional degrees of freedom (if NULL, try to infer from n)
#' @param n Optional sample size to infer df if df is missing
#' @param null_value Hypothesized null value for test statistic (default 0)
#'
#' @return 1-row matrix with columns:
#' Estimate, SE, t, df, p, LL, UL
#'
#' @export
sp.infer <- function(results, df = NULL, n = NULL, null_value = 0, conf_level = 0.95) {
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
    Stat = stat,
    df = df,
    p = p,
    LL = ll,
    UL = ul
  )
  rownames(out) <- rownames(results)
  return(out)
}


#' Clean and Reformat Tukey CI Output
#'
#' This function takes the output from the Tukey confidence interval function (`ci.tukey`)  
#' and reformats it by removing the `pair` columns and assigning more descriptive row names using the pair indices.  
#' Row names will be formatted as `"1 v 2"`, `"1 v 3"`, etc.
#'
#' @param ci_out A matrix or data frame produced by `ci.tukey`, where the first two columns represent pairwise comparisons.
#'
#' @return A cleaned matrix without the first two `pair` columns. The rows will be named according to the pairs compared,  
#' using the format `"1 v 2"`, `"1 v 3"`, etc.
#'
#' @examples
#' out <- ci.tukey(alpha = 0.05, m = c(5, 6, 7), sd = 2, n = 10)
#' sp.tukey(out)
#'
#' @export
sp.tukey <- function(ci_out) {
  pairs <- ci_out[, 1:2]
  rownames(ci_out) <- apply(pairs, 1, function(x) paste0(x[1], " v ", x[2]))
  ci_out <- ci_out[, -(1:2), drop = FALSE]
  return(ci_out)
}
