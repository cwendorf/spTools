# spTools
## Output Functions


#' Format Console Output
#'
#' Formats numeric results (data frames or lists of data frames) with specified digits, width, and padding.
#'
#' @param results A data frame or a list of data frames/matrices.
#' @param digits Number of decimal digits to round to.
#' @param padding Number of extra spaces to pad for width.
#' @param width Minimum width for formatted columns. If `NULL`, calculated from `digits` and `padding`.
#' @param spaced Logical; whether to print extra newlines between elements.
#' @param ... Additional arguments passed to `format()`.
#'
#' @return The formatted result is printed to the console and returned (invisibly) as a list or data frame.
#'
#' @examples
#' df <- data.frame(x = c(1.2345, 2.3456), y = c(3.4567, 4.5678))
#' sp.format(df, digits = 2)
#'
#' @export
sp.format <- function(results, digits = 3, padding = 2, width = NULL, spaced = TRUE, ...) {
  if (is.null(width)) width <- digits + (padding * 2)

  format_one <- function(res) {
    format(as.data.frame(round(res, digits = digits)),
           width = width, trim = TRUE, nsmall = digits, scientific = FALSE, ...)
  }

  if (is.list(results) && !is.data.frame(results)) {
    out_list <- lapply(results, format_one)
    names(out_list) <- names(results)  # preserve names

    for (nm in names(out_list)) {
      if (spaced) cat("\n")
      cat(nm, "\n", strrep("-", nchar(nm)), "\n", sep = "")
      print(out_list[[nm]])
      if (spaced) cat("\n")
    }

    invisible(out_list)
  } else {
    out <- format_one(results)
    if (spaced) cat("\n")
    print(out)
    if (spaced) cat("\n")
    invisible(out)
  }
}

#' Save Output to File
#'
#' Saves a result object to an `.rds`, `.txt`, or `.csv` file. If `file` is not provided,
#' a name is automatically generated using the object's name or timestamp.
#'
#' @param x The object to save (e.g., a list, data frame, or matrix).
#' @param file Optional file name (including extension: `.rds`, `.txt`, or `.csv`).
#' @param dir Output directory. If it does not exist, it will be created. Default is `"."`.
#' @param format File format: `"rds"` (default), `"txt"`, or `"csv"`.
#' @param name Optional name for auto-naming when `file` is `NULL`.
#' @param digits Number of digits to round numeric values for text or CSV output.
#' @param row.names Logical; whether to include row names in CSV/TXT. Default is FALSE.
#' @param ... Additional arguments passed to `write.table()` or `write.csv()`.
#'
#' @return Invisibly returns the full path to the saved file.
#'
#' @examples
#' # Example object to save
#' result <- data.frame(Estimate = 0.52, SE = 0.12, df = 24)
#'
#' # Save as RDS with automatic filename
#' preserve(result)
#'
#' # Save as TXT with a custom filename
#' preserve(result, file = "ci_output.txt", format = "txt")
#'
#' # Save as CSV with an auto-generated name, into a subfolder
#' preserve(result, format = "csv", dir = "outputs", name = "ci_mean1")
#'
#' # Save a list of results as RDS
#' res_list <- list(t1 = result, t2 = result)
#' sp.save(res_list, file = "results_list.rds")
#'
#' @export
sp.save <- function(x,
                        file = NULL,
                        dir = ".",
                        format = c("rds", "txt", "csv"),
                        name = NULL,
                        digits = 3,
                        row.names = FALSE,
                        ...) {
  format <- match.arg(format)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Auto name generation
  if (is.null(file)) {
    base <- if (!is.null(name)) name else deparse(substitute(x))
    base <- gsub("[^a-zA-Z0-9_]", "_", base)
    file <- file.path(dir, paste0(base, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", format))
  } else {
    file <- file.path(dir, file)
  }

  # Save based on format
  if (format == "rds") {
    saveRDS(x, file = file)
  } else if (format == "txt") {
    utils::write.table(round(as.data.frame(x), digits),
                       file = file, sep = "\t", quote = FALSE,
                       row.names = row.names, ...)
  } else if (format == "csv") {
    utils::write.csv(round(as.data.frame(x), digits),
                     file = file, row.names = row.names, ...)
  }

  invisible(file)
}


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

#' Calculate Additional Metrics for Statistical Estimates
#'
#' This function computes additional metrics (Width, Margin of Error, Relative Width)
#' for a data frame of estimates with confidence intervals.
#'
#' @param input A data frame containing at least the columns `"Estimate"`, `"LL"`, and `"UL"`,
#' where `"LL"` and `"UL"` represent the lower and upper bounds of a confidence interval, respectively.
#'
#' @return A data frame identical to `input`, with additional columns:
#' \describe{
#'   \item{Width}{The width of the confidence interval (`UL - LL`).}
#'   \item{MoE}{The margin of error (`Width / 2`).}
#'   \item{Relative}{The relative width of the interval (`Width / abs(Estimate)`). Returns `NA` if `Estimate` is zero.}
#' }
#'
#' @examples
#' df <- data.frame(Estimate = c(10, 0, 5), LL = c(8, -1, 4), UL = c(12, 1, 6))
#' sp.metrics(df)
#'
#' @export
sp.metrics <- function(input) {
  width <- input[, "UL"] - input[, "LL"]
  moe <- width / 2
  relative <- ifelse(input[, "Estimate"] == 0, NA, width / abs(input[, "Estimate"]))
  out <- cbind(input, Width = width, MoE = moe, Relative = relative)
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
