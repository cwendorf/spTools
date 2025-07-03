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
#' sp.console(df, digits = 2)
#'
#' @export
sp.console <- function(results, digits = 3, padding = 2, width = NULL, spaced = TRUE, ...) {
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


#' Format Output as Markdown Table
#'
#' Formats numeric results (data frames or lists of data frames) into markdown tables, suitable for display in RMarkdown or other markdown-rendering environments.
#'
#' @param results A data frame or a list of data frames/matrices.
#' @param digits Number of decimal digits to round to.
#' @param spaced Logical; whether to print extra newlines between elements.
#' @param ... Additional arguments passed to `knitr::kable()`.
#'
#' @return The markdown-formatted table(s) are printed and returned (invisibly) as character strings or a list of strings.
#'
#' @examples
#' df <- data.frame(x = c(1.2345, 2.3456), y = c(3.4567, 4.5678))
#' sp.markdown(df, digits = 2)
#'
#' @export
sp.markdown <- function(results, digits = 3, spaced = TRUE, ...) {
  format_one <- function(res) {
    df <- as.data.frame(round(res, digits))
    df[] <- lapply(df, function(col) {
      if (is.numeric(col)) {
        sprintf(paste0("%.", digits, "f"), col)
      } else {
        as.character(col)
      }
    })
    knitr::kable(df, format = "markdown", align = rep("r", ncol(df)), ...)
  }

  if (is.list(results) && !is.data.frame(results)) {
    out_list <- lapply(results, format_one)
    names(out_list) <- names(results)  # preserve names

    for (nm in names(out_list)) {
      if (spaced) cat("\n")
      cat(nm, "\n", strrep("-", nchar(nm)), "\n", sep = "")
      cat(out_list[[nm]], sep = "\n")
      if (spaced) cat("\n")
    }

    invisible(out_list)
  } else {
    out <- format_one(results)
    if (spaced) cat("\n")
    cat(out, sep = "\n")
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


#' Add test statistic, df, and p-value to Bonett-style CI output
#'
#' @param results 1-row matrix output from Bonett-style CI functions (with Estimate, SE, LL, UL)
#' @param df Optional degrees of freedom (if NULL, try to infer from n)
#' @param n Optional sample size to infer df if df is missing
#' @param null_value Hypothesized null value for test statistic (default 0)
#'
#' @return 1-row matrix with columns:
#' Estimate, SE, Statistic, df, p, LL, UL
#'
#' @export
sp.infer <- function(results, df = NULL, n = NULL, null_value = 0) {
  required_cols <- c("Estimate", "SE", "LL", "UL")
  if (!all(required_cols %in% colnames(results))) {
    stop("Input must contain columns: Estimate, SE, LL, UL")
  }

  results <- as.matrix(results)
  nr <- nrow(results)

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

  if (length(null_value) == 1) null_value <- rep(null_value, nr)
  if (length(null_value) != nr) stop("Length of null_value must be 1 or equal to number of rows")

  est <- as.numeric(results[, "Estimate"])
  se <- as.numeric(results[, "SE"])
  ll <- as.numeric(results[, "LL"])
  ul <- as.numeric(results[, "UL"])

  stat <- (est - null_value) / se

  is_t <- !is.na(df) & is.finite(df) & df > 0

  p <- numeric(nr)
  for (i in seq_len(nr)) {
    if (is_t[i]) {
      p[i] <- 2 * pt(-abs(stat[i]), df[i])
    } else {
      p[i] <- 2 * pnorm(-abs(stat[i]))
      df[i] <- NA
    }
  }

  out <- cbind(
    Estimate = est,
    SE = se,
    Statistic = stat,
    df = df,
    p = p,
    LL = ll,
    UL = ul
  )

  rownames(out) <- rownames(results)

  return(out)
}
