# spTools
## Output Functions

### Format Console Output

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
#' format_console(df, digits = 2)
#'
#' @export
format_console <- function(results, digits = 3, padding = 2, width = NULL, spaced = TRUE, ...) {
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

### Format Markdown Output

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
#' format_markdown(df, digits = 2)
#'
#' @export
format_markdown <- function(results, digits = 3, spaced = TRUE, ...) {
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


### Save Output

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
#' save_output(result)
#'
#' # Save as TXT with a custom filename
#' save_output(result, file = "ci_output.txt", format = "txt")
#'
#' # Save as CSV with an auto-generated name, into a subfolder
#' save_output(result, format = "csv", dir = "outputs", name = "ci_mean1")
#'
#' # Save a list of results as RDS
#' res_list <- list(t1 = result, t2 = result)
#' save_output(res_list, file = "results_list.rds")
#'
#' @export
save_output <- function(x,
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
