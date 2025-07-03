# spTools
## Input Handling


#' Extract Row or Column as Named Vector from Data Frame or Matrix
#'
#' This function extracts a single row or column from a matrix or data frame
#' by name or index. It automatically detects whether the input corresponds to
#' a row or a column and returns the result as a named vector. The names are
#' preserved from the opposite dimension (column names for rows, row names for columns).
#'
#' @param data A matrix or data frame from which to extract data.
#' @param index A character or numeric value specifying the row or column to extract.
#'   If character, it is matched against row names first, then column names.
#'   If numeric, it is interpreted as a row index first, then a column index.
#'
#' @return A named vector containing the extracted row or column values.
#'
#' @examples
#' input <- data.frame(
#'   m = c(1.1, 2.2, 3.3),
#'   sd = c(0.1, 0.2, 0.3),
#'   n = c(10, 20, 30),
#'   row.names = c("group1", "group2", "group3")
#' )
#'
#' # Extract column "m" as named vector (names are row names)
#' sp.extract(input, "m")
#'
#' # Extract row "group2" as named vector (names are column names)
#' sp.extract(input, "group2")
#'
#' @export
sp.extract <- function(data, index) {
  if (is.character(index) && index %in% rownames(data)) {
    vec <- data[index, ]           # drop=TRUE by default → atomic vector
    nms <- colnames(data)
    if (!is.null(nms) && length(vec) == length(nms)) {
      names(vec) <- nms
    }
    return(vec)
  }
  
  if (is.character(index) && index %in% colnames(data)) {
    vec <- data[, index]           # drop=TRUE by default
    nms <- rownames(data)
    if (!is.null(nms) && length(vec) == length(nms)) {
      names(vec) <- nms
    }
    return(vec)
  }
  
  if (is.numeric(index)) {
    if (index <= nrow(data)) {
      vec <- data[index, ]         # drop=TRUE by default
      nms <- colnames(data)
      if (!is.null(nms) && length(vec) == length(nms)) {
        names(vec) <- nms
      }
      return(vec)
    } else if (index <= ncol(data)) {
      vec <- data[, index]         # drop=TRUE by default
      nms <- rownames(data)
      if (!is.null(nms) && length(vec) == length(nms)) {
        names(vec) <- nms
      }
      return(vec)
    }
  }
  
  stop("Invalid index: not found in row or column names, or out of bounds.")
}


#' Compute Descriptive Statistics for Numeric Variables
#'
#' Calculates basic descriptive statistics (N, mean, and standard deviation) for numeric variables
#' in a data frame, optionally grouped by a factor using a formula interface.
#'
#' @param data A data frame containing the variables of interest.
#' @param vars Optional character vector specifying the names of numeric variables to include.
#'   If `NULL`, all numeric variables in `data` are used.
#' @param formula Optional formula specifying a grouping variable (e.g., `~ group`).
#'   If provided, statistics are computed separately for each group.
#'
#' @return If `formula` is `NULL`, returns a matrix of descriptive statistics (N, mean, and SD)
#'   for each variable. If `formula` is provided, returns a named list of such matrices, one for each group.
#'
#' @examples
#' data(iris)
#'
#' # Descriptive statistics for all numeric variables
#' describe(iris)
#'
#' # Descriptive statistics for specific variables
#' sp.describe(iris, vars = c("Sepal.Length", "Petal.Length"))
#'
#' # Grouped descriptive statistics by Species
#' sp.describe(iris, formula = ~ Species)
#'
#' @export
sp.describe <- function(data, vars = NULL, formula = NULL) {
  # Get variable names
  if (is.null(vars)) {
    vars <- names(data)
  }
  
  # Handle formula-based grouping
  if (!is.null(formula)) {
    groupvar <- all.vars(formula)[1]
    if (!(groupvar %in% names(data))) {
      stop("Grouping variable not found in data.")
    }
    groups <- unique(data[[groupvar]])
    
    # Only use numeric variables (excluding groupvar)
    measurevars <- vars[vars %in% names(data) & sapply(data[vars], is.numeric)]
    
    results <- list()
    for (g in groups) {
      subset <- data[data[[groupvar]] == g, , drop = FALSE]
      stats <- t(sapply(subset[measurevars], function(x) {
        c(N = sum(!is.na(x)), 
        M = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE))
      }))
      results[[as.character(g)]] <- stats
    }
    return(results)
  }
  
  # If no formula, just compute for selected variables
  vars <- vars[vars %in% names(data) & sapply(data[vars], is.numeric)]
  stats <- t(sapply(data[vars], function(x) {
    c(N = sum(!is.na(x)),
    M = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE))
  }))
  return(stats)
}


#' Compute Correlation or Covariance Matrices
#'
#' Computes a correlation or covariance matrix for selected numeric variables in a data frame,
#' optionally grouped by a factor using a formula interface.
#'
#' @param data A data frame containing the variables of interest.
#' @param vars Optional character vector specifying the names of numeric variables to include.
#'   If `NULL`, all numeric variables in `data` are used.
#' @param formula Optional formula specifying a grouping variable, e.g., `~ group`.
#'   If provided, the correlation or covariance matrix is computed separately for each group.
#' @param type Character string indicating whether to compute `"cor"` (correlation) or `"cov"` (covariance).
#'   Default is `"cor"`.
#' @param method Character string specifying the correlation method: one of `"pearson"` (default), `"spearman"`, or `"kendall"`.
#'   Ignored if `type = "cov"`.
#'
#' @return If `formula` is `NULL`, returns a correlation or covariance matrix for the selected variables.
#' If `formula` is provided, returns a named list of matrices, one for each group level.
#'
#' @examples
#' data(iris)
#'
#' # Correlation matrix for all numeric variables
#' sp.correlate(iris)
#'
#' # Covariance matrix for specific variables
#' sp.correlate(iris, vars = c("Sepal.Length", "Petal.Length"), type = "cov")
#'
#' # Grouped correlation matrices by Species
#' sp.correlate(iris, formula = ~ Species)
#'
#' @export
sp.correlate <- function(data, vars = NULL, formula = NULL, type = "cor", method = "pearson") {
  # Validate type
  type <- match.arg(type, choices = c("cor", "cov"))

  # Variable selection
  if (is.null(vars)) {
    vars <- names(data)
  }

  # Filter numeric variables
  vars <- vars[vars %in% names(data) & sapply(data[vars], is.numeric)]

  # Internal computation: either correlation or covariance
  compute_matrix <- function(df) {
    if (type == "cor") {
      return(cor(df, use = "pairwise.complete.obs", method = method))
    } else {
      return(cov(df, use = "pairwise.complete.obs"))
    }
  }

  # Handle grouping
  if (!is.null(formula)) {
    groupvar <- all.vars(formula)[1]
    if (!(groupvar %in% names(data))) {
      stop("Grouping variable not found in data.")
    }
    groups <- unique(data[[groupvar]])
    results <- list()

    for (g in groups) {
      subset <- data[data[[groupvar]] == g, , drop = FALSE]
      results[[as.character(g)]] <- compute_matrix(subset[vars])
    }

    return(results)
  }

  # No grouping: compute directly
  return(compute_matrix(data[vars]))
}
