# spTools
## Object Manipulation


#' Filter Columns from Data Frame, Matrix, or List
#'
#' Filters specified columns from a data frame, matrix, or list of such objects.
#'
#' @param out A data frame, matrix, atomic vector, or a list of such objects.
#' @param cols A numeric vector of column indices or a character vector of column names to keep.
#'
#' @return A filtered object with only the specified columns. The output will match the input type (matrix, data frame, or list).
#'
#' @examples
#' df <- data.frame(Estimate = 1:3, SE = 0.1, Extra = 4:6)
#' sp.columns(df, cols = c("Estimate", "SE"))
#'
#' @export
sp.columns <- function(out, cols = c("Estimate", "SE", "df", "LL", "UL")) {
  filter_one <- function(obj) {
    # Atomic vector → 1-row data frame
    if (is.atomic(obj) && !is.data.frame(obj) && !is.matrix(obj)) {
      obj <- as.data.frame(t(obj))
      colnames(obj) <- paste0("V", seq_len(ncol(obj)))
    }

    # Matrix → convert to data frame
    is_matrix <- is.matrix(obj)
    if (is_matrix) {
      rown <- rownames(obj)
      obj <- as.data.frame(obj, stringsAsFactors = FALSE)
    }

    if (!is.data.frame(obj)) return(NULL)

    # Filter by column number or name
    result <- if (is.numeric(cols)) {
      idx <- cols[cols >= 1 & cols <= ncol(obj)]
      obj[, idx, drop = FALSE]
    } else if (is.character(cols)) {
      keep <- intersect(cols, colnames(obj))
      obj[, keep, drop = FALSE]
    } else {
      obj
    }

    # Return in original type
    if (is_matrix) {
      result <- as.matrix(result)
      if (!is.null(rown)) rownames(result) <- rown
    }

    result
  }

  if (is.list(out)) {
    result <- lapply(out, filter_one)
    names(result) <- names(out)
    return(result)
  }

  filter_one(out)
}


#' Filter Rows from Data Frame, Matrix, or List
#'
#' Filters specified rows from a data frame, matrix, or list of such objects.
#'
#' @param out A data frame, matrix, atomic vector, or a list of such objects.
#' @param rows A numeric vector of row indices or a character vector of row names to retain. If `NULL`, all rows are returned.
#'
#' @return A filtered object with only the specified rows. The output will match the input type (matrix, data frame, or list).
#'
#' @examples
#' df <- data.frame(A = 1:5, B = letters[1:5])
#' sp.rows(df, rows = c(1, 3))
#'
#' @export
sp.rows <- function(out, rows = NULL) {
  filter_one <- function(obj) {
    # Atomic vector → data frame
    if (is.atomic(obj) && !is.data.frame(obj) && !is.matrix(obj)) {
      obj <- data.frame(value = obj)
    }

    # Matrix → convert to data frame, preserve names
    is_matrix <- is.matrix(obj)
    if (is_matrix) {
      obj_df <- as.data.frame(obj, stringsAsFactors = FALSE)
    } else {
      obj_df <- obj
    }

    if (!is.data.frame(obj_df)) return(NULL)

    # Default: all rows
    result <- if (is.null(rows)) {
      obj_df
    } else if (is.numeric(rows)) {
      idx <- rows[rows >= 1 & rows <= nrow(obj_df)]
      obj_df[idx, , drop = FALSE]
    } else if (is.character(rows)) {
      if (!is.null(rownames(obj_df))) {
        obj_df[rownames(obj_df) %in% rows, , drop = FALSE]
      } else {
        obj_df[0, , drop = FALSE]
      }
    } else {
      obj_df
    }

    # Restore to matrix if original was matrix
    if (is_matrix) {
      result_matrix <- as.matrix(result)
      colnames(result_matrix) <- colnames(result)  # restore col names
      rownames(result_matrix) <- rownames(result)  # preserve actual filtered row names
      return(result_matrix)
    }

    result
  }

  if (is.list(out)) {
    result <- lapply(out, filter_one)
    names(result) <- names(out)
    return(result)
  }

  filter_one(out)
}


#' Label One or More Objects
#'
#' Assigns names to objects, useful for labeling results before formatting or saving.
#'
#' This function helps attach meaningful names to results—either to a single object
#' (which is wrapped in a named list) or to a list of objects (which are directly named).
#' It uses non-standard evaluation to capture names from the call.
#'
#' @param x A single object or a list of objects to be labeled.
#' @param ... The name(s) to assign to the object(s), typically unquoted.
#'
#' @return A named list. If `x` is a list, it is returned with names applied.
#'   If `x` is a single object, it is wrapped in a named list.
#'
#' @examples
#' # Label a single result
#' result <- list(Estimate = 0.5, SE = 0.1)
#' labeled <- sp.label(result, my_result)
#'
#' # Label multiple results in a list
#' list_of_results <- list(
#'   list(Estimate = 0.5),
#'   list(Estimate = 0.7)
#' )
#' labeled_list <- sp.label(list_of_results, t1, t2)
#'
#' @export
sp.label <- function(x, ...) {
  nm <- as.character(substitute(list(...)))[-1L]

  if (is.list(x) && !is.data.frame(x)) {
    if (length(nm) != length(x)) {
      stop("Number of names does not match the number of list elements")
    }
    names(x) <- nm
    x
  } else {
    if (length(nm) != 1) {
      stop("Single object requires exactly one name")
    }
    # Wrap single object in a named list
    setNames(list(x), nm)
  }
}
