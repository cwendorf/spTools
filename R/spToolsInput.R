# spTools
## Input Handling

### Extract Vector

#' Extract a Row or Column from a Matrix or Data Frame
#'
#' Extracts a row or column from a matrix or data frame using a name or index,
#' returning a named numeric vector.
#'
#' @param data A matrix or data frame.
#' @param which A character (row or column name) or numeric index to extract.
#' @param margin Either `"row"` or `"column"` (or `1` or `2`) to specify the direction.
#' @param required Optional character vector of names that must be present in the result.
#'
#' @return A named numeric vector.
#'
#' @examples
#' input <- data.frame(
#'   m  = c(50, 60),
#'   sd = c(10, 12),
#'   n  = c(25, 30),
#'   alpha = c(0.05, 0.01)
#' )
#'
#' # Extract first row
#' extract_vector(input, which = 1, margin = "row")
#'
#' # Extract column by name
#' extract_vector(input, which = "m", margin = "column")
#'
#' # Extract row by name
#' rownames(input) <- c("Group1", "Group2")
#' extract_vector(input, which = "Group2", margin = "row")
#'
#' @export
extract_vector <- function(data, which = 1, margin = "row", required = NULL) {
  if (!is.data.frame(data)) data <- as.data.frame(data)

  margin <- tolower(margin)
  margin <- if (margin %in% c("row", 1)) 1 else if (margin %in% c("column", 2)) 2 else stop("Margin must be 'row', 'column', 1, or 2.")

  if (margin == 1) {
    # Row extraction
    row_idx <- if (is.numeric(which)) {
      if (which > nrow(data)) stop("Row index out of bounds.")
      which
    } else if (is.character(which)) {
      idx <- match(which, rownames(data))
      if (is.na(idx)) stop("Row name not found.")
      idx
    } else {
      stop("Invalid row specification.")
    }

    result <- unlist(data[row_idx, , drop = FALSE])
    result <- as.numeric(result)
    names(result) <- names(data)

  } else if (margin == 2) {
    # Column extraction
    col_idx <- if (is.numeric(which)) {
      if (which > ncol(data)) stop("Column index out of bounds.")
      which
    } else if (is.character(which)) {
      idx <- match(which, names(data))
      if (is.na(idx)) stop("Column name not found.")
      idx
    } else {
      stop("Invalid column specification.")
    }

    result <- data[[col_idx]]
    result <- as.numeric(result)
    names(result) <- rownames(data)
  }

  # Validate required names
  if (!is.null(required)) {
    if (!all(required %in% names(result))) {
      missing <- setdiff(required, names(result))
      stop("Missing required names: ", paste(missing, collapse = ", "))
    }
  }

  return(result)
}
