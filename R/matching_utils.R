# ==============================================================================
# Matching Utilities - Shared helpers for matching layer
# ==============================================================================

#' Validate matching inputs
#'
#' @keywords internal
validate_matching_inputs <- function(left, right, vars = NULL) {
  # Check that inputs are data frames or can be coerced
  if (!is.data.frame(left)) {
    stop("left must be a data frame", call. = FALSE)
  }
  if (!is.data.frame(right)) {
    stop("right must be a data frame", call. = FALSE)
  }

  # Check for empty inputs
  if (nrow(left) == 0) {
    stop("left must have at least one row", call. = FALSE)
  }
  if (nrow(right) == 0) {
    stop("right must have at least one row", call. = FALSE)
  }

  # Check that required variables exist in both datasets
  if (!is.null(vars)) {
    missing_left <- setdiff(vars, names(left))
    if (length(missing_left) > 0) {
      stop(sprintf("left is missing required variables: %s",
                   paste(missing_left, collapse = ", ")), call. = FALSE)
    }

    missing_right <- setdiff(vars, names(right))
    if (length(missing_right) > 0) {
      stop(sprintf("right is missing required variables: %s",
                   paste(missing_right, collapse = ", ")), call. = FALSE)
    }

    # Check that variables are numeric
    for (v in vars) {
      if (!is.numeric(left[[v]])) {
        stop(sprintf("Variable '%s' in left must be numeric", v), call. = FALSE)
      }
      if (!is.numeric(right[[v]])) {
        stop(sprintf("Variable '%s' in right must be numeric", v), call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}

#' Extract and standardize IDs from data frames
#'
#' @keywords internal
extract_ids <- function(df, prefix = "id") {
  # If there's an 'id' column, use it
  if ("id" %in% names(df)) {
    return(as.character(df$id))
  }

  # Otherwise, use row names if they're meaningful
  rn <- rownames(df)
  if (!is.null(rn) && !all(rn == as.character(seq_len(nrow(df))))) {
    return(rn)
  }

  # Last resort: create sequential IDs
  paste0(prefix, "_", seq_len(nrow(df)))
}

#' Extract matching variables from data frame
#'
#' @keywords internal
extract_matching_vars <- function(df, vars) {
  mat <- as.matrix(df[, vars, drop = FALSE])

  # Check for NA/NaN/Inf
  if (any(is.na(mat))) {
    stop("Missing values (NA) not allowed in matching variables", call. = FALSE)
  }
  if (any(is.nan(mat))) {
    stop("NaN values not allowed in matching variables", call. = FALSE)
  }
  if (any(is.infinite(mat))) {
    stop("Infinite values not allowed in matching variables", call. = FALSE)
  }

  mat
}

#' Standardize block ID column name
#'
#' @keywords internal
get_block_id_column <- function(df) {
  # Check for common block ID column names
  candidates <- c("block_id", "blockid", "block", "stratum", "stratum_id")

  found <- intersect(candidates, names(df))
  if (length(found) > 0) {
    return(found[1])
  }

  NULL
}

#' Check if data frame has blocking information
#'
#' @keywords internal
has_blocks <- function(df) {
  !is.null(get_block_id_column(df))
}

#' Validate weights parameter
#'
#' @keywords internal
validate_weights <- function(weights, vars) {
  if (is.null(weights)) {
    return(rep(1, length(vars)))
  }

  if (is.numeric(weights)) {
    if (length(weights) != length(vars)) {
      stop(sprintf("weights must have length %d (one per variable)", length(vars)),
           call. = FALSE)
    }
    if (any(weights < 0)) {
      stop("weights must be non-negative", call. = FALSE)
    }
    return(weights)
  }

  # Named weights
  if (is.list(weights) || (is.numeric(weights) && !is.null(names(weights)))) {
    w_vec <- rep(1, length(vars))
    names(w_vec) <- vars

    for (nm in names(weights)) {
      if (!(nm %in% vars)) {
        stop(sprintf("weights contains unknown variable: %s", nm), call. = FALSE)
      }
      w_vec[nm] <- weights[[nm]]
    }
    return(as.numeric(w_vec))
  }

  stop("weights must be a numeric vector or named list", call. = FALSE)
}

#' Validate calipers parameter
#'
#' @keywords internal
validate_calipers <- function(calipers, vars) {
  if (is.null(calipers)) {
    return(NULL)
  }

  if (!is.list(calipers) && !is.numeric(calipers)) {
    stop("calipers must be a named numeric vector or list", call. = FALSE)
  }

  if (is.null(names(calipers))) {
    stop("calipers must be named (variable names)", call. = FALSE)
  }

  # Check that all caliper variables exist in vars
  unknown <- setdiff(names(calipers), vars)
  if (length(unknown) > 0) {
    stop(sprintf("calipers contains unknown variables: %s",
                 paste(unknown, collapse = ", ")), call. = FALSE)
  }

  # Check that values are positive
  vals <- as.numeric(calipers)
  if (any(vals <= 0)) {
    stop("caliper values must be positive", call. = FALSE)
  }

  calipers
}
