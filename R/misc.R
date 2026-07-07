#' Create a named list
#'
#' Creates a named list where names are automatically inferred from
#' the variable names of the arguments.
#'
#' @param ... Objects to include in the list.
#' @return A named list.
#' @export
nlist <- function(...) {
  m <- match.call()
  out <- list(...)
  no_names <- is.null(names(out))
  has_name <- if (no_names) FALSE else nzchar(names(out))
  if (all(has_name)) return(out)
  nms <- as.character(m)[-1L]
  if (no_names) {
    names(out) <- nms
  } else {
    names(out)[!has_name] <- nms[!has_name]
  }
  return(out)
}


#' Open a Stan model file in the editor
#'
#' @param mod_name Name of the model (corresponds to directory name under Models/).
#' @export
edit_model <- function(mod_name) {
  file.edit(file.path("Models", mod_name, "model", paste0(mod_name, ".stan")))
}


#' Recursively convert a matrix or array to row-major nested lists
#'
#' @param x A matrix or array.
#' @return A nested list suitable for row-major JSON serialization.
#' @noRd
.array_to_list <- function(x) {
  d <- dim(x)
  if (length(d) == 2) {
    lapply(seq_len(d[1L]), function(i) {
      row <- x[i, , drop = TRUE]
      if (is.integer(x)) as.list(as.integer(row)) else as.list(as.double(row))
    })
  } else {
    lapply(seq_len(d[1L]), function(i) {
      idx <- c(list(i), replicate(length(d) - 1L, TRUE, simplify = FALSE),
               list(drop = FALSE))
      sub_x <- do.call(`[`, c(list(x), idx))
      dim(sub_x) <- d[-1L]
      .array_to_list(sub_x)
    })
  }
}

#' Write data or initial values to JSON for CmdStan
#'
#' Writes a named list of R objects to a JSON file compatible with CmdStan's
#' \code{data file=} and \code{init=} arguments.
#'
#' @param data A named list of R objects (scalars, vectors, matrices, arrays).
#' @param file Output file path. Use \code{""} for stdout.
#' @return Invisibly returns the file path.
#' @details
#' Type handling:
#' \itemize{
#'   \item Scalars are written as JSON scalars (not length-1 arrays)
#'   \item Logical values are converted to integers (0/1)
#'   \item Factors are converted to integers
#'   \item Matrices are written as 2D arrays (list of row vectors) matching
#'     CmdStan's row-major expectation
#'   \item Arrays of dimension > 2 are written as nested lists following
#'     row-major ordering
#'   \item Data frames are converted via \code{data.matrix()}
#' }
#' @importFrom jsonlite toJSON
#' @export
write_stan_json <- function(data, file) {
  if (!is.list(data)) {
    stop("'data' must be a named list.", call. = FALSE)
  }
  if (length(data) > 0L &&
      (is.null(names(data)) || any(!nzchar(names(data))))) {
    stop("All elements of 'data' must be named.", call. = FALSE)
  }

  prep_element <- function(x) {
    if (is.logical(x)) {
      x <- as.integer(x)
    } else if (is.factor(x)) {
      x <- as.integer(x)
    } else if (is.data.frame(x)) {
      x <- data.matrix(x)
    }
    if (!is.null(dim(x)) && length(dim(x)) >= 2L) {
      x <- .array_to_list(x)
    }
    x
  }

  data <- lapply(data, prep_element)

  cat(jsonlite::toJSON(data, auto_unbox = TRUE, digits = NA, pretty = TRUE),
      "\n", file = file, sep = "")
  invisible(file)
}

#' Convert a list of same-dimension arrays to a higher-dimensional array
#'
#' @param x A list of numeric vectors, matrices, or arrays of equal dimensions.
#' @param name Optional name for error messages.
#' @return An array with one additional dimension prepended.
#' @export
list_to_array <- function(x, name = NULL) {
  list_length <- length(x)
  if (list_length == 0) return(NULL)
  all_dims <- lapply(x, function(z) dim(z) %||% length(z))
  all_equal_dim <- all(vapply(all_dims, function(d) {
    isTRUE(all.equal(d, all_dims[[1]]))
  }, logical(1)))
  if (!all_equal_dim) {
    stop("All matrices/vectors in list '", name, "' must be the same size!", call. = FALSE)
  }
  all_numeric <- all(vapply(x, is.numeric, logical(1)))
  if (!all_numeric) {
    stop("All elements in list '", name, "' must be numeric!", call. = FALSE)
  }
  element_num_of_dim <- length(all_dims[[1]])
  x <- unlist(x)
  dim(x) <- c(all_dims[[1]], list_length)
  aperm(x, c(element_num_of_dim + 1L, seq_len(element_num_of_dim)))
}


#' Highest Posterior Density Interval
#'
#' Computes the Highest Posterior Density Interval (HPDI) for MCMC samples.
#'
#' @param samples Numeric vector, matrix, data.frame, or mcmc object.
#' @param prob Probability mass to include (default 0.89).
#' @return A named numeric vector with lower and upper bounds.
#' @export
HPDI <- function(samples, prob = 0.89) {
  coerce.list <- c("numeric", "matrix", "data.frame", "integer", "array")
  if (inherits(samples, coerce.list)) {
    samples <- coda::as.mcmc(samples)
  }
  x <- sapply(prob, function(p) coda::HPDinterval(samples, prob = p))
  n <- length(prob)
  result <- rep(0, n * 2)
  for (i in 1:n) {
    low_idx <- n + 1 - i
    up_idx <- n + i
    result[low_idx] <- x[1, i]
    result[up_idx] <- x[2, i]
    names(result)[low_idx] <- paste("|", prob[i])
    names(result)[up_idx] <- paste(prob[i], "|")
  }
  return(result)
}

#' Parameter summary table
#'
#' Summarises posterior estimates with RSE and IIV.
#'
#' @param THETAhat Draws for fixed-effect parameters.
#' @param THETAomega Draws for random-effect parameters.
#' @return A data.frame with Parameter, Estimate, RSE, and IIV columns.
#' @export
parameter_table <- function(THETAhat, THETAomega) {
  hat_summary <- posterior::summarise_draws(THETAhat)
  hat_summary$RSE <- hat_summary$sd / hat_summary$median
  hat_result <- hat_summary[, c("variable", "median", "RSE")]
  names(hat_result) <- c("Parameter", "Estimate", "RSE")

  omega_summary <- posterior::summarise_draws(THETAomega)
  omega_summary$IIV <- omega_summary$sd / omega_summary$median
  omega_result <- omega_summary[, c("variable", "IIV")]
  names(omega_result) <- c("Parameter", "IIV")

  merge(hat_result, omega_result, by = "Parameter", all.x = TRUE)
}

#' Select posterior columns by pattern
#'
#' Selects columns containing "hat", "theta", or "sigma" from a posterior data.frame.
#'
#' @param x A data.frame of posterior draws.
#' @param ... Additional column name patterns to match.
#' @return A subset data.frame.
#' @export
select_hat_omega_sigma <- function(x, ...) {
  extra <- c(...)
  patterns <- c("Chain", "hat", "theta", "sigma", extra)
  cols <- grep(paste(patterns, collapse = "|"), names(x), value = TRUE)
  x[, cols, drop = FALSE]
}


#' Null-coalescing operator
#' @param x,y Any R objects.
#' @return \code{x} if not \code{NULL}, otherwise \code{y}.
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}
