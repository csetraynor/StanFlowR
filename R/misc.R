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


#' Write data in Stan JSON format
#'
#' Writes a named list of data to a JSON file suitable for CmdStan input.
#'
#' @param data A named list of R objects (numeric vectors, matrices, data.frames, logicals, or lists).
#' @param file Path to the output JSON file.
#' @export
write_stan_json <- function(data, file) {
  if (!is.character(file) || !nzchar(file)) {
    stop("The supplied filename is invalid!", call. = FALSE)
  }
  for (var_name in names(data)) {
    var <- data[[var_name]]
    if (!(is.numeric(var) || is.factor(var) || is.logical(var) ||
          is.data.frame(var) || is.list(var))) {
      stop("Variable '", var_name, "' is of invalid type.", call. = FALSE)
    }
    if (is.logical(var)) {
      mode(var) <- "integer"
    } else if (is.data.frame(var)) {
      var <- data.matrix(var)
    } else if (is.list(var)) {
      var <- list_to_array(var, var_name)
    }
    data[[var_name]] <- var
  }
  jsonlite::write_json(data, path = file, auto_unbox = TRUE,
                       factor = "integer", digits = NA, pretty = TRUE)
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
