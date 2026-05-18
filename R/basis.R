#' High-level function to preprocess basis
#' Assumes right censoring setting
#' @param time observed times.
#' @param status status
#' @param min_time lower bound time defult to 0.
#' @param degree splines degree default to 3.
#' @param df degrees of freedom default to 6.
#' @export
#' @examples
#' # not run
#' # rstanarm:::make_basis
preprocess_basis <- function(time, status, min_time = 0, degree = 3, df = 6) {
  min_t = min_time
  max_t = max(time)

  tt <- time[status == 1]

  ##### get b-splines basis
  bknots <- c(min_t, max_t)
  nk <- df - degree - 1
  knots <- qtile(tt, nq = nk + 1)  # evenly spaced percentiles
  iknots <- get_iknots(tt, df = df, iknots = knots,
                       degree = degree, intercept = TRUE)
  basis  <- get_basis(tt, iknots = iknots, bknots = bknots, degree = degree, type = "ms")
  basis
}

#' Calculate log_crude_event_rate
#' Helps to centre the intercept and the linear predictor
#' @param xdata dataset with time and status variables.
#' @export
calculate_log_crude_event_rate <- function(xdata, time_var = "time", status_var = "status"){

  t_beg <- rep(0, nrow(xdata)) # entry time
  t_end <- xdata[[time_var]]

  t_tmp <- sum(t_end - t_beg)
  d_tmp <- sum(!xdata[[status_var]] == 0)
  log_crude_event_rate <- log(d_tmp / t_tmp)
  log_crude_event_rate
}

#' Evaluate a spline basis matrix at the specified times
#'
#' @param time A numeric vector.
#' @param basis Info on the spline basis.
#' @param integrate A logical, should the integral of the basis be returned?
#' @return A two-dimensional array.
#' @export
basis_matrix <- function(times, basis, integrate = FALSE) {
  out <- predict(basis, times)
  if (integrate) {
    stopifnot(inherits(basis, "mSpline"))
    class(basis) <- c("matrix", "iSpline")
    out <- predict(basis, times)
  }
  as.array(out)
}

#' Return the desired spline basis for the given knot locations
#' @export
get_basis <- function(x, iknots, bknots = range(x),
                      degree = 3, intercept = TRUE,
                      type = c("bs", "is", "ms")) {
  type <- match.arg(type)
  if (type == "bs") {
    out <- splines::bs(x, knots = iknots, Boundary.knots = bknots,
                       degree = degree, intercept = intercept)
  } else if (type == "is") {
    out <- splines2::iSpline(x, knots = iknots, Boundary.knots = bknots,
                             degree = degree, intercept = intercept)
  } else if (type == "ms") {
    out <- splines2::mSpline(x, knots = iknots, Boundary.knots = bknots,
                             degree = degree, intercept = intercept)
  } else {
    stop("'type' is not yet accommodated.")
  }
  out
}

#' Return a vector with internal knots for 'x', based on evenly spaced quantiles
#'
#' @param x A numeric vector.
#' @param df The degrees of freedom. If specified, then 'df - degree - intercept'.
#'   knots are placed at evenly spaced percentiles of 'x'. If 'iknots' is
#'   specified then 'df' is ignored.
#' @return A numeric vector of internal knot locations, or NULL if there are
#'   no internal knots.
#' @export
get_iknots <- function(x, df = 6L, degree = 3L, iknots = NULL, intercept = TRUE) {

  # obtain number of internal knots
  if (is.null(iknots)) {
    nk <- df - degree - intercept
  } else {
    nk <- length(iknots)
  }

  # validate number of internal knots
  if (nk < 0) {
    stop("Number of internal knots cannot be negative.")
  }

  # obtain default knot locations if necessary
  if (is.null(iknots)) {
    iknots <- qtile(x, nq = nk + 1)  # evenly spaced percentiles
  }

  # return internal knot locations, ensuring they are positive
  validate_positive_scalar(iknots)

  return(iknots)
}

### helpers

qtile <- function (x, nq = 2) {
  if (nq > 1) {
    probs <- seq(1, nq - 1)/nq
    return(quantile(x, probs = probs))
  }
  else if (nq == 1) {
    return(NULL)
  }
  else {
    stop("'nq' must be >= 1.")
  }
}


validate_positive_scalar <- function (x, not_greater_than = NULL)
{
  nm <- deparse(substitute(x))
  if (is.null(x))
    stop(nm, " cannot be NULL", call. = FALSE)
  if (!is.numeric(x))
    stop(nm, " should be numeric", call. = FALSE)
  if (any(x <= 0))
    stop(nm, " should be postive", call. = FALSE)
  if (!is.null(not_greater_than)) {
    if (!is.numeric(not_greater_than) || (not_greater_than <=
                                          0))
      stop("'not_greater_than' should be numeric and postive")
    if (!all(x <= not_greater_than))
      stop(nm, " should less than or equal to ", not_greater_than,
           call. = FALSE)
  }
}


