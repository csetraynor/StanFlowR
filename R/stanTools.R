#' Convert mcmc.list to data.frame
#'
#' Converts an mcmc.list array to a data.frame with chain and iteration columns.
#'
#' @param x An mcmc.list object.
#' @param ... Additional arguments passed to \code{as.data.frame}.
#' @return A data.frame with \code{chain} and \code{iteration} columns prepended.
#' @export
getSimsTable <- function(x, ...) {
  nChains <- dim(x)[2]
  nPost <- dim(x)[1]
  df <- as.data.frame(x, ...)
  df$chain <- rep(1:nChains, each = nPost)
  df$iteration <- rep(1:nPost, nChains)
  df
}

#' Summary method for mcmc.list
#'
#' Custom summary for mcmc.list objects producing mean, SD, quantiles,
#' and time-series SE.
#'
#' @param object An mcmc.list object.
#' @param quantiles Numeric vector of quantiles to compute.
#' @param ... Additional arguments (currently ignored).
#' @return An object of class \code{summary.mcmc}.
#' @export
summary.mcmc.list <- function(object, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), ...) {
  x <- coda::mcmc.list(object)
  statnames <- c("Mean", "SD", "Naive SE", "Time-series SE")
  varstats <- matrix(nrow = coda::nvar(x), ncol = length(statnames),
                     dimnames = list(coda::varnames(x), statnames))
  xtsvar <- matrix(nrow = coda::nchain(x), ncol = coda::nvar(x))
  if (is.matrix(x[[1]])) {
    for (i in 1:coda::nchain(x)) {
      for (j in 1:coda::nvar(x)) {
        xtsvar[i, j] <- stats::spectrum(x[[i]][, j], plot = FALSE)$spec[1] / length(x[[i]][, j])
      }
    }
    xlong <- do.call("rbind", x)
  } else {
    for (i in 1:coda::nchain(x)) {
      xtsvar[i, ] <- stats::spectrum(x[[i]], plot = FALSE)$spec[1] / length(x[[i]])
    }
    xlong <- as.matrix(x)
  }
  xmean <- apply(xlong, 2, mean, na.rm = TRUE)
  xvar <- apply(xlong, 2, var, na.rm = TRUE)
  xtsvar <- apply(xtsvar, 2, mean, na.rm = TRUE)
  varquant <- t(apply(xlong, 2, quantile, quantiles, na.rm = TRUE))
  varstats[, 1] <- xmean
  varstats[, 2] <- sqrt(xvar)
  varstats[, 3] <- sqrt(xvar / (coda::niter(x) * coda::nchain(x)))
  varstats[, 4] <- sqrt(xtsvar / (coda::niter(x) * coda::nchain(x)))
  varquant <- drop(varquant)
  varstats <- drop(varstats)
  out <- list(statistics = varstats, quantiles = varquant,
              start = start(x), end = end(x), thin = coda::thin(x),
              nchain = coda::nchain(x))
  class(out) <- "summary.mcmc"
  return(out)
}


#' Column-wise variance
#'
#' Computes variance for each column of a matrix.
#'
#' @param a A numeric matrix.
#' @return A numeric vector of column variances.
#' @export
colVars <- function(a) {
  apply(a, 2, var)
}


#' Check if a name is a legal Stan variable name
#'
#' Validates that a name does not conflict with Stan or C++ reserved keywords.
#'
#' @param name A character string to check.
#' @return Logical; \code{TRUE} if the name is legal in Stan.
#' @export
is_legal_stan_vname <- function(name) {
  stan_kw1 <- c("for", "in", "while", "repeat", "until", "if", "then", "else", "true", "false")
  stan_kw2 <- c("int", "real", "vector", "simplex", "ordered", "positive_ordered",
                 "row_vector", "matrix", "corr_matrix", "cov_matrix", "lower", "upper")
  stan_kw3 <- c("model", "data", "parameters", "quantities", "transformed", "generated")
  cpp_kw <- c("alignas", "alignof", "and", "and_eq", "asm", "auto", "bitand", "bitor",
              "bool", "break", "case", "catch", "char", "char16_t", "char32_t", "class",
              "compl", "const", "constexpr", "const_cast", "continue", "decltype", "default",
              "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit", "export",
              "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int",
              "long", "mutable", "namespace", "new", "noexcept", "not", "not_eq", "nullptr",
              "operator", "or", "or_eq", "private", "protected", "public", "register",
              "reinterpret_cast", "return", "short", "signed", "sizeof", "static",
              "static_assert", "static_cast", "struct", "switch", "template", "this",
              "thread_local", "throw", "true", "try", "typedef", "typeid", "typename",
              "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t",
              "while", "xor", "xor_eq")

  if (grepl("\\.", name)) return(FALSE)
  if (grepl("^\\d", name)) return(FALSE)
  if (grepl("__$", name)) return(FALSE)
  if (name %in% stan_kw1) return(FALSE)
  if (name %in% stan_kw2) return(FALSE)
  if (name %in% stan_kw3) return(FALSE)
  !name %in% cpp_kw
}


#' Check if numeric values are whole numbers
#' @param x A numeric vector.
#' @return Logical; \code{TRUE} if all finite values are integers.
#' @noRd
real_is_integer <- function(x) {
  if (length(x) < 1L) return(TRUE)
  if (any(is.infinite(x)) || any(is.nan(x))) return(FALSE)
  all(floor(x) == x)
}

#' Write objects in Stan rdump or JSON format
#'
#' Writes R objects (vectors, matrices, arrays) in Stan rdump text format or
#' JSON format suitable for CmdStan's \code{data file=} and \code{init=} arguments.
#'
#' @param list Character vector of object names to dump.
#' @param file Output file path (default \code{""} for stdout).
#' @param append Logical; append to existing file? (rdump only)
#' @param envir Environment from which to retrieve objects.
#' @param width Line width for formatting (rdump only).
#' @param quiet Logical; suppress warnings?
#' @param format Output format: \code{"json"} (default) or \code{"rdump"}.
#' @return Invisibly, a character vector of valid names written.
#' @export
new_stanrdump <- function(list, file = "", append = FALSE, envir = parent.frame(),
                          width = options("width")$width, quiet = FALSE,
                          format = c("json", "rdump")) {
  format <- match.arg(format)

  ex <- sapply(list, exists, envir = envir)
  if (!all(ex)) {
    notfound_list <- list[!ex]
    if (!quiet)
      warning("objects not found: ", paste(notfound_list, collapse = ", "))
  }
  list <- list[ex]
  if (length(list) == 0L) return(invisible(character()))

  for (x in list) {
    if (!is_legal_stan_vname(x) && !quiet)
      warning("variable name ", x, " is not allowed in Stan")
  }

  if (format == "json") {
    out_file <- file
    if (nzchar(out_file)) {
      out_file <- sub("\\.(R|rdump)$", ".json", out_file)
    }
    data_list <- setNames(
      lapply(list, function(v) get(v, envir = envir)),
      list
    )
    write_stan_json(data_list, file = out_file)
    return(invisible(list))
  }

  # --- rdump path ---
  if (is.character(file)) {
    if (nzchar(file)) {
      file <- file(file, ifelse(append, "a", "w"))
      on.exit(close(file), add = TRUE)
    } else {
      file <- stdout()
    }
  }

  l2 <- NULL
  addnlpat <- paste0("(.{1,", width, "})(\\s|$)")
  for (v in list) {
    vv <- get(v, envir)
    if (is.data.frame(vv)) {
      vv <- data.matrix(vv)
    } else if (is.list(vv)) {
      vv <- list_to_array(vv)
    } else if (is.logical(vv)) {
      mode(vv) <- "integer"
    } else if (is.factor(vv)) {
      vv <- as.integer(vv)
    }
    if (!is.numeric(vv)) {
      if (!quiet) warning("variable ", v, " is not supported for dumping.")
      next
    }
    if (!is.integer(vv) && max(abs(vv)) < .Machine$integer.max &&
        real_is_integer(vv))
      storage.mode(vv) <- "integer"
    if (is.vector(vv)) {
      if (length(vv) == 0) {
        cat(v, " <- integer(0)\n", file = file, sep = "")
        next
      }
      if (length(vv) == 1) {
        cat(v, " <- ", as.character(vv), "\n", file = file, sep = "")
        next
      }
      str <- paste0(v, " <- \nc(", paste(vv, collapse = ", "), ")")
      str <- gsub(addnlpat, "\\1\n", str, perl = TRUE)
      cat(str, file = file)
      l2 <- c(l2, v)
      next
    }
    if (is.matrix(vv) || is.array(vv)) {
      l2 <- c(l2, v)
      vvdim <- dim(vv)
      cat(v, " <- \n", file = file, sep = "")
      if (length(vv) == 0) {
        str <- "structure(integer(0), "
      } else {
        str <- paste0("structure(c(", paste(as.vector(vv), collapse = ", "), "),")
      }
      str <- gsub(addnlpat, "\\1\n", str, perl = TRUE)
      cat(str, ".Dim = c(", paste(vvdim, collapse = ", "), "))\n",
          file = file, sep = "")
      next
    }
  }
  invisible(l2)
}
