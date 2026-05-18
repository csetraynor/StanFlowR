#' Extract log-likelihood from posterior
#'
#' Extracts \code{log_lik} columns from a posterior data.frame and reshapes
#' into a 3D array (draws x chains x observations) suitable for the \code{loo} package.
#'
#' @param post A data.frame of posterior draws with a \code{Chain} column
#'   and columns starting with \code{log_lik}.
#' @return A 3-dimensional array: draws x chains x observations.
#' @export
cmdstan_extract_log_lik <- function(post) {
  log_lik_cols <- grep("^log_lik", names(post), value = TRUE)
  chain_col <- post[["Chain"]]
  if (is.null(chain_col)) stop("'post' must have a 'Chain' column.", call. = FALSE)

  chains <- unique(chain_col)
  nchains <- length(chains)
  n <- length(log_lik_cols)

  chain_list <- lapply(chains, function(ch) {
    as.matrix(post[chain_col == ch, log_lik_cols, drop = FALSE])
  })

  ndraws <- nrow(chain_list[[1]])
  array_stack <- array(unlist(chain_list), dim = c(ndraws, n, nchains))
  aperm(array_stack, c(1, 3, 2))
}


#' Check posterior completeness
#'
#' Asserts that a posterior data.frame has the expected number of rows.
#'
#' @param post A data.frame of posterior draws.
#' @param ndraws Expected total number of draws.
#' @return Logical \code{TRUE} if counts match; otherwise an error.
#' @export
check_complete_post <- function(post, ndraws) {
  if (nrow(post) != ndraws) {
    stop("Expected ", ndraws, " draws but found ", nrow(post), ".", call. = FALSE)
  }
  invisible(TRUE)
}


#' Delete CSV output files by tag
#'
#' Removes CSV files matching a tag from a model's output directory.
#' Optionally checks that the posterior is complete before deleting.
#'
#' @param mod_name Model name.
#' @param label_tag Tag used in file naming.
#' @param modelDir Path to model directory.
#' @param ndraws Optional expected draw count for safety check.
#' @return Invisibly returns \code{NULL}.
#' @export
clear_csv_tag <- function(mod_name, label_tag, modelDir, ndraws = NULL) {
  if (missing(modelDir)) {
    modelDir <- file.path("Models", mod_name, "model")
  }
  foo <- list.files(path = file.path(modelDir, mod_name),
                    pattern = "*.csv", full.names = TRUE)
  foo_files <- grep(label_tag, foo, value = TRUE)

  if (!is.null(ndraws)) {
    if (check_complete_post(read_post(mod_name, label_tag), ndraws)) {
      warning("Removing complete posterior")
      user_input <- readline(prompt = "Do you want to continue? (yes/no) ")
      if (tolower(user_input) != "yes") {
        message("Deletion cancelled.")
        return(invisible(NULL))
      }
    }
  }

  if (length(foo_files) > 0) {
    file.remove(foo_files)
  }
  invisible(NULL)
}
