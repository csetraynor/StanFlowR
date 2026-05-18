#' Read CmdStan posterior output
#'
#' Reads CmdStan CSV output files for a model, combines chains into a single data.frame.
#'
#' @param mod Model name.
#' @param tag Run tag used in output file naming.
#' @param chains Optional integer vector specifying which chains to read.
#' @param modelDir Model directory (default \code{"Models"}).
#' @return A data.frame with a \code{Chain} column identifying each chain.
#' @export
read_post <- function(mod, tag, chains = NULL, modelDir = "Models") {
  model_dir <- get_model_file(mod, modelDir)
  all_files <- list.files(model_dir, pattern = paste0("_", tag, "_"),
                          include.dirs = FALSE, recursive = TRUE, full.names = FALSE)
  csv_files <- grep("\\.csv$", all_files, value = TRUE)
  if (length(csv_files) == 0) {
    stop("No CSV files found for model '", mod, "' with tag '", tag, "'.", call. = FALSE)
  }

  full_paths <- file.path(model_dir, csv_files)
  pars <- colnames_cmdstan(full_paths[1])

  if (!is.null(chains)) {
    full_paths <- full_paths[chains]
  }

  post_list <- lapply(seq_along(full_paths), function(i) {
    df <- select_vars_cmdstan(full_paths[i], vars = pars)
    df$Chain <- as.character(i)
    df
  })

  do.call(rbind, post_list)
}
