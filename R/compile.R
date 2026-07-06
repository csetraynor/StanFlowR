## StanFlowR: functions to compile and run CmdStan models

#' Resolve CmdStan directory path
#'
#' Follows hierarchy: explicit argument > option > env var > error.
#' @param dir Optional explicit path.
#' @return Resolved CmdStan path.
#' @noRd
.resolve_cmdstan_path <- function(dir = NULL) {
  if (!is.null(dir) && nzchar(dir)) return(dir)
  opt <- getOption("StanFlowR.cmdstan_path")
  if (!is.null(opt) && nzchar(opt)) return(opt)
  env <- Sys.getenv("CMDSTAN_PATH", unset = "")
  if (nzchar(env)) return(env)
  stop("CmdStan path not found. Set via argument, ",
       "options(StanFlowR.cmdstan_path = '/path/to/cmdstan'), ",
       "or Sys.setenv(CMDSTAN_PATH = '/path/to/cmdstan').",
       call. = FALSE)
}


#' Prepare init/data for a CmdStan run
#'
#' Shared init-handling logic used by all user-facing runners.
#' @noRd
.prepare_run <- function(chain, modelName, init, init_dir, data, tag, seed,
                         init_format = c("json", "rdump")) {
  init_format <- match.arg(init_format)
  SEED <- seed + chain
  set.seed(SEED)
  init_file <- NULL

  if (!is.null(init) && !init_dir) {
    write_inits(init, chain, modelName, label = tag, format = init_format)
    init_file <- get_init_file(modelName, "Models", chain, tag = tag,
                               format = init_format)
  }

  if (is.null(init) && init_dir) {
    init_file <- get_init_file(modelName, "Models", chain, tag = tag,
                               format = init_format)
  }

  if (is.null(data)) {
    data <- file.path(getwd(), "Models", modelName, "data", paste0(modelName, ".data.R"))
  }

  model <- get_model_file(modelName)

  list(SEED = SEED, init_file = init_file, data = data, model = model)
}


#' Execute a system command with error handling
#'
#' Wrapper around \code{system()} that checks the exit code.
#' @param cmd Command string to execute.
#' @param description Description for error message.
#' @return The exit code (invisibly).
#' @noRd
.run_system <- function(cmd, description = "CmdStan") {
  exit_code <- system(cmd)
  if (exit_code != 0) {
    warning(description, " exited with code ", exit_code, call. = FALSE)
  }
  invisible(exit_code)
}


#' Compile a Stan model via CmdStan make
#'
#' @param model Model name (without .stan extension).
#' @param dir Path to CmdStan installation. If \code{NULL}, resolved via
#'   \code{getOption("StanFlowR.cmdstan_path")} or \code{Sys.getenv("CMDSTAN_PATH")}.
#' @param modeldir Full path to the model executable (without extension).
#'   Defaults to \code{Models/<model>/model/<model>}.
#' @param compiler_flags Optional compiler flags (e.g., \code{"--use-opencl"}).
#' @export
compile_cmdstan <- function(model, dir = NULL, modeldir, compiler_flags = NULL) {
  dir <- .resolve_cmdstan_path(dir)

  if (missing(modeldir)) {
    modeldir <- file.path(getwd(), "Models", model, "model", model)
  } else {
    modeldir <- file.path(getwd(), modeldir, model, "model", model)
  }

  compileModel(model = modeldir, stanDir = dir, compiler_flags = compiler_flags)
}


#' Compile a Stan model (worker)
#'
#' Runs \code{make} in the CmdStan directory to compile a Stan model.
#'
#' @param model Full path to the model (without extension).
#' @param stanDir Path to CmdStan installation.
#' @param compiler_flags Optional flags passed via STANCFLAGS.
#' @export
compileModel <- function(model, stanDir, compiler_flags = NULL) {
  modelName <- basename(model)
  dir.create(model, showWarnings = FALSE)

  stan_src <- paste0(model, ".stan")
  if (!file.exists(stan_src)) {
    # Try one level up
    stan_src_alt <- paste(model, "stan", sep = ".")
    if (file.exists(stan_src_alt)) stan_src <- stan_src_alt
  }

  file.copy(stan_src, file.path(model, paste0(modelName, ".stan")), overwrite = TRUE)
  model <- file.path(model, modelName)

  if (is.null(compiler_flags)) {
    cmd <- paste0("make --directory=", stanDir, " ", model)
  } else {
    cmd <- paste0("export STANCFLAGS=", compiler_flags, "; make --directory=", stanDir, " ", model)
  }

  .run_system(cmd, description = paste("Compilation of", modelName))
}


#' Create CmdStan model directory structure
#'
#' Creates the standard directory layout for a CmdStan model: data/ and model/ subdirectories.
#'
#' @param modelName Name of the model.
#' @param modelFile Path to the .stan file. Defaults to \code{Models/<modelName>.stan}.
#' @param modelDir Parent directory for models (default \code{"Models"}).
#' @param delete Logical; remove original .stan file after copying? Default \code{TRUE}.
#' @return Invisibly returns 0.
#' @export
cmdstan_mkdir <- function(modelName, modelFile, modelDir = "Models", delete = TRUE) {
  if (missing(modelFile)) {
    modelFile <- file.path(getwd(), modelDir, paste0(modelName, ".stan"))
  }
  if (!file.exists(modelFile)) {
    warning("The model file ", modelFile, " does not exist.")
    return(invisible(1))
  }

  target_dir <- file.path(getwd(), modelDir, modelName)
  dir.create(target_dir, showWarnings = FALSE)
  dir.create(file.path(target_dir, "data"), showWarnings = FALSE)
  dir.create(file.path(target_dir, "model"), showWarnings = FALSE)
  dir.create(file.path(target_dir, "model", "model_cache"), showWarnings = FALSE)

  file.copy(modelFile, file.path(target_dir, "model", paste0(modelName, ".stan")),
            overwrite = TRUE)
  if (delete) {
    file.remove(modelFile)
  }
  invisible(0)
}


#' Write Stan data file
#'
#' Writes a Stan data list to an R dump file.
#'
#' @param data A named list of Stan data.
#' @param modelName Name of the model (used for default file path).
#' @param tag Optional identifier for building different datasets.
#' @param modelFile Explicit output path (overrides defaults).
#' @export
write_data <- function(data, modelName, tag, modelFile) {
  if (!missing(modelFile)) {
    destiny <- modelFile
  } else if (missing(tag)) {
    destiny <- file.path("Models", modelName, "data", paste0(modelName, ".data.R"))
  } else {
    destiny <- file.path("Models", modelName, "data", paste0(modelName, "_", tag, ".data.R"))
  }
  with(data, new_stanrdump(ls(data), destiny))
}


#' Get path to initial values file
#'
#' @param modelName Name of model.
#' @param modelDir Model directory (default \code{"Models"}).
#' @param chain Chain ID.
#' @param tag Run tag.
#' @param format Init file format: \code{"json"} (default) or \code{"rdump"}.
#' @return File path to the init file.
#' @noRd
get_init_file <- function(modelName, modelDir = "Models", chain, tag,
                          format = c("json", "rdump")) {
  format <- match.arg(format)
  ext <- if (format == "json") "init.json" else "init.R"
  file.path(getwd(), modelDir, modelName, "model", modelName,
            paste0("temp", tag), chain, ext)
}


#' Get path to compiled model executable
#'
#' @param modelName Name of model.
#' @param modelDir Model directory (default \code{"Models"}).
#' @return Directory path containing the model executable.
#' @noRd
get_model_file <- function(modelName, modelDir = "Models") {
  file.path(getwd(), modelDir, modelName, "model", modelName)
}

