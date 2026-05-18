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
.prepare_run <- function(chain, modelName, init, init_dir, data, tag, seed) {
  SEED <- seed + chain
  set.seed(SEED)
  init_file <- NULL

  if (!is.null(init) && !init_dir) {
    write_inits(init, chain, modelName, label = tag)
    init_file <- get_init_file(modelName, "Models", chain, tag = tag)
  }

  if (is.null(init) && init_dir) {
    init_file <- get_init_file(modelName, "Models", chain, tag = tag)
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
#' @return File path to the init file.
#' @noRd
get_init_file <- function(modelName, modelDir = "Models", chain, tag) {
  file.path(getwd(), modelDir, modelName, "model", modelName,
            paste0("temp", tag), chain, "init.R")
}


#' Get path to compiled model executable
#'
#' @param modelName Name of model.
#' @param modelDir Model directory (default \code{"Models"}).
#' @return Directory path containing the model executable.
#' @noRd
get_model_file <- function(modelName, modelDir = "Models") {
  file.path(getwd(), modelDir, modelName, "model")
}


#' Run a CmdStan model
#'
#' User-facing function to execute a compiled CmdStan model via system calls.
#'
#' @param chain Chain ID.
#' @param modelName Name of the model.
#' @param modelDir Model directory (default \code{"Models"}).
#' @param data Path to the data file.
#' @param init A function that returns a list of initial values, or \code{NULL}.
#' @param init_dir Logical; if \code{TRUE}, use pre-existing init files.
#' @param iter Number of post-warmup iterations.
#' @param warmup Number of warmup iterations.
#' @param thin Thinning interval.
#' @param adapt_delta Target acceptance rate.
#' @param stepsize Initial step size.
#' @param tag Optional run tag for output file naming.
#' @param method Sampling method: \code{"sample"} or \code{"variational"}.
#' @param algorithm Algorithm specification (default \code{"hmc engine=nuts"}).
#' @param seed Random seed.
#' @param ... Currently ignored.
#' @return Invisibly returns 0.
#' @export
run_cmdstan <- function(chain, modelName,
                        modelDir = "Models",
                        data = NULL,
                        init = NULL, init_dir = FALSE,
                        iter = 1000, warmup = 1000, thin = 1,
                        adapt_delta = 0.9, stepsize = 1,
                        tag = NULL,
                        method = "sample",
                        algorithm = "hmc engine=nuts",
                        seed = sample.int(.Machine$integer.max, 1), ...) {

  prep <- .prepare_run(chain, modelName, init, init_dir, data, tag, seed)

  runModel(model = prep$model, data = prep$data,
           iter = iter, warmup = warmup, thin = thin,
           init = if (!is.null(prep$init_file)) prep$init_file else "",
           seed = prep$SEED, tag = tag, method = method,
           chain = chain, refresh = 100,
           adapt_delta = adapt_delta, stepsize = stepsize,
           algorithm = algorithm)
  invisible(0)
}


#' Grep step size from CmdStan CSV
#'
#' Extracts the step size from a CmdStan CSV output file.
#'
#' @param file Path to a CmdStan output CSV file.
#' @return Numeric step size value.
#' @export
grep_step_size <- function(file) {
  lines <- readLines(file, n = 50)
  step_line <- grep("^# Step size = ", lines, value = TRUE)
  if (length(step_line) == 0) {
    stop("Could not find step size in file: ", file, call. = FALSE)
  }
  as.numeric(gsub("# Step size = ", "", step_line[1]))
}


#' Grep inverse metric from CmdStan CSV
#'
#' Extracts the inverse metric (diagonal) from a CmdStan CSV output file.
#'
#' @param file Path to a CmdStan output CSV file.
#' @return Numeric vector of inverse metric values.
#' @export
grep_metric_file <- function(file) {
  lines <- readLines(file, n = 60)
  # The metric line follows "# Diagonal elements of inverse mass matrix:"
  metric_header <- grep("inverse mass matrix", lines)
  if (length(metric_header) == 0) {
    stop("Could not find inverse metric in file: ", file, call. = FALSE)
  }
  inv_line <- lines[metric_header[1] + 1]
  inv_line <- gsub("#\\s*", "", inv_line)
  as.numeric(unlist(strsplit(inv_line, ",")))
}


#' Create initial value directory
#' @noRd
create_init_dir <- function(modelName, modelDir = "Models", label) {
  modelDir <- file.path(getwd(), modelDir, modelName, "model")
  tempDir <- file.path(modelDir, modelName, paste0("temp", label))
  if (!dir.exists(tempDir)) {
    dir.create(tempDir, recursive = TRUE)
  }
}


#' Generate and write initial values
#'
#' Generates initial values by calling the provided function and writes them
#' as R dump files for each chain.
#'
#' @param init A function that returns a named list of initial values.
#' @param chain Chain ID (integer or integer vector).
#' @param modelName Name of the model.
#' @param modelDir Model directory (default \code{"Models"}).
#' @param seed Random seed.
#' @param label Run label/tag.
#' @param force Logical; force overwriting existing inits.
#' @return Invisibly returns 0.
#' @export
write_inits <- function(init, chain, modelName, modelDir = "Models",
                        seed = sample.int(.Machine$integer.max, 1),
                        label = NULL, force = TRUE) {
  tempDir <- file.path(getwd(), modelDir, modelName, "model", modelName, paste0("temp", label))

  if (!dir.exists(tempDir) || force) {
    create_init_dir(modelName, modelDir, label)
    modelDir_full <- file.path(getwd(), modelDir, modelName, "model")
    tempDir <- file.path(modelDir_full, modelName, paste0("temp", label))

    if (!is.numeric(chain)) stop("Chain must be numeric", call. = FALSE)

    for (cha in chain) {
      chain_dir <- file.path(tempDir, cha)
      dir.create(chain_dir, showWarnings = FALSE, recursive = TRUE)
      SEED <- seed + cha
      set.seed(SEED)
      inits <- init()
      with(inits, new_stanrdump(ls(inits), file = file.path(chain_dir, "init.R")))
    }
  }
  invisible(0)
}


#' Get step size from warmup output
#'
#' Convenience wrapper to extract step size from the warmup CSV of a model.
#'
#' @param model Model name.
#' @param modelfile Path to CSV file (default: warmup chain 1 output).
#' @return Numeric step size.
#' @export
get_step_size <- function(model, modelfile = NULL) {
  if (is.null(modelfile)) {
    modelfile <- file.path("Models", model, "model", model, paste0(model, "_warmup_1.csv"))
  }
  grep_step_size(modelfile)
}


#' Write metric file for CmdStan
#'
#' Extracts the inverse metric from a warmup CSV and writes it as a JSON
#' file for reuse in subsequent runs.
#'
#' @param model Model name.
#' @param modelfile Path to warmup CSV (default: warmup chain 1 output).
#' @return Invisibly returns 0.
#' @export
write_metric_file <- function(model, modelfile = NULL) {
  metric_file <- file.path("Models", model, "data", paste0("metric_", model, ".json"))

  if (is.null(modelfile)) {
    modelfile <- file.path("Models", model, "model", model, paste0(model, "_warmup_1.csv"))
  }
  metric_data <- list(inv_metric = grep_metric_file(modelfile))

  write_stan_json(data = metric_data, file = metric_file)
  invisible(0)
}


#' Run CmdStan with fixed parameters
#'
#' User-facing function to run a model with the fixed_param algorithm (for simulation).
#'
#' @inheritParams run_cmdstan
#' @return Invisibly returns 0.
#' @export
run_cmdstan_fixed <- function(modelName, chain = 1,
                              modelDir = "Models",
                              data = NULL,
                              init = NULL, init_dir = FALSE,
                              iter = 1000, warmup = 1000, thin = 1,
                              adapt_delta = 0.9, stepsize = 1,
                              tag = NULL,
                              seed = sample.int(.Machine$integer.max, 1), ...) {

  prep <- .prepare_run(chain, modelName, init, init_dir, data, tag, seed)

  modelName_base <- basename(prep$model)
  model_exec <- file.path(prep$model, modelName_base)
  if (!is.null(tag)) output <- paste0(model_exec, "_", tag, "_") else output <- model_exec

  cmd <- paste0(model_exec, " sample algorithm=fixed_param",
                " num_samples=", iter,
                " data file=", prep$data,
                " random seed=", prep$SEED,
                " output file=", paste0(output, chain, ".csv"),
                " refresh=100")
  .run_system(cmd, description = paste("Fixed-param run of", modelName))
  invisible(0)
}


#' Run CmdStan diagnose
#'
#' Runs the CmdStan \code{diagnose} method on a compiled model.
#'
#' @param model Path to model directory.
#' @param data Path to data file.
#' @param init Path to init file.
#' @param seed Random seed.
#' @param chain Chain ID.
#' @param refresh Refresh rate for output.
#' @export
runDiagnose <- function(model, data, init, seed, chain = 1, refresh = 100) {
  modelName <- basename(model)
  model <- file.path(model, modelName)
  cmd <- paste0(model, " diagnose",
                " data file=", data,
                " init=", init,
                " random seed=", seed,
                " output file=", paste0(model, chain, ".csv"),
                " refresh=", refresh)
  .run_system(cmd, description = "CmdStan diagnose")
}


#' Read column names from CmdStan CSV
#'
#' Extracts parameter names from a CmdStan output CSV file by parsing
#' the header line (skipping comment lines).
#'
#' @param file Path to a CmdStan CSV output file.
#' @return Character vector of column names.
#' @export
colnames_cmdstan <- function(file) {
  lines <- readLines(file)
  # Find first non-comment line (the header)
  header_idx <- which(!grepl("^#", lines))[1]
  if (is.na(header_idx)) {
    stop("Could not find header line in: ", file, call. = FALSE)
  }
  cols <- unlist(strsplit(lines[header_idx], ","))
  cols
}


#' Select variables from CmdStan CSV
#'
#' Reads selected columns from a CmdStan output CSV file using
#' \code{data.table::fread} for efficiency.
#'
#' @param file Path to a CmdStan CSV file.
#' @param vars Character vector of column names to select.
#' @param newfile Optional path to write selected columns as CSV.
#' @return A data.frame of selected columns, or \code{NULL} if \code{newfile} is specified.
#' @importFrom data.table fread
#' @export
select_vars_cmdstan <- function(file, vars, newfile = NULL) {
  # Create temp file without comment lines
  all_lines <- readLines(file)
  data_lines <- all_lines[!grepl("^#", all_lines)]
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(data_lines, tmp)

  post <- data.table::fread(tmp, select = vars)

  if (is.null(newfile)) {
    return(as.data.frame(post))
  } else {
    data.table::fwrite(post, paste0(newfile, ".csv"))
    invisible(NULL)
  }
}


#' Clean up compiled model files
#'
#' Removes the compiled model binary and .hpp file.
#'
#' @param modelName Name of the model.
#' @param modelDir Path to directory containing the model binary.
#' @export
cleanup_model <- function(modelName, modelDir) {
  if (missing(modelDir)) {
    modelDir <- file.path("Models", modelName, "model", modelName)
  }
  bin_file <- file.path(modelDir, modelName)
  hpp_file <- paste0(bin_file, ".hpp")

  if (file.exists(bin_file)) file.remove(bin_file)
  if (file.exists(hpp_file)) file.remove(hpp_file)
  invisible(0)
}


#' Remove CSV output files
#'
#' Removes all CSV output files from a model directory.
#'
#' @param modelName Name of the model.
#' @param modelDir Path to directory containing the CSV files.
#' @export
clear_csv_model <- function(modelName, modelDir) {
  if (missing(modelDir)) {
    modelDir <- file.path("Models", modelName, "model", modelName)
  }
  csv_files <- list.files(modelDir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) > 0) {
    file.remove(csv_files)
  }
  invisible(0)
}
