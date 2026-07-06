## StanFlowR: Stan model execution functions (local, GPU, OpenMP, MPI)


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
#' @param init_format Init file format: \code{"json"} (default, recommended for
#'   CmdStan v2.26+) or \code{"rdump"} for legacy behavior.
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
                        init_format = c("json", "rdump"),
                        seed = sample.int(.Machine$integer.max, 1), ...) {
  init_format <- match.arg(init_format)
  
  prep <- .prepare_run(chain, modelName, init, init_dir, data, tag, seed,
                       init_format = init_format)
  
  runModel(model = prep$model, data = prep$data,
           iter = iter, warmup = warmup, thin = thin,
           init = if (!is.null(prep$init_file)) prep$init_file else "",
           seed = prep$SEED, tag = tag, method = method,
           chain = chain, refresh = 100,
           adapt_delta = adapt_delta, stepsize = stepsize,
           algorithm = algorithm)
  invisible(0)
}

#' Run a CmdStan model (worker)
#'
#' Core worker function that executes a compiled CmdStan binary via \code{system()}.
#' Supports sampling and variational inference methods.
#'
#' @param model Path to model directory.
#' @param data Path to data file.
#' @param iter Number of sampling iterations.
#' @param warmup Number of warmup iterations.
#' @param thin Thinning interval.
#' @param init Path to init file (empty string for no inits).
#' @param seed Random seed.
#' @param chain Chain ID.
#' @param stepsize Initial step size.
#' @param adapt_delta Target acceptance rate.
#' @param method One of \code{"sample"} or \code{"variational"}.
#' @param max_depth Maximum tree depth.
#' @param refresh Refresh rate for output.
#' @param tag Optional run tag.
#' @param algorithm Algorithm specification.
#' @param metric_file Optional path to metric file for adapted runs.
#' @param adapt_args Optional extra adapt arguments string (e.g., \code{"init_buffer=0 window=0 term_buffer=50"}).
#' @export
runModel <- function(model, data, iter, warmup, thin, init, seed, chain = 1,
                     stepsize = 1, adapt_delta = 0.8, method = "sample",
                     max_depth = 10, refresh = 100, tag = NULL,
                     algorithm = "hmc engine=nuts",
                     metric_file = NULL, adapt_args = NULL) {
  if (!is.null(tag)) output <- paste0(model, "_", tag, "_") else output <- model
  
  modelName <- basename(model)                                                                                                                                                                  
  model <- file.path(model, modelName)  
  
  if (method == "sample") {
    adapt_str <- ""
    if (!is.null(adapt_args)) {
      adapt_str <- paste0(" adapt ", adapt_args)
    }
    metric_str <- ""
    if (!is.null(metric_file)) {
      metric_str <- paste0(" metric_file=", metric_file)
    }
    cmd <- paste0(model, " sample algorithm=", algorithm,
                  " max_depth=", max_depth,
                  " stepsize=", stepsize,
                  metric_str,
                  adapt_str,
                  " num_samples=", iter,
                  " num_warmup=", warmup,
                  " thin=", thin,
                  " adapt delta=", adapt_delta,
                  " data file=", data,
                  " init=", init,
                  " random seed=", seed,
                  " output file=", paste0(output, chain, ".csv"),
                  " refresh=", refresh)
  } else if (method == "variational") {
    cmd <- paste0(model, " variational algorithm=meanfield",
                  " output_samples=", iter,
                  " data file=", data,
                  " init=", init,
                  " random seed=", seed,
                  " output file=", paste0(output, chain, ".csv"),
                  " refresh=", refresh)
  } else {
    stop("Method not recognised: ", method, call. = FALSE)
  }
  
  .run_system(cmd, description = paste("CmdStan", method))
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
#' to files for each chain. Defaults to JSON format for CmdStan compatibility.
#'
#' @param init A function that returns a named list of initial values.
#' @param chain Chain ID (integer or integer vector).
#' @param modelName Name of the model.
#' @param modelDir Model directory (default \code{"Models"}).
#' @param seed Random seed.
#' @param label Run label/tag.
#' @param force Logical; force overwriting existing inits.
#' @param format Output format: \code{"json"} (default) or \code{"rdump"}.
#' @return Invisibly returns 0.
#' @export
write_inits <- function(init, chain, modelName, modelDir = "Models",
                        seed = sample.int(.Machine$integer.max, 1),
                        label = NULL, force = TRUE,
                        format = c("json", "rdump")) {
  format <- match.arg(format)
  tempDir <- file.path(getwd(), modelDir, modelName, "model", modelName, paste0("temp", label))
  
  if (!dir.exists(tempDir) || force) {
    create_init_dir(modelName, modelDir, label)
    modelDir_full <- file.path(getwd(), modelDir, modelName, "model")
    tempDir <- file.path(modelDir_full, modelName, paste0("temp", label))
    
    if (!is.numeric(chain)) stop("Chain must be numeric", call. = FALSE)
    
    ext <- if (format == "json") "init.json" else "init.R"
    for (cha in chain) {
      chain_dir <- file.path(tempDir, cha)
      dir.create(chain_dir, showWarnings = FALSE, recursive = TRUE)
      SEED <- seed + cha
      set.seed(SEED)
      inits <- init()
      with(inits, new_stanrdump(ls(inits),
                                file = file.path(chain_dir, ext),
                                format = format))
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
                              init_format = c("json", "rdump"),
                              seed = sample.int(.Machine$integer.max, 1), ...) {
  init_format <- match.arg(init_format)
  
  prep <- .prepare_run(chain, modelName, init, init_dir, data, tag, seed,
                       init_format = init_format)
  
  model_exec <- prep$model
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



#' Run CmdStan with GPU acceleration
#'
#' User-facing function to run a compiled CmdStan model with GPU (CUDA) support.
#' Requires CUDA modules to be available on the system.
#'
#' @inheritParams run_cmdstan
#' @param cuda_modules Character string of module load commands for CUDA
#'   (e.g., \code{"module load CUDA cuDNN"}). Set to \code{NULL} to skip module loading.
#' @return Invisibly returns 0.
#' @export
run_gpu_cmdstan <- function(chain, modelName,
                            modelDir = "Models",
                            data = NULL,
                            init = NULL, init_dir = FALSE,
                            iter = 1000, warmup = 1000, thin = 1,
                            adapt_delta = 0.9, stepsize = 1,
                            tag = NULL,
                            method = "sample",
                            algorithm = "hmc engine=nuts",
                            cuda_modules = NULL,
                            seed = sample.int(.Machine$integer.max, 1), ...) {

  prep <- .prepare_run(chain, modelName, init, init_dir, data, tag, seed)

  model_exec <- prep$model
  if (!is.null(tag)) output <- paste0(model_exec, "_", tag, "_") else output <- model_exec

  if (method == "sample") {
    core_cmd <- paste0(model_exec, " sample algorithm=", algorithm,
                       " max_depth=10",
                       " stepsize=", stepsize,
                       " num_samples=", iter,
                       " num_warmup=", warmup,
                       " thin=", thin,
                       " adapt delta=", adapt_delta,
                       " data file=", prep$data,
                       " init=", if (!is.null(prep$init_file)) prep$init_file else "",
                       " random seed=", prep$SEED,
                       " output file=", paste0(output, chain, ".csv"),
                       " refresh=100")
  } else if (method == "variational") {
    core_cmd <- paste0(model_exec, " variational algorithm=meanfield",
                       " output_samples=", iter,
                       " data file=", prep$data,
                       " init=", if (!is.null(prep$init_file)) prep$init_file else "",
                       " random seed=", prep$SEED,
                       " output file=", paste0(output, chain, ".csv"),
                       " refresh=100")
  } else {
    stop("Method not recognised: ", method, call. = FALSE)
  }

  if (!is.null(cuda_modules) && nzchar(cuda_modules)) {
    cmd <- paste0(cuda_modules, "; ", core_cmd)
  } else {
    cmd <- core_cmd
  }

  .run_system(cmd, description = "GPU CmdStan run")
  invisible(0)
}


#' Run CmdStan with OpenMP (multi-threaded)
#'
#' User-facing function to run a model with OpenMP threading.
#'
#' @param n_threads Number of threads (default 1).
#' @inheritParams run_cmdstan
#' @param max_depth Maximum tree depth.
#' @return Invisibly returns 0.
#' @export
run_mp_cmdstan <- function(n_threads = 1,
                           chain, modelName,
                           modelDir = "Models",
                           data = NULL,
                           init = NULL, init_dir = FALSE,
                           iter = 1000, warmup = 1000, thin = 1,
                           adapt_delta = 0.9, max_depth = 10,
                           tag = NULL, stepsize = 1,
                           seed = sample.int(.Machine$integer.max, 1), ...) {

  prep <- .prepare_run(chain, modelName, init, init_dir, data, tag, seed)

  mprunModel(n_threads = n_threads,
             model = prep$model, data = prep$data,
             iter = iter, warmup = warmup, thin = thin,
             init = if (!is.null(prep$init_file)) prep$init_file else "",
             seed = prep$SEED, chain = chain,
             stepsize = stepsize, adapt_delta = adapt_delta,
             max_depth = max_depth, tag = tag)
  invisible(0)
}


#' Run CmdStan with MPI (Torsten)
#'
#' User-facing function to run a model via MPI for parallel execution.
#'
#' @param ncores Number of MPI processes (default 1).
#' @inheritParams run_cmdstan
#' @param max_depth Maximum tree depth.
#' @param metric_file Optional path to metric file.
#' @param debugging Logical; enable debug output.
#' @return Invisibly returns 0.
#' @export
mpi_run_cmdstan <- function(ncores = 1,
                            chain, modelName,
                            modelDir = "Models",
                            data = NULL,
                            init = NULL, init_dir = FALSE,
                            iter = 1000, warmup = 1000, thin = 1,
                            adapt_delta = 0.9, max_depth = 10,
                            tag = NULL, stepsize = 1,
                            metric_file = NULL,
                            debugging = FALSE,
                            method = "sample",
                            seed = sample.int(.Machine$integer.max, 1), ...) {

  prep <- .prepare_run(chain, modelName, init, init_dir, data, tag, seed)

  mpi_runModel(ncores = ncores,
               model = prep$model, data = prep$data,
               iter = iter, warmup = warmup, thin = thin,
               init = prep$init_file,
               seed = prep$SEED, chain = chain,
               stepsize = stepsize, adapt_delta = adapt_delta,
               max_depth = max_depth, metric_file = metric_file,
               tag = tag, method = method, debugging = debugging)
  invisible(0)
}


#' Run model with OpenMP (worker)
#'
#' Worker function for \code{run_mp_cmdstan}. Sets \code{STAN_NUM_THREADS} and
#' executes the compiled Stan binary.
#'
#' @param n_threads Number of threads.
#' @param model Path to model directory.
#' @param data Path to data file.
#' @param iter,warmup,thin Sampling parameters.
#' @param init Path to init file.
#' @param seed Random seed.
#' @param chain Chain ID.
#' @param stepsize,adapt_delta,max_depth Adaptation parameters.
#' @param tag Optional run tag.
#' @param algorithm Algorithm specification.
#' @export
mprunModel <- function(n_threads, model, data, iter, warmup, thin, init,
                       seed, chain, stepsize, adapt_delta, max_depth,
                       tag = NULL, algorithm = "hmc engine=nuts") {
  if (!is.null(tag)) output <- paste0(model, "_", tag, "_") else output <- model

  cmd <- paste0("export STAN_NUM_THREADS=", n_threads, "; ",
                model, " sample algorithm=", algorithm,
                " max_depth=", max_depth,
                " stepsize=", stepsize,
                " num_samples=", iter,
                " num_warmup=", warmup,
                " thin=", thin,
                " adapt delta=", adapt_delta,
                " data file=", data,
                " init=", init,
                " random seed=", seed,
                " output file=", paste0(output, chain, ".csv"))
  .run_system(cmd, description = "OpenMP CmdStan run")
}


#' Run model with MPI (worker)
#'
#' Worker function for \code{mpi_run_cmdstan}. Executes via \code{mpirun}.
#'
#' @param ncores Number of MPI processes.
#' @param model Path to model directory.
#' @param data Path to data file.
#' @param iter,warmup,thin Sampling parameters.
#' @param init Path to init file.
#' @param seed Random seed.
#' @param chain Chain ID.
#' @param stepsize,adapt_delta,max_depth Adaptation parameters.
#' @param metric_file Optional path to metric file.
#' @param method One of \code{"sample"} or \code{"variational"}.
#' @param tag Optional run tag.
#' @param debugging Logical; enable debug output.
#' @export
mpi_runModel <- function(ncores, model, data, iter, warmup, thin, init,
                         seed, chain, stepsize, adapt_delta, max_depth,
                         metric_file = NULL, method = "sample",
                         tag = NULL, debugging = FALSE) {
  if (!is.null(tag)) output <- paste0(model, "_", tag, "_") else output <- model

  if (method == "sample") {
    if (!is.null(metric_file)) {
      cmd <- paste0("mpirun ", model,
                    " sample adapt init_buffer=0 window=0 term_buffer=50 num_warmup=50",
                    " algorithm=hmc engine=nuts",
                    " max_depth=", max_depth,
                    " stepsize=", stepsize,
                    " metric_file=", metric_file,
                    " num_samples=", iter,
                    " thin=", thin,
                    " adapt delta=", adapt_delta,
                    " data file=", data,
                    " init=", init,
                    " random seed=", seed,
                    " output file=", paste0(output, chain, ".csv"))
    } else {
      cmd <- paste0("mpirun ", model,
                    " sample algorithm=hmc engine=nuts",
                    " max_depth=", max_depth,
                    " stepsize=", stepsize,
                    " num_samples=", iter,
                    " num_warmup=", warmup,
                    " thin=", thin,
                    " adapt delta=", adapt_delta,
                    " data file=", data,
                    " init=", init,
                    " random seed=", seed,
                    " output file=", paste0(output, chain, ".csv"))
    }
  } else if (method == "variational") {
    cmd <- paste0("mpirun ", model,
                  " variational algorithm=meanfield",
                  " output_samples=", iter,
                  " data file=", data,
                  " init=", init,
                  " random seed=", seed,
                  " output file=", paste0(output, chain, ".csv"))
  } else {
    stop("Method not recognised: ", method, call. = FALSE)
  }

  .run_system(cmd, description = "MPI CmdStan run")
}
