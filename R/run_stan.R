## StanFlowR: Stan model execution functions (local, GPU, OpenMP, MPI)


#' Build the CmdStan command string
#'
#' Pure function: assembles all arguments into a single ready-to-execute
#' command string. No filesystem side effects.
#'
#' @param exe_path Full path to the compiled Stan executable.
#' @param method "sample" or "variational".
#' @param data_file Path to data file.
#' @param init_file Path to init file, or \code{NULL}/\code{""} for no init.
#' @param output_prefix Output CSV path prefix (chain number and .csv appended).
#' @param chain Chain ID.
#' @param iter,warmup,thin,seed,adapt_delta,max_depth,stepsize,refresh Sampling parameters.
#' @param algorithm Algorithm string (default \code{"hmc engine=nuts"}).
#' @param metric_file Optional path to metric file.
#' @param adapt_args Optional adapt sub-arguments string.
#' @param mpi Logical; prepend \code{mpirun -np mpi_nprocs}.
#' @param mpi_nprocs Number of MPI processes.
#' @param n_threads Optional; sets \code{STAN_NUM_THREADS} env var.
#' @param cuda_modules Optional CUDA module-load command string.
#' @return Character string: the full command.
#' @noRd
.build_cmdstan_cmd <- function(exe_path, method, data_file, init_file,
                               output_prefix, chain,
                               iter, warmup, thin, seed,
                               adapt_delta, max_depth, stepsize, refresh,
                               algorithm,
                               metric_file, adapt_args,
                               mpi, mpi_nprocs,
                               n_threads, cuda_modules) {
  # Shell prefix (env vars, module loads) — mutually exclusive
  prefix <- ""
  if (!is.null(n_threads)) {
    prefix <- paste0("export STAN_NUM_THREADS=", n_threads, "; ")
  } else if (!is.null(cuda_modules) && nzchar(cuda_modules)) {
    prefix <- paste0(cuda_modules, "; ")
  }

  # Executable invocation with optional MPI launcher
  exe_cmd <- if (mpi) {
    paste0("mpirun -np ", mpi_nprocs, " ", exe_path)
  } else {
    exe_path
  }

  init_str <- if (!is.null(init_file) && nzchar(init_file)) {
    paste0(" init=", init_file)
  } else ""

  output_file <- paste0(output_prefix, chain, ".csv")

  if (method == "sample") {
    adapt_str <- if (!is.null(adapt_args) && nzchar(adapt_args)) {
      paste0(" adapt ", adapt_args)
    } else ""
    metric_str <- if (!is.null(metric_file)) {
      paste0(" metric_file=", metric_file)
    } else ""
    cmd <- paste0(exe_cmd,
                  " sample algorithm=", algorithm,
                  " max_depth=", max_depth,
                  " stepsize=", stepsize,
                  metric_str,
                  adapt_str,
                  " num_samples=", iter,
                  " num_warmup=", warmup,
                  " thin=", thin,
                  " adapt delta=", adapt_delta,
                  " data file=", data_file,
                  init_str,
                  " random seed=", seed,
                  " output file=", output_file,
                  " refresh=", refresh)
  } else if (method == "variational") {
    cmd <- paste0(exe_cmd,
                  " variational algorithm=meanfield",
                  " output_samples=", iter,
                  " data file=", data_file,
                  init_str,
                  " random seed=", seed,
                  " output file=", output_file,
                  " refresh=", refresh)
  } else {
    stop("Method not recognised: ", method, call. = FALSE)
  }

  paste0(prefix, cmd)
}


#' Run a CmdStan model
#'
#' User-facing function to execute a compiled CmdStan model via system calls.
#' Supports sampling, variational inference, OpenMP threading, MPI, and GPU.
#'
#' @param chain Chain ID.
#' @param modelName Name of the model.
#' @param modelDir Model directory (default \code{"Models"}).
#' @param data Path to the data file.
#' @param init A function that returns a named list of initial values, or \code{NULL}.
#' @param init_dir Logical; if \code{TRUE}, use pre-existing init files.
#' @param iter Number of post-warmup iterations.
#' @param warmup Number of warmup iterations.
#' @param thin Thinning interval.
#' @param adapt_delta Target acceptance rate.
#' @param stepsize Initial step size.
#' @param max_depth Maximum tree depth (default 10).
#' @param tag Optional run tag for output file naming.
#' @param method Sampling method: \code{"sample"} (default) or \code{"variational"}.
#' @param algorithm Algorithm specification (default \code{"hmc engine=nuts"}).
#' @param metric_file Optional path to metric file for adapted runs.
#' @param adapt_args Optional adapt sub-arguments string passed after
#'   \code{adapt} in the CmdStan command.
#' @param mpi Logical; run via \code{mpirun} (default \code{FALSE}).
#' @param mpi_nprocs Number of MPI processes (ignored if \code{mpi = FALSE}).
#' @param n_threads Optional; sets \code{STAN_NUM_THREADS} for OpenMP threading.
#' @param cuda_modules Optional CUDA module-load command string prepended to
#'   the CmdStan invocation.
#' @param init_format Init file format: \code{"json"} (default, recommended for
#'   CmdStan v2.26+) or \code{"rdump"} for legacy behavior.
#' @param refresh Progress print interval (default 100; 0 = silent).
#' @param seed Random seed.
#' @param ... Currently ignored.
#' @return Invisibly returns 0.
#' @export
run_cmdstan <- function(chain, modelName,
                        modelDir = "Models",
                        data = NULL,
                        init = NULL, init_dir = FALSE,
                        iter = 1000, warmup = 1000, thin = 1,
                        adapt_delta = 0.9, stepsize = 1, max_depth = 10,
                        tag = NULL,
                        method = c("sample", "variational"),
                        algorithm = "hmc engine=nuts",
                        metric_file = NULL, adapt_args = NULL,
                        mpi = FALSE, mpi_nprocs = 1,
                        n_threads = NULL, cuda_modules = NULL,
                        init_format = c("json", "rdump"),
                        refresh = 100,
                        seed = sample.int(.Machine$integer.max, 1), ...) {
  method <- match.arg(method)
  init_format <- match.arg(init_format)

  prep <- .prepare_run(chain, modelName, init, init_dir, data, tag, seed,
                       init_format = init_format)

  output_prefix <- if (!is.null(tag)) {
    paste0(prep$model, "_", tag, "_")
  } else {
    prep$model
  }

  cmd <- .build_cmdstan_cmd(
    exe_path      = prep$model,
    method        = method,
    data_file     = prep$data,
    init_file     = prep$init_file,
    output_prefix = output_prefix,
    chain         = chain,
    iter          = iter, warmup = warmup, thin = thin,
    seed          = prep$SEED,
    adapt_delta   = adapt_delta, max_depth = max_depth, stepsize = stepsize,
    refresh       = refresh, algorithm = algorithm,
    metric_file   = metric_file, adapt_args = adapt_args,
    mpi           = mpi, mpi_nprocs = mpi_nprocs,
    n_threads     = n_threads, cuda_modules = cuda_modules
  )

  .run_system(cmd, description = paste("CmdStan", method))
  invisible(0)
}


#' Run a CmdStan model (worker)
#'
#' Executes a compiled CmdStan binary via \code{system()}.
#' Accepts the full path to the executable directly.
#'
#' @param model Full path to the compiled Stan executable.
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
#' @param adapt_args Optional extra adapt arguments string.
#' @export
runModel <- function(model, data, iter, warmup, thin, init, seed, chain = 1,
                     stepsize = 1, adapt_delta = 0.8, method = "sample",
                     max_depth = 10, refresh = 100, tag = NULL,
                     algorithm = "hmc engine=nuts",
                     metric_file = NULL, adapt_args = NULL) {
  output_prefix <- if (!is.null(tag)) paste0(model, "_", tag, "_") else model
  cmd <- .build_cmdstan_cmd(
    exe_path      = model,
    method        = method,
    data_file     = data,
    init_file     = init,
    output_prefix = output_prefix,
    chain         = chain,
    iter          = iter, warmup = warmup, thin = thin, seed = seed,
    adapt_delta   = adapt_delta, max_depth = max_depth, stepsize = stepsize,
    refresh       = refresh, algorithm = algorithm,
    metric_file   = metric_file, adapt_args = adapt_args,
    mpi           = FALSE, mpi_nprocs = 1,
    n_threads     = NULL, cuda_modules = NULL
  )
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
  tempDir <- file.path(getwd(), modelDir, modelName, "model", modelName,
                       paste0("temp", label))

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
    modelfile <- file.path("Models", model, "model", model,
                           paste0(model, "_warmup_1.csv"))
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
  metric_file <- file.path("Models", model, "data",
                           paste0("metric_", model, ".json"))

  if (is.null(modelfile)) {
    modelfile <- file.path("Models", model, "model", model,
                           paste0(model, "_warmup_1.csv"))
  }
  metric_data <- list(inv_metric = grep_metric_file(modelfile))

  write_stan_json(data = metric_data, file = metric_file)
  invisible(0)
}


#' Run CmdStan with fixed parameters (deprecated)
#'
#' @description `r lifecycle::badge("deprecated")`
#' Use \code{run_cmdstan(..., algorithm = "fixed_param")} instead.
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
  .Deprecated('run_cmdstan(..., algorithm = "fixed_param")')
  init_format <- match.arg(init_format)
  run_cmdstan(chain = chain, modelName = modelName, modelDir = modelDir,
              data = data, init = init, init_dir = init_dir,
              iter = iter, warmup = warmup, thin = thin,
              adapt_delta = adapt_delta, stepsize = stepsize,
              tag = tag, algorithm = "fixed_param",
              init_format = init_format, seed = seed)
}


#' Run CmdStan diagnose
#'
#' Runs the CmdStan \code{diagnose} method on a compiled model.
#'
#' @param model Full path to compiled Stan executable.
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
  header_idx <- which(!grepl("^#", lines))[1]
  if (is.na(header_idx)) {
    stop("Could not find header line in: ", file, call. = FALSE)
  }
  unlist(strsplit(lines[header_idx], ","))
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


#' Run CmdStan with GPU acceleration (deprecated)
#'
#' @description `r lifecycle::badge("deprecated")`
#' Use \code{run_cmdstan(..., cuda_modules = "...")} instead.
#'
#' @inheritParams run_cmdstan
#' @param cuda_modules Character string of module load commands for CUDA.
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
  .Deprecated("run_cmdstan(..., cuda_modules = cuda_modules)")
  run_cmdstan(chain = chain, modelName = modelName, modelDir = modelDir,
              data = data, init = init, init_dir = init_dir,
              iter = iter, warmup = warmup, thin = thin,
              adapt_delta = adapt_delta, stepsize = stepsize,
              tag = tag, method = method, algorithm = algorithm,
              cuda_modules = cuda_modules, seed = seed)
}


#' Run CmdStan with OpenMP threading (deprecated)
#'
#' @description `r lifecycle::badge("deprecated")`
#' Use \code{run_cmdstan(..., n_threads = n)} instead.
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
  .Deprecated("run_cmdstan(..., n_threads = n_threads)")
  run_cmdstan(chain = chain, modelName = modelName, modelDir = modelDir,
              data = data, init = init, init_dir = init_dir,
              iter = iter, warmup = warmup, thin = thin,
              adapt_delta = adapt_delta, stepsize = stepsize,
              max_depth = max_depth, tag = tag,
              n_threads = n_threads, seed = seed)
}


#' Run CmdStan with MPI (deprecated)
#'
#' @description `r lifecycle::badge("deprecated")`
#' Use \code{run_cmdstan(..., mpi = TRUE, mpi_nprocs = ncores)} instead.
#'
#' @param ncores Number of MPI processes (default 1).
#' @inheritParams run_cmdstan
#' @param max_depth Maximum tree depth.
#' @param metric_file Optional path to metric file.
#' @param debugging Ignored (was never used).
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
  .Deprecated("run_cmdstan(..., mpi = TRUE, mpi_nprocs = ncores)")
  adapt_args <- NULL
  if (!is.null(metric_file)) {
    adapt_args <- "init_buffer=0 window=0 term_buffer=50"
    warmup <- 50L
  }
  run_cmdstan(chain = chain, modelName = modelName, modelDir = modelDir,
              data = data, init = init, init_dir = init_dir,
              iter = iter, warmup = warmup, thin = thin,
              adapt_delta = adapt_delta, stepsize = stepsize,
              max_depth = max_depth, tag = tag, method = method,
              metric_file = metric_file, adapt_args = adapt_args,
              mpi = TRUE, mpi_nprocs = ncores, seed = seed)
}


#' Run model with OpenMP (worker, deprecated)
#'
#' @description `r lifecycle::badge("deprecated")`
#' Use \code{run_cmdstan(..., n_threads = n_threads)} instead.
#'
#' @param n_threads Number of threads.
#' @param model Full path to compiled Stan executable.
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
  .Deprecated("run_cmdstan(..., n_threads = n_threads)")
  output_prefix <- if (!is.null(tag)) paste0(model, "_", tag, "_") else model
  cmd <- .build_cmdstan_cmd(
    exe_path      = model,
    method        = "sample",
    data_file     = data,
    init_file     = init,
    output_prefix = output_prefix,
    chain         = chain,
    iter          = iter, warmup = warmup, thin = thin, seed = seed,
    adapt_delta   = adapt_delta, max_depth = max_depth, stepsize = stepsize,
    refresh       = 100, algorithm = algorithm,
    metric_file   = NULL, adapt_args = NULL,
    mpi           = FALSE, mpi_nprocs = 1,
    n_threads     = n_threads, cuda_modules = NULL
  )
  .run_system(cmd, description = "OpenMP CmdStan run")
}


#' Run model with MPI (worker, deprecated)
#'
#' @description `r lifecycle::badge("deprecated")`
#' Use \code{run_cmdstan(..., mpi = TRUE, mpi_nprocs = ncores)} instead.
#'
#' @param ncores Number of MPI processes.
#' @param model Full path to compiled Stan executable.
#' @param data Path to data file.
#' @param iter,warmup,thin Sampling parameters.
#' @param init Path to init file.
#' @param seed Random seed.
#' @param chain Chain ID.
#' @param stepsize,adapt_delta,max_depth Adaptation parameters.
#' @param metric_file Optional path to metric file.
#' @param method One of \code{"sample"} or \code{"variational"}.
#' @param tag Optional run tag.
#' @param debugging Ignored (was never used).
#' @export
mpi_runModel <- function(ncores, model, data, iter, warmup, thin, init,
                         seed, chain, stepsize, adapt_delta, max_depth,
                         metric_file = NULL, method = "sample",
                         tag = NULL, debugging = FALSE) {
  .Deprecated("run_cmdstan(..., mpi = TRUE, mpi_nprocs = ncores)")
  output_prefix <- if (!is.null(tag)) paste0(model, "_", tag, "_") else model
  adapt_args <- NULL
  if (!is.null(metric_file)) {
    adapt_args <- "init_buffer=0 window=0 term_buffer=50"
    warmup <- 50L
  }
  cmd <- .build_cmdstan_cmd(
    exe_path      = model,
    method        = method,
    data_file     = data,
    init_file     = init,
    output_prefix = output_prefix,
    chain         = chain,
    iter          = iter, warmup = warmup, thin = thin, seed = seed,
    adapt_delta   = adapt_delta, max_depth = max_depth, stepsize = stepsize,
    refresh       = 100, algorithm = "hmc engine=nuts",
    metric_file   = metric_file, adapt_args = adapt_args,
    mpi           = TRUE, mpi_nprocs = ncores,
    n_threads     = NULL, cuda_modules = NULL
  )
  .run_system(cmd, description = "MPI CmdStan run")
}
