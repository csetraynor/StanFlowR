## StanFlowR: Stan model execution functions (local, GPU, OpenMP, MPI)

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
  modelName <- basename(model)
  model <- file.path(model, modelName)
  if (!is.null(tag)) output <- paste0(model, "_", tag, "_") else output <- model

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

  modelName_base <- basename(prep$model)
  model_exec <- file.path(prep$model, modelName_base)
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
  modelName <- basename(model)
  model <- file.path(model, modelName)
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
  modelName <- basename(model)
  model <- file.path(model, modelName)
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
