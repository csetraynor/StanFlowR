## StanFlowR: Nextflow file generation utilities

#' Generate a Nextflow file for CmdStan SLURM jobs
#'
#' Writes a Nextflow DSL2 `.nf` file that submits CmdStan sampling runs as
#' SLURM jobs, with an embedded Rscript per process execution.
#'
#' @param outfile Character. Path to write the `.nf` file.
#' @param queue Character. SLURM queue/partition name.
#' @param memory Character. Memory allocation (e.g. \code{"8 GB"}).
#' @param time_limit Character. Wall-time limit (e.g. \code{"24h"}).
#' @param label Character. Nextflow process label.
#' @param ncores Integer. Number of tasks passed to \code{--ntasks}.
#' @param module_load Character. Colon-separated module string passed to
#'   the Nextflow \code{module} directive.
#' @param n_chains Integer. Number of MCMC chains.
#' @param methods Character. Sampling method (e.g. \code{"sample"}).
#' @param initfile Character. Path to the R inits script sourced in the job.
#' @param model_names Character vector. One or more Stan model names.
#' @param data_file Character. Path to the Stan JSON data file.
#' @param tag_name Character. Run tag appended to output file names.
#' @param iter Integer. Number of post-warmup iterations.
#' @param warmup Integer. Number of warmup iterations.
#' @param compile Logical. Whether to compile the model inside the job.
#' @param source_pkg Character. Path passed to \code{source_all()}.
#' @param init_call Character. R expression string for the \code{init}
#'   argument of \code{run_cmdstan()}, inserted verbatim. Example:
#'   \code{"make_init(standatafile = datafile)"}.
#' @param process_name Character. Name of the Nextflow process block.
#'   Default \code{"mH"}.
#' @param executor Character. Nextflow executor. Default \code{"slurm"}.
#' @param overwrite Logical. Overwrite \code{outfile} if it exists?
#'   Default \code{FALSE}.
#' @param additional_cluster_options Character or \code{NULL}. Extra options
#'   appended to the \code{clusterOptions} directive. Default \code{NULL}.
#' @return Invisibly, the path \code{outfile}.
#' @export
generate_nextflow <- function(outfile,
                              queue,
                              memory,
                              time_limit,
                              label,
                              ncores,
                              module_load,
                              n_chains,
                              methods,
                              initfile,
                              model_names,
                              data_file,
                              tag_name,
                              iter,
                              warmup,
                              compile,
                              source_pkg,
                              init_call,
                              process_name = "mH",
                              executor = "slurm",
                              overwrite = FALSE,
                              additional_cluster_options = NULL) {

  # --- input validation ---
  .nfw_check_string  <- function(x, nm) {
    if (!is.character(x) || length(x) != 1L || !nzchar(x))
      stop("'", nm, "' must be a non-empty character string.", call. = FALSE)
  }
  .nfw_check_int <- function(x, nm) {
    if (!is.numeric(x) || length(x) != 1L || x != as.integer(x) || x < 1L)
      stop("'", nm, "' must be a positive integer.", call. = FALSE)
  }

  .nfw_check_string(outfile,       "outfile")
  .nfw_check_string(queue,         "queue")
  .nfw_check_string(memory,        "memory")
  .nfw_check_string(time_limit,    "time_limit")
  .nfw_check_string(label,         "label")
  .nfw_check_int(ncores,           "ncores")
  .nfw_check_string(module_load,   "module_load")
  .nfw_check_int(n_chains,         "n_chains")
  .nfw_check_string(methods,       "methods")
  .nfw_check_string(initfile,      "initfile")
  .nfw_check_string(data_file,     "data_file")
  .nfw_check_string(tag_name,      "tag_name")
  .nfw_check_int(iter,             "iter")
  .nfw_check_int(warmup,           "warmup")
  .nfw_check_string(source_pkg,    "source_pkg")
  .nfw_check_string(init_call,     "init_call")
  .nfw_check_string(process_name,  "process_name")
  .nfw_check_string(executor,      "executor")

  if (!is.character(model_names) || length(model_names) == 0L ||
      any(!nzchar(model_names)))
    stop("'model_names' must be a non-empty character vector.", call. = FALSE)
  if (!is.logical(compile) || length(compile) != 1L)
    stop("'compile' must be a single logical value.", call. = FALSE)
  if (!is.logical(overwrite) || length(overwrite) != 1L)
    stop("'overwrite' must be a single logical value.", call. = FALSE)
  if (!is.null(additional_cluster_options) &&
      (!is.character(additional_cluster_options) ||
       length(additional_cluster_options) != 1L))
    stop("'additional_cluster_options' must be a single character string or NULL.",
         call. = FALSE)

  if (file.exists(outfile) && !overwrite)
    stop("'", outfile, "' already exists. Set overwrite = TRUE to replace it.",
         call. = FALSE)

  # --- build derived values ---
  cluster_opts <- paste0("'--ntasks=", ncores, "'")
  if (!is.null(additional_cluster_options) && nzchar(additional_cluster_options))
    cluster_opts <- paste0(cluster_opts, " + ' ", additional_cluster_options, "'")

  compile_str <- if (compile) "TRUE" else "FALSE"

  # Channel.from() takes a bare list for multiple items, or a single quoted string
  model_names_nf <- if (length(model_names) == 1L) {
    paste0('"', model_names, '"')
  } else {
    paste0('"', model_names, '"', collapse = ", ")
  }

  chains_range <- if (n_chains == 1L) "1" else paste0("1 .. ", n_chains)

  # --- template ---
  # Nextflow uses !{var} interpolation inside shell: blocks.
  # We write those literally — no R interpolation of those tokens.
  nf_text <- paste0(
    '#!/usr/bin/env nextflow\n',
    '\n',
    'process ', process_name, ' {\n',
    '\n',
    '  executor \'', executor, '\'\n',
    '  queue \'', queue, '\'\n',
    '  memory \'', memory, '\'\n',
    '  time "', time_limit, '"\n',
    '  label \'', label, '\'\n',
    '  clusterOptions ', cluster_opts, '\n',
    '\n',
    '  module \'', module_load, '\'\n',
    '\n',
    '  input:\n',
    '    each chain\n',
    '    val run_method\n',
    '    val initsfile\n',
    '    each model_name\n',
    '    val data_file\n',
    '    val tag_name\n',
    '\n',
    '  shell:\n',
    '  """\n',
    '  #!/usr/bin/env Rscript\n',
    '\n',
    '  .libPaths()\n',
    '\n',
    '  setwd("!{launchDir}")\n',
    '\n',
    '  compile   = ', compile_str, '\n',
    '\n',
    '  datafile  = "!{data_file}"\n',
    '  tag2      = "!{tag_name}"\n',
    '  method    = "!{run_method}"\n',
    '  mod_name  = "!{model_name}"\n',
    '  label_tag = paste0(method, tag2)\n',
    '\n',
    '  devtools::load_all("', source_pkg, '")\n',
    '  source("!{initsfile}")\n',
    '\n',
    '  start.time <- Sys.time()\n',
    '  run_cmdstan(ncores    = ', ncores, ',\n',
    '              chain     = !{chain},\n',
    '              init      = ', init_call, ',\n',
    '              modelName = mod_name,\n',
    '              tag       = label_tag,\n',
    '              iter      = ', iter, ',\n',
    '              warmup    = ', warmup, ',\n',
    '              method    = method,\n',
    '              data      = datafile)\n',
    '  end.time <- Sys.time()\n',
    '  time.taken <- end.time - start.time\n',
    '  time.taken\n',
    '\n',
    '  """\n',
    '}\n',
    '\n',
    'workflow {\n',
    '\n',
    '    chains     = Channel.from(', chains_range, ')\n',
    '    methods    = "', methods, '"\n',
    '    initfile   = \'', initfile, '\'\n',
    '    model_name = Channel.from(', model_names_nf, ')\n',
    '    data_file  = "', data_file, '"\n',
    '    tag_name   = "', tag_name, '"\n',
    '\n',
    '    ', process_name, '(chains, methods, initfile, model_name, data_file, tag_name)\n',
    '}\n'
  )

  # --- write ---
  out_dir <- dirname(outfile)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  writeLines(nf_text, con = outfile)
  message("Nextflow file written to: ", outfile)
  invisible(outfile)
}


#' Launch a Nextflow workflow
#'
#' Constructs and optionally executes the shell command to run a generated
#' Nextflow file.
#'
#' @param nf_file Character. Path to the \code{.nf} file.
#' @param work_dir Character. Nextflow work directory. Default \code{"work"}.
#' @param resume Logical. Append \code{-resume} flag? Default \code{FALSE}.
#' @param dry_run Logical. Print the command without executing it?
#'   Default \code{FALSE}.
#' @param nextflow_bin Character. Path (or name) of the \code{nextflow}
#'   executable. Default \code{"nextflow"}.
#' @return Invisibly, the command string. When \code{dry_run = FALSE} also
#'   returns the system exit code as an attribute \code{"exit_code"}.
#' @export
launch_nextflow <- function(nf_file,
                            work_dir = "work",
                            resume = FALSE,
                            dry_run = FALSE,
                            nextflow_bin = "nextflow") {
  if (!is.character(nf_file) || length(nf_file) != 1L || !nzchar(nf_file))
    stop("'nf_file' must be a non-empty character string.", call. = FALSE)
  if (!is.character(work_dir) || length(work_dir) != 1L || !nzchar(work_dir))
    stop("'work_dir' must be a non-empty character string.", call. = FALSE)
  if (!is.logical(resume)  || length(resume)  != 1L)
    stop("'resume' must be a single logical.", call. = FALSE)
  if (!is.logical(dry_run) || length(dry_run) != 1L)
    stop("'dry_run' must be a single logical.", call. = FALSE)
  if (!is.character(nextflow_bin) || length(nextflow_bin) != 1L ||
      !nzchar(nextflow_bin))
    stop("'nextflow_bin' must be a non-empty character string.", call. = FALSE)

  resume_flag <- if (resume) " -resume" else ""
  cmd <- paste0(nextflow_bin, " run ", nf_file,
                " -w ", work_dir,
                resume_flag)

  if (dry_run) {
    message("Dry run — command not executed:\n  ", cmd)
    return(invisible(cmd))
  }

  message("Running: ", cmd)
  exit_code <- system(cmd)
  if (exit_code != 0L)
    warning("Nextflow exited with code ", exit_code, call. = FALSE)

  result <- cmd
  attr(result, "exit_code") <- exit_code
  invisible(result)
}
