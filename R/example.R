#' Prepare a CmdStan example
#'
#' Copies example Stan model files into the project directory structure.
#' Requires a CmdStan installation with example models.
#'
#' @param example Name of the example to prepare.
#'   Currently supports: \code{"pk2cpt"}, \code{"friberg"}.
#' @param cmdstan_dir Path to CmdStan installation directory.
#'   If \code{NULL}, resolved via \code{getOption("StanFlowR.cmdstan_path")}
#'   or \code{Sys.getenv("CMDSTAN_PATH")}.
#' @return Invisibly returns 0.
#' @export
prepare_example <- function(example = c("pk2cpt", "friberg"), cmdstan_dir = NULL) {
  example <- match.arg(example)
  cmdstan_dir <- .resolve_cmdstan_path(cmdstan_dir)

  examples_map <- list(
    pk2cpt = list(
      model_src = "example-models/pk2cpt/pk2cpt.stan",
      data_src = "example-models/pk2cpt/pk2cpt.data.R",
      init_src = "example-models/pk2cpt/pk2cpt.init.R",
      model_name = "pk2cpt"
    ),
    friberg = list(
      model_src = "example-models/FribergKarlsson/FribergKarlsson.stan",
      data_src = "example-models/FribergKarlsson/fribergKarlsson.data.R",
      init_src = "example-models/FribergKarlsson/fribergKarlsson.init.R",
      model_name = "friberg"
    )
  )

  ex <- examples_map[[example]]
  model_name <- ex$model_name

  model_file <- file.path(cmdstan_dir, ex$model_src)
  data_file <- file.path(cmdstan_dir, ex$data_src)
  init_file <- file.path(cmdstan_dir, ex$init_src)

  if (!file.exists(model_file)) {
    stop("Example model file not found: ", model_file, call. = FALSE)
  }

  # Copy model file and create directory structure
  local_stan <- file.path("Models", paste0(model_name, ".stan"))
  if (!file.exists(local_stan)) {
    file.copy(model_file, local_stan)
  }

  cmdstan_mkdir(model_name)

  # Copy data and init files
  data_dest <- file.path("Models", model_name, "data", paste0(model_name, ".data.R"))
  if (!file.exists(data_dest) && file.exists(data_file)) {
    file.copy(data_file, data_dest)
  }

  init_dest <- file.path("Models", model_name, "data", paste0(model_name, ".init.R"))
  if (!file.exists(init_dest) && file.exists(init_file)) {
    file.copy(init_file, init_dest)
  }

  message("Example '", example, "' prepared successfully.")
  invisible(0)
}


#' Compile and run a CmdStan example
#'
#' End-to-end demonstration: compiles and runs a short test of an example model.
#'
#' @param example Name of the example (\code{"pk2cpt"} or \code{"friberg"}).
#' @param cmdstan_dir Path to CmdStan installation (resolved automatically if NULL).
#' @return Invisibly returns 0.
#' @export
compile_run_example <- function(example = c("pk2cpt", "friberg"), cmdstan_dir = NULL) {
  example <- match.arg(example)

  prepare_example(example, cmdstan_dir = cmdstan_dir)
  compile_cmdstan(example, dir = cmdstan_dir)

  if (example == "pk2cpt") {
    inits <- function() {
      list(
        CL = stats::rlnorm(1, log(7)),
        ka = stats::rlnorm(1, log(1)),
        Q = stats::rlnorm(1, log(28)),
        sigma = stats::rlnorm(1, log(0.5)),
        V1 = stats::rlnorm(1, log(80)),
        V2 = stats::rlnorm(1, log(60))
      )
    }
  } else if (example == "friberg") {
    inits <- function() {
      list(
        alphaHat = stats::rlnorm(1, log(0.0002), 1),
        circ0Hat = stats::rlnorm(1, log(7), 1),
        CLHat = stats::rlnorm(1, log(19), 1),
        gamma = 0.168,
        kaHat = 6.81,
        mttHat = 116.4,
        omega = rep(0.1, 7),
        QHat = 16.5,
        sigma = 0.2,
        sigmaNeut = 0.2,
        V1Hat = 131.3,
        V2Hat = 312.2
      )
    }
  }

  run_cmdstan(chain = 1,
              init = inits,
              modelName = example,
              tag = "test",
              iter = 1,
              warmup = 1,
              data = file.path("Models", example, "data", paste0(example, ".data.R")))

  message("Example '", example, "' completed.")
  invisible(0)
}
