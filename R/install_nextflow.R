## StanFlowR: Nextflow installation utility

#' Install Nextflow on a Linux system
#'
#' Downloads and installs the Nextflow workflow manager into a user-writable
#' directory, optionally updating PATH for the current R session and
#' \code{~/.bashrc}. Designed for HPC environments where users lack root access.
#'
#' @param version Character. Nextflow version to install. Use \code{"latest"}
#'   to install via the official \url{https://get.nextflow.io} installer script,
#'   or supply a specific version string (e.g. \code{"23.10.1"}) to download
#'   that release directly from GitHub.
#' @param install_dir Character. Directory in which to place the
#'   \code{nextflow} binary. Created recursively if it does not exist.
#'   Default \code{"~/bin"}.
#' @param add_to_path Logical. If \code{TRUE}, prepend \code{install_dir} to
#'   \code{PATH} in the current R session via \code{Sys.setenv()}.
#'   Default \code{TRUE}.
#' @param update_bashrc Logical. If \code{TRUE} and \code{add_to_path} is
#'   \code{TRUE}, append an \code{export PATH} line to \code{~/.bashrc} (only
#'   if the exact line is not already present). Default \code{FALSE}.
#' @param java_check Logical. If \code{TRUE}, verify that Java 11+ is
#'   available before proceeding; issues a \code{warning()} (not an error) if
#'   Java is absent or below version 11. Default \code{TRUE}.
#' @param force Logical. If \code{FALSE} and the binary already exists in
#'   \code{install_dir}, skip installation and report the installed version.
#'   If \code{TRUE}, overwrite the existing binary. Default \code{FALSE}.
#'
#' @return A named list (invisibly) with components:
#'   \describe{
#'     \item{success}{Logical. \code{TRUE} if the binary is present and
#'       executable after the call.}
#'     \item{version_installed}{Character. Version string parsed from
#'       \code{nextflow -version}, or \code{NA} on failure.}
#'     \item{path}{Character. Full path to the installed binary.}
#'     \item{java_ok}{Logical. Whether Java 11+ was detected (\code{NA} if
#'       \code{java_check = FALSE}).}
#'   }
#'
#' @examples
#' \dontrun{
#' # Install latest Nextflow to ~/bin and update current session PATH
#' install_nextflow()
#'
#' # Install a specific version to a project-local directory
#' install_nextflow(
#'   version     = "23.10.1",
#'   install_dir = "~/tools/nextflow",
#'   update_bashrc = TRUE
#' )
#'
#' # Force reinstall latest without Java check
#' install_nextflow(force = TRUE, java_check = FALSE)
#' }
#'
#' @export
install_nextflow <- function(version      = "latest",
                             install_dir  = "~/bin",
                             add_to_path  = TRUE,
                             update_bashrc = FALSE,
                             java_check   = TRUE,
                             force        = FALSE) {

  # --- input validation ---
  if (!is.character(version) || length(version) != 1L || !nzchar(version))
    stop("'version' must be a non-empty character string.", call. = FALSE)
  if (!is.character(install_dir) || length(install_dir) != 1L || !nzchar(install_dir))
    stop("'install_dir' must be a non-empty character string.", call. = FALSE)
  for (nm in c("add_to_path", "update_bashrc", "java_check", "force")) {
    val <- get(nm)
    if (!is.logical(val) || length(val) != 1L || is.na(val))
      stop("'", nm, "' must be TRUE or FALSE.", call. = FALSE)
  }

  install_dir <- path.expand(install_dir)
  nf_bin      <- file.path(install_dir, "nextflow")
  java_ok     <- NA

  # --- check writability ---
  if (dir.exists(install_dir)) {
    if (file.access(install_dir, mode = 2L) != 0L)
      stop("'", install_dir, "' exists but is not writable. ",
           "Choose a different install_dir.", call. = FALSE)
  }

  # --- Java check ---
  if (java_check) {
    message("Checking Java version...")
    java_ok <- .nfi_check_java()
  }

  # --- already installed? ---
  if (file.exists(nf_bin) && !force) {
    ver_out <- system2(nf_bin, args = "-version", stdout = TRUE, stderr = TRUE)
    message("Nextflow already installed at: ", nf_bin)
    message(paste(ver_out, collapse = "\n"))
    version_str <- .nfi_parse_version(ver_out)
    return(invisible(list(
      success           = TRUE,
      version_installed = version_str,
      path              = nf_bin,
      java_ok           = java_ok
    )))
  }

  # --- create install_dir ---
  if (!dir.exists(install_dir)) {
    message("Creating directory: ", install_dir)
    dir.create(install_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(install_dir))
      stop("Failed to create directory: ", install_dir, call. = FALSE)
  }

  # --- download / install ---
  if (version == "latest") {
    .nfi_install_latest(install_dir, nf_bin)
  } else {
    .nfi_install_version(version, nf_bin)
  }

  # --- make executable ---
  chmod_ok <- tryCatch({
    Sys.chmod(nf_bin, mode = "0755")
    TRUE
  }, warning = function(w) { warning(w); FALSE },
     error   = function(e) { warning("chmod failed: ", conditionMessage(e)); FALSE })

  if (!file.exists(nf_bin))
    stop("Installation appeared to succeed but binary not found at: ", nf_bin,
         call. = FALSE)

  # --- update PATH in current session ---
  if (add_to_path) {
    current_path <- Sys.getenv("PATH")
    path_dirs    <- strsplit(current_path, ":", fixed = TRUE)[[1L]]
    if (!install_dir %in% path_dirs) {
      Sys.setenv(PATH = paste(install_dir, current_path, sep = ":"))
      message("Added '", install_dir, "' to PATH for this R session.")
    }

    if (update_bashrc) {
      .nfi_update_bashrc(install_dir)
    }
  }

  # --- verify ---
  message("Verifying installation...")
  ver_out <- tryCatch(
    system2(nf_bin, args = "-version", stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  version_str <- .nfi_parse_version(ver_out)
  if (length(ver_out) > 0L) {
    message("Installation complete.\n", paste(ver_out, collapse = "\n"))
  } else {
    warning("Binary installed but could not retrieve version information.",
            call. = FALSE)
  }

  invisible(list(
    success           = file.exists(nf_bin),
    version_installed = version_str,
    path              = nf_bin,
    java_ok           = java_ok
  ))
}


# ---------------------------------------------------------------------------
# Internal helpers (unexported)
# ---------------------------------------------------------------------------

#' Check Java version; return TRUE if >= 11
#' @noRd
.nfi_check_java <- function() {
  java_out <- tryCatch(
    system2("java", args = "-version", stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )

  if (length(java_out) == 0L) {
    warning(
      "Java not found on PATH. Nextflow requires Java 11+.\n",
      "On HPC systems try: module load Java/11\n",
      "Or install Java 11+ before running Nextflow.",
      call. = FALSE
    )
    return(FALSE)
  }

  # java -version prints to stderr; the first line contains the version
  version_line <- java_out[1L]
  m <- regmatches(version_line,
                  regexpr('[0-9]+(?:[._][0-9]+)*', version_line))
  if (length(m) == 0L) {
    warning("Could not parse Java version from: ", version_line, call. = FALSE)
    return(FALSE)
  }

  # "1.8.0_xxx" → major 8; "11.0.2" → major 11; "17" → major 17
  parts <- strsplit(m, "[._]")[[1L]]
  major <- as.integer(parts[1L])
  if (!is.na(major) && major == 1L && length(parts) >= 2L)
    major <- as.integer(parts[2L])  # old-style 1.8 → 8

  if (is.na(major) || major < 11L) {
    warning(
      "Java ", m, " detected, but Nextflow requires Java 11+.\n",
      "On HPC systems try: module load Java/11",
      call. = FALSE
    )
    return(FALSE)
  }

  message("Java ", m, " detected. OK.")
  TRUE
}


#' Install latest Nextflow via the official installer script
#' @noRd
.nfi_install_latest <- function(install_dir, nf_bin) {
  message("Downloading and installing latest Nextflow via https://get.nextflow.io ...")

  # Run the installer inside install_dir using a single bash -c command so
  # the child shell's CWD is install_dir — that is where the installer drops
  # the `nextflow` binary.  R's setwd() does NOT affect child processes.
  cmd <- paste0("cd '", install_dir, "' && curl -fsSL https://get.nextflow.io | bash")
  output <- system2("bash", args = c("-c", cmd), stdout = TRUE, stderr = TRUE)

  exit_code <- attr(output, "status")
  if (length(output) > 0L)
    message("Installer output:\n", paste(output, collapse = "\n"))

  if (!is.null(exit_code) && exit_code != 0L)
    stop("Nextflow installer failed with exit code ", exit_code, ".\n",
         "Output:\n", paste(output, collapse = "\n"), call. = FALSE)

  if (!file.exists(nf_bin)) {
    # Provide diagnostics before giving up
    alt_locations <- c(
      file.path(getwd(), "nextflow"),
      file.path(tempdir(), "nextflow")
    )
    found_at <- alt_locations[file.exists(alt_locations)]
    hint <- if (length(found_at) > 0L) {
      paste0("\nBinary found at unexpected location: ", found_at[1L],
             "\nTry setting install_dir to that directory.")
    } else ""
    stop("Installer ran but 'nextflow' binary not found at: ", nf_bin, hint,
         call. = FALSE)
  }
}


#' Download a specific Nextflow release binary from GitHub
#' @noRd
.nfi_install_version <- function(version, nf_bin) {
  url <- paste0("https://github.com/nextflow-io/nextflow/releases/download/v",
                version, "/nextflow")
  message("Downloading Nextflow ", version, " from:\n  ", url)

  tmp_bin <- tempfile()
  on.exit(unlink(tmp_bin), add = TRUE)

  result <- tryCatch(
    download.file(url, destfile = tmp_bin, mode = "wb", quiet = FALSE,
                  method = "auto"),
    error = function(e)
      stop("Download failed: ", conditionMessage(e),
           "\nURL attempted: ", url,
           "\nCheck that version '", version,
           "' exists at https://github.com/nextflow-io/nextflow/releases",
           call. = FALSE)
  )

  if (result != 0L)
    stop("download.file() returned non-zero status for URL:\n  ", url,
         "\nCheck the version string and network connectivity.", call. = FALSE)

  file_size <- file.info(tmp_bin)$size
  if (is.na(file_size) || file_size < 1000L)
    stop("Downloaded file appears empty or truncated (", file_size, " bytes).\n",
         "URL attempted: ", url, call. = FALSE)

  file.copy(tmp_bin, nf_bin, overwrite = TRUE)
}


#' Append export PATH line to ~/.bashrc if not already present
#' @noRd
.nfi_update_bashrc <- function(install_dir) {
  bashrc      <- path.expand("~/.bashrc")
  export_line <- paste0('export PATH="', install_dir, ':$PATH"')
  comment     <- "# Added by StanFlowR::install_nextflow()"

  existing <- if (file.exists(bashrc)) readLines(bashrc, warn = FALSE) else character(0)

  if (export_line %in% existing) {
    message("PATH export already present in ~/.bashrc — not modified.")
    return(invisible(NULL))
  }

  cat("\n", comment, "\n", export_line, "\n",
      file = bashrc, append = TRUE, sep = "")
  message("Appended to ~/.bashrc:\n  ", export_line,
          "\nRestart your shell or run: source ~/.bashrc")
}


#' Parse Nextflow version string from `nextflow -version` output
#' @noRd
.nfi_parse_version <- function(lines) {
  if (length(lines) == 0L) return(NA_character_)
  # Look for a line like "      N E X T F L O W  version 23.10.1 build ..."
  # or simply any token matching major.minor.patch
  m <- regmatches(lines,
                  regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", lines))
  if (length(m) == 0L || !nzchar(m[1L])) return(NA_character_)
  m[1L]
}
