#' Create project directory skeleton
#'
#' Creates a standard project directory structure for Stan modelling workflows.
#'
#' @param path Path to project directory (default current directory).
#' @param dirs Character vector of directory names to create.
#' @export
create_project_skeleton <- function(path = ".",
                                    dirs = c("Models", "Scripts", "DerivedData",
                                             "SourceData", "Simulations", "Img", "Results")) {
  for (d in dirs) {
    dir_path <- file.path(path, d)
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  }
  invisible(dirs)
}
