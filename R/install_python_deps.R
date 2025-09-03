#' Install required Python dependencies for Numbers to CSV conversion
#'
#' This function installs the Python modules `numbers-parser` and `pandas`
#' using `reticulate::py_install()`.
#'
#' @param method Installation method: "auto", "pip", or "conda".
#' @param envname (Optional) Name of the virtual/conda environment to install to.
#' @export
#' @examples
#' \dontrun{
#' install_python_deps()  # installs into default environment
#' install_python_deps(method = "pip", envname = "r-reticulate")
#' }
install_python_deps <- function(method = c("auto", "pip", "conda"),
                                envname = NULL) {
  method <- match.arg(method)
  modules <- c("numbers-parser", "pandas")
  message("Installing required Python modules: ", paste(modules, collapse = ", "))
  reticulate::py_install(modules, method = method, envname = envname)
}
