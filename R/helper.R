#Ensure Python modules are available
.require_py_modules <- function(modules,
                                auto_install = FALSE,
                                method = c("pip", "auto"),
                                envname = NULL) {
  missing <- modules[!vapply(modules, reticulate::py_module_available, logical(1))]

  if (length(missing) == 0) return(invisible(TRUE))

  method <- match.arg(method)

  msg <- paste0("Missing Python modules: ", paste(missing, collapse = ", "))
  install_call <- paste0(
    "reticulate::py_install(c(", paste(sprintf('\"%s\"', missing), collapse = ", "), ")",
    if (!is.null(envname)) paste0(", envname = \"", envname, "\"") else "",
    if (method != "auto") paste0(", method = \"", method, "\"") else "",
    ")"
  )

  if (auto_install) {
    message(msg, "Installing automatically...")
    reticulate::py_install(missing, method = method, envname = envname)
  } else {
    stop(msg, "\nInstall them via: ", install_call, call. = FALSE)
  }

  invisible(TRUE)
}
