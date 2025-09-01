#' Run the Shiny Application
#'
#' @param onStart Startup function
#' @param options Options list passed to shinyApp
#' @param enableBookmarking Bookmarking mode
#' @param uiPattern URL pattern for the UI
#' @noRd
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  # Increase file upload size limit
  options(shiny.maxRequestSize = 2000 * 1024^2)

  # --- Ensure required packages are installed and loaded ---
  check_and_install_packages <- function(packages) {
    for (pkg in packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        message(paste("\U0001F4E6 Installing package:", pkg))
        tryCatch({
          install.packages(pkg, repos = "https://cran.rstudio.com/")
        }, error = function(e) {
          stop(paste("Failed to install package:", pkg, ". Error:", e$message))
        })
      }
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }
  }

  required_packages <- c("shiny", "DT", "dplyr", "ggplot2", "readr", "shinythemes", "tidyr")
  check_and_install_packages(required_packages)

  # --- Launch the app ---
  with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
