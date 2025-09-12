#' Run the Shiny Application
#'
#' @param onStart Startup function
#' @param options Options list passed to shinyApp
#' @param enableBookmarking Bookmarking mode
#' @param uiPattern URL pattern for the UI
#' @param ... Additional options passed to `golem_opts`
#' @export
run_app <- function(...) {
  shinyApp(
    ui = app_ui,
    server = app_server
  )
}
