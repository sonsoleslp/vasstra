#' Launch the Interactive VaSStra App
#'
#' Opens a Shiny application that runs the complete VaSStra workflow
#' interactively: load data, map the subject, time, and indicator roles,
#' fit states and trajectories with automated or explicit counts, inspect
#' evaluation panels and tidy fit indices, rename groups, and export every
#' tidy table. The app requires the suggested `shiny` and `DT` packages.
#'
#' @param ... Passed to [shiny::runApp()].
#'
#' @return Called for its side effect of running the app.
#' @export
launch_app <- function(...) {
  # nocov start — interactive Shiny entrypoint, not reachable in batch tests
  for (pkg in c("shiny", "DT")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        "Package '%s' is required. Install it with: install.packages('%s')",
        pkg, pkg
      ), call. = FALSE)
    }
  }
  app_dir <- system.file("shiny", "vasstra_app", package = "VaSStra")
  if (!nzchar(app_dir)) {
    stop("Could not find the Shiny app directory. Re-install VaSStra.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
  # nocov end
}
