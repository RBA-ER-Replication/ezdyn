# Overview tab -------------------------------------------------------------

#' Build the namespaced Overview tab UI.
#'
#' @param id Module namespace.
#' @param overview_config Validated Overview configuration.
#'
#' @return Shiny UI.
UITab_Overview <- function(id, overview_config) {
  ns <- shiny::NS(id)

  shiny::uiOutput(ns("content"))
}

#' Serve the namespaced Overview tab.
#'
#' @param id Module namespace.
#' @param overview_config Validated Overview configuration.
#'
#' @return Invisibly, `NULL`.
ServerTab_Overview <- function(id, overview_config) {
  shiny::moduleServer(id, function(input, output, session) {
    output$content <- shiny::renderUI({
      if (is.function(overview_config$ui)) {
        return(overview_config$ui())
      }
      if (!is.null(overview_config$html_path) && file.exists(overview_config$html_path)) {
        return(shiny::tags$iframe(
          srcdoc = paste(readLines(overview_config$html_path, warn = FALSE), collapse = "\n"),
          title = "Dashboard overview",
          style = "width: 100%; min-height: 900px; border: 0;"
        ))
      }

      shiny::div(
        class = "alert alert-info",
        "Overview content is not available for this dashboard configuration."
      )
    })
  })
}
