# Dashboard plot rendering ---------------------------------------------------

#' Save a dashboard plot, matching the renderer used to build it.
#'
#' `plotter` should match whichever backend built `plot` (e.g. the value
#' returned by [dashboard_selected_plotter()]), not be re-derived from `plot`
#' itself, since a plain ggplot2 object cannot be distinguished from one built
#' by [plot_pretty()] with `plotter = "ggplot"`.
#'
#' @param file Output file path.
#' @param plot A ggplot-compatible plot object.
#' @param plotter Plotting backend used to build `plot`. `NULL` (default)
#'   resolves via [ezdyn_resolve_plotter()].
#'
#' @return Invisibly, `NULL`.
ezdyn_ggsave <- function(file, plot, plotter = NULL) {
  plotter <- ezdyn_resolve_plotter(plotter)
  if (plotter == "ggrba") {
    ggrba::ggsave_rba(file, plot)
  } else {
    ggplot2::ggsave(file, plot)
  }
  invisible(NULL)
}

#' Render a dashboard plot in a temporary image file.
#'
#' Uses a PNG because it preserves the reviewed dashboard chart proportions.
#' This is the default renderer for dashboard plot modules.
#'
#' @param plt A ggplot-compatible plot object.
#' @param file_ext Image file extension.
#' @param plotter Plotting backend used to build `plt`. `NULL` (default)
#'   resolves via [ezdyn_resolve_plotter()].
#'
#' @return A list accepted by [shiny::renderImage()].
render_rbaplot <- function(plt, file_ext = ".png", plotter = NULL) {
  outfile <- tempfile(fileext = file_ext)
  if (!is.null(plt)) {
    suppressWarnings(ezdyn_ggsave(outfile, plt, plotter = plotter))
  }
  list(src = outfile, width = 800, alt = "Plot")
}

#' Resolve the dashboard's selected plotting backend.
#'
#' Reads the plot-style control added by [dashboard_plot_mode_controls()]. The
#' control is absent when the optional ggrba package is not installed, in
#' which case `input[[plotter_input]]` is `NULL` and this falls back to
#' `"ggplot"` via [ezdyn_resolve_plotter()].
#'
#' @param input Shiny input object for the current module.
#' @param plotter_input Input identifier for the plot-style control.
#'
#' @return `"ggrba"` or `"ggplot"`.
dashboard_selected_plotter <- function(input, plotter_input = "plotter") {
  ezdyn_resolve_plotter(input[[plotter_input]])
}

#' Sync plot compatibility mode to the selected plotting backend.
#'
#' Switches "Plot compatibility mode" on when `"ggplot"` is selected and off
#' when `"ggrba"` is selected, since [render_rbaplot()]'s direct PNG renderer
#' is tuned for `ggrba`'s chart proportions.
#'
#' @param input Shiny input object for the current module.
#' @param session Shiny session object for the current module.
#' @param plotter_input Input identifier for the plot-style control.
#' @param compatibility_input Input identifier for the compatibility-mode checkbox.
#'
#' @return Invisibly, `NULL`.
dashboard_sync_compatibility_mode <- function(
    input, session,
    plotter_input = "plotter",
    compatibility_input = "use_compatibility_plot") {
  shiny::observeEvent(input[[plotter_input]], {
    shiny::updateCheckboxInput(session, compatibility_input, value = identical(input[[plotter_input]], "ggplot"))
  }, ignoreInit = TRUE)
  invisible(NULL)
}

#' Build dashboard Advanced Options controls for plot rendering.
#'
#' This keeps the renderer selector in the tab's left panel, using the existing
#' Alternative Paths Advanced Options switch style. Compatibility mode enables
#' direct Shiny rendering for deployments that require separated plot panels.
#'
#' @param ns A Shiny namespace function.
#' @param advanced_input Input identifier for the Advanced Options switch.
#' @param compatibility_input Input identifier for the renderer checkbox.
#' @param plotter_input Input identifier for the plot-style control. Hidden
#'   entirely when the optional ggrba package is not installed, since there is
#'   then only one available backend.
#' @param advanced_controls Optional module-specific controls shown alongside
#'   the renderer checkbox when Advanced options is enabled.
#'
#' @return Shiny UI.
dashboard_plot_mode_controls <- function(
    ns,
    advanced_input = "show_plot_advanced_options",
    compatibility_input = "use_compatibility_plot",
    plotter_input = "plotter",
    advanced_controls = NULL) {
  shiny::tagList(
    shinyWidgets::prettySwitch(
      ns(advanced_input),
      label = "Advanced options",
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = sprintf("input.%s === true", advanced_input),
      ns = ns,
      shiny::checkboxInput(
        ns(compatibility_input),
        "Plot compatibility mode",
        value = FALSE
      ),
      if (requireNamespace("ggrba", quietly = TRUE)) {
        shiny::radioButtons(
          ns(plotter_input),
          "Plot style:",
          choices = c("RBA style" = "ggrba", "ggplot2" = "ggplot"),
          selected = "ggrba",
          inline = TRUE
        )
      },
      advanced_controls
    )
  )
}

#' Build dashboard plot outputs with direct and compatibility renderers.
#'
#' Default mode writes the plot to a temporary PNG using [render_rbaplot()].
#' Compatibility mode displays the plot through [shiny::renderPlot()]. A 400px
#' height matches the legacy IRF image output size.
#'
#' @param ns A Shiny namespace function.
#' @param height Plot height in pixels.
#' @param compatibility_input Input identifier for the renderer checkbox.
#' @param direct_output Output identifier for the direct plot.
#' @param compatibility_output Output identifier for the temporary-file image.
#'
#' @return Shiny UI.
dashboard_plot_mode_ui <- function(
    ns,
    height = 400,
    compatibility_input = "use_compatibility_plot",
    direct_output = "plot_direct",
    compatibility_output = "plot_image") {
  compatibility_id <- ns(compatibility_input)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === true", compatibility_id),
      shiny::plotOutput(ns(direct_output), height = height)
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] !== true", compatibility_id),
      shiny::imageOutput(ns(compatibility_output), height = height)
    )
  )
}

#' Bind direct and temporary-file dashboard plot outputs.
#'
#' @param output Shiny output object for the current module.
#' @param graph Reactive function returning a ggplot-compatible plot object.
#' @param compatibility_mode Reactive function returning one logical value.
#' @param plotter Optional reactive function returning the plotting backend
#'   (`"ggrba"` or `"ggplot"`) used to build `graph()`, e.g.
#'   [dashboard_selected_plotter()]. `NULL` (default) resolves automatically.
#' @param direct_output Output identifier for the direct plot.
#' @param compatibility_output Output identifier for the temporary-file image.
#'
#' @return Invisibly, `NULL`.
dashboard_bind_plot_mode <- function(
    output,
    graph,
    compatibility_mode,
    plotter = NULL,
    direct_output = "plot_direct",
    compatibility_output = "plot_image") {
  # 1. Compatibility mode uses direct Shiny rendering for separate plot panels.
  output[[direct_output]] <- shiny::renderPlot({
    shiny::req(compatibility_mode())
    graph_object <- graph()
    shiny::req(graph_object)
    graph_object
  })

  # 2. Default mode uses the established PNG renderer and its chart proportions.
  output[[compatibility_output]] <- shiny::renderImage({
    shiny::req(!compatibility_mode())
    graph_object <- graph()
    shiny::req(graph_object)
    render_rbaplot(graph_object, plotter = if (is.null(plotter)) NULL else plotter())
  }, deleteFile = TRUE)

  invisible(NULL)
}
