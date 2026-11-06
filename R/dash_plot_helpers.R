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
#' control is absent when only one backend is configured, in which case
#' `input[[plotter_input]]` is `NULL` and this returns the configured backend
#' (`"ggrba"` when it is one of `plotters`, otherwise the first configured
#' value).
#'
#' @param input Shiny input object for the current module.
#' @param plotters Character vector of configured backends, e.g.
#'   `config$plotters` from [configure_dashboard()].
#' @param plotter_input Input identifier for the plot-style control.
#'
#' @return `"ggrba"` or `"ggplot"`.
dashboard_selected_plotter <- function(input, plotters = "ggplot", plotter_input = "plotter") {
  selected <- input[[plotter_input]]
  if (!is.null(selected)) {
    return(selected)
  }
  if ("ggrba" %in% plotters) "ggrba" else plotters[[1]]
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
#' It starts on when the default backend is `"ggplot"`, matching
#' [dashboard_sync_compatibility_mode()], which only reacts to later changes.
#'
#' @param ns A Shiny namespace function.
#' @param plotters Character vector of configured backends, e.g.
#'   `config$plotters` from [configure_dashboard()]. The "Plot style" control
#'   is shown only when more than one backend is configured, since there is
#'   otherwise only one available choice.
#' @param advanced_input Input identifier for the Advanced Options switch.
#' @param compatibility_input Input identifier for the renderer checkbox.
#' @param plotter_input Input identifier for the plot-style control.
#' @param advanced_controls Optional module-specific controls shown alongside
#'   the renderer checkbox when Advanced options is enabled.
#'
#' @return Shiny UI.
dashboard_plot_mode_controls <- function(
    ns,
    plotters = "ggplot",
    advanced_input = "show_plot_advanced_options",
    compatibility_input = "use_compatibility_plot",
    plotter_input = "plotter",
    advanced_controls = NULL) {
  plotter_labels <- c(ggrba = "RBA style", ggplot = "ggplot2")
  default_plotter <- if ("ggrba" %in% plotters) "ggrba" else plotters[[1]]
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
        value = identical(default_plotter, "ggplot")
      ),
      if (length(plotters) > 1) {
        shiny::radioButtons(
          ns(plotter_input),
          "Plot style:",
          choices = stats::setNames(plotters, plotter_labels[plotters]),
          selected = default_plotter,
          inline = TRUE
        )
      },
      advanced_controls
    )
  )
}

#' Return the distinct configured response-note caveats that apply to the
#' models/variables actually present in a plotted data frame.
#'
#' Matches each `response_notes` row by `model_name` (always) and, when that
#' row's `display_name` is non-NA, also by `display_name` - so a single
#' configuration mechanism covers both a whole-model caveat (e.g. "this model
#' is a linearised approximation") and a one-variable caveat (e.g. one
#' variable derived via a particular economic relationship), without any
#' model- or variable-name branching in this generic helper.
#'
#' Returns one element per distinct applicable note - never pre-joined into a
#' single string - since `ggrba::footnote_rba()` expects each footnote as its
#' own vector element (it numbers each with its own `*`/`**`/... marker via
#' `construct_caption()`); joining multiple notes into one string beforehand
#' would register them as a single numbered footnote instead. Callers
#' targeting `ggplot2::labs(caption = ...)` (a single string) should
#' `paste(..., collapse = "\n")` the result themselves.
#'
#' @param data Plotted long data frame with `model_name` and `display_name` columns.
#' @param response_notes Validated `response_notes` data frame from
#'   `configure_dashboard()` (`config$response_notes`).
#'
#' @return A character vector of distinct applicable notes, or `NULL` if none apply.
dashboard_response_notes_footnote <- function(data, response_notes) {
  if (is.null(response_notes) || nrow(response_notes) == 0 ||
      !is.data.frame(data) || !all(c("model_name", "display_name") %in% names(data))) {
    return(NULL)
  }

  applies <- vapply(seq_len(nrow(response_notes)), function(i) {
    rule <- response_notes[i, ]
    model_rows <- data$model_name %in% rule$model_name
    if (is.na(rule$display_name)) {
      any(model_rows)
    } else {
      any(model_rows & data$display_name %in% rule$display_name)
    }
  }, logical(1))

  notes <- unique(response_notes$note[applies])
  if (length(notes) == 0) NULL else notes
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
