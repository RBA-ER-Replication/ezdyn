# Dashboard plot rendering ---------------------------------------------------

#' Height a ggplot2 chart needs at a given width.
#'
#' The title, facet strips, axes, legend and caption of a ggplot2 chart take a
#' fixed height; the panels get whatever is left. A wrapped legend or several
#' footnotes can therefore squeeze the panels to nothing, or push the caption
#' off the bottom of a fixed-height canvas. This returns `min_height`, or more
#' when the panels would otherwise get less than `min_panel_height` (or
#' `min_panel_row_height` per row of facet panels). Text is measured on a
#' throwaway, file-less device, so no default graphics device is opened.
#'
#' @param plot A plot object. Anything that is not a ggplot gets `min_height`.
#' @param width Canvas width in inches.
#' @param min_height Minimum canvas height in inches.
#' @param min_panel_height,min_panel_row_height Minimum panel height in
#'   inches, for the whole chart and for each row of facet panels.
#'
#' @return Canvas height in inches.
#' @keywords internal
ezdyn_ggplot_height <- function(plot, width, min_height, min_panel_height = 2.5, min_panel_row_height = 1.6) {
  if (!inherits(plot, "ggplot")) {
    return(min_height)
  }
  old_dev <- grDevices::dev.cur()
  grDevices::pdf(NULL, width = width, height = min_height)
  measure_dev <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(measure_dev)
    if (old_dev > 1) grDevices::dev.set(old_dev)
  }, add = TRUE)
  gt <- ggplot2::ggplotGrob(plot)
  # Panel rows are "null" units, which convert to 0: the sum is the fixed height.
  fixed_h <- sum(grid::convertHeight(gt$heights, "in", valueOnly = TRUE))
  panel_rows <- length(unique(gt$layout$t[grepl("^panel", gt$layout$name)]))
  max(min_height, fixed_h + max(min_panel_height, panel_rows * min_panel_row_height))
}

#' Save a dashboard plot, matching the renderer used to build it.
#'
#' `plotter` should match whichever backend built `plot` (e.g. the value
#' returned by [dashboard_selected_plotter()]), not be re-derived from `plot`
#' itself, since a plain ggplot2 object cannot be distinguished from one built
#' by [plot_pretty()] with `plotter = "ggplot"`.
#'
#' A ggplot2 chart is saved `width` inches wide and at least `height` inches
#' tall: taller when its legend and footnotes need the room (see
#' [ezdyn_ggplot_height()]). Its size therefore never depends on whichever
#' graphics device happens to be open. `ggrba` charts are saved by
#' `ggrba::ggsave_rba()` at its own size.
#'
#' @param file Output file path.
#' @param plot A ggplot-compatible plot object.
#' @param plotter Plotting backend used to build `plot`. `NULL` (default)
#'   resolves via [ezdyn_resolve_plotter()].
#' @param width,height Width and minimum height in inches of a ggplot2 chart
#'   (ignored for `ggrba`).
#' @param dpi Resolution of raster files such as PNG.
#'
#' @return Invisibly, the saved size as `list(width, height)` in inches for a
#'   ggplot2 chart, or `NULL` for `ggrba`.
ezdyn_ggsave <- function(file, plot, plotter = NULL, width = 7, height = 6, dpi = 300) {
  plotter <- ezdyn_resolve_plotter(plotter)
  if (plotter == "ggrba") {
    ggrba::ggsave_rba(file, plot)
    return(invisible(NULL))
  }
  # Whole pixels at `dpi`, so a dashboard PNG is exactly the height reported.
  height <- ceiling(ezdyn_ggplot_height(plot, width, height) * dpi - 1e-6) / dpi
  ggplot2::ggsave(file, plot, width = width, height = height, units = "in", dpi = dpi)
  invisible(list(width = width, height = height))
}

#' Render a dashboard plot in a temporary image file.
#'
#' This is the default renderer for dashboard plot modules. A ggplot2 chart is
#' drawn at the output's own width in CSS pixels, at 72 dpi as
#' [shiny::renderPlot()] draws (times `pixelratio` for high-DPI screens), so it
#' looks the same as in compatibility mode. It is `height` pixels tall, or
#' taller when its legend and footnotes need the room. A `ggrba` chart keeps
#' the proportions `ggrba::ggsave_rba()` gives it.
#'
#' @param plt A ggplot-compatible plot object.
#' @param file_ext Image file extension.
#' @param plotter Plotting backend used to build `plt`. `NULL` (default)
#'   resolves via [ezdyn_resolve_plotter()].
#' @param width Output width in CSS pixels.
#' @param height Minimum output height in CSS pixels.
#' @param pixelratio Browser device pixel ratio
#'   (`session$clientData$pixelratio`).
#'
#' @return A list accepted by [shiny::renderImage()].
render_rbaplot <- function(plt, file_ext = ".png", plotter = NULL, width = 800, height = 400, pixelratio = 1) {
  outfile <- tempfile(fileext = file_ext)
  if (is.null(plt)) {
    return(list(src = outfile, width = width, alt = "Plot"))
  }
  plotter <- ezdyn_resolve_plotter(plotter)
  if (plotter == "ggrba") {
    suppressWarnings(ezdyn_ggsave(outfile, plt, plotter = plotter))
    return(list(src = outfile, width = 800, alt = "Plot"))
  }
  size <- suppressWarnings(ezdyn_ggsave(
    outfile, plt, plotter = plotter,
    width = width / 72, height = height / 72, dpi = 72 * pixelratio
  ))
  list(
    src = outfile,
    width = width,
    height = round(size$height * 72),
    alt = "Plot",
    class = "ezdyn-fitted-plot"
  )
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
#' targeting `ggplot2::labs(caption = ...)` (a single string) should use
#' [dashboard_ggplot_caption()], which wraps and joins them.
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

#' Build the ggplot2 caption layer for dashboard footnotes.
#'
#' ggplot2 draws a caption one line per newline and never wraps it, so long
#' response notes ran off the right edge of the plot. Each footnote starts on
#' its own line and is wrapped to fit the narrowest dashboard plot (see
#' `ezdyn_ggplot_wrap_widths()`). The ggrba backend keeps
#' `ggrba::footnote_rba()`.
#'
#' @param footnotes Character vector of footnotes, one element per note, e.g.
#'   from [dashboard_response_notes_footnote()].
#'
#' @return A `ggplot2::labs()` caption layer.
dashboard_ggplot_caption <- function(footnotes) {
  footnotes <- ezdyn_wrap_text(footnotes, ezdyn_ggplot_wrap_widths()$caption)
  ggplot2::labs(caption = paste(footnotes, collapse = "\n"))
}

#' Build dashboard plot outputs with direct and compatibility renderers.
#'
#' Default mode writes the plot to a temporary PNG using [render_rbaplot()].
#' Compatibility mode displays the plot through [shiny::renderPlot()]. Both
#' are at least `height` pixels tall (400px matches the legacy IRF image
#' output size), and taller when a ggplot2 chart's legend or footnotes need
#' the room.
#'
#' @param ns A Shiny namespace function.
#' @param height Minimum plot height in pixels.
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
  # The outputs take their height from the rendered chart; the frame keeps
  # `height` free for it before the first render.
  plot_frame <- function(output) {
    shiny::tags$div(
      class = "ezdyn-plot-frame",
      style = sprintf("min-height: %s;", shiny::validateCssUnit(height)),
      output
    )
  }

  shiny::tagList(
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === true", compatibility_id),
      plot_frame(shiny::plotOutput(ns(direct_output), height = "auto"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] !== true", compatibility_id),
      plot_frame(shiny::imageOutput(ns(compatibility_output), height = "auto"))
    )
  )
}

#' Bind direct and temporary-file dashboard plot outputs.
#'
#' Both renderers draw a ggplot2 chart at the output's width in the browser
#' and make it `height` pixels tall, or taller when its legend or footnotes
#' need the room (see [ezdyn_ggplot_height()]).
#'
#' @param output Shiny output object for the current module.
#' @param graph Reactive function returning a ggplot-compatible plot object.
#' @param compatibility_mode Reactive function returning one logical value.
#' @param plotter Optional reactive function returning the plotting backend
#'   (`"ggrba"` or `"ggplot"`) used to build `graph()`, e.g.
#'   [dashboard_selected_plotter()]. `NULL` (default) resolves automatically.
#' @param direct_output Output identifier for the direct plot.
#' @param compatibility_output Output identifier for the temporary-file image.
#' @param height Minimum plot height in pixels; match the `height` given to
#'   [dashboard_plot_mode_ui()].
#' @param session Shiny session object for the current module.
#'
#' @return Invisibly, `NULL`.
dashboard_bind_plot_mode <- function(
    output,
    graph,
    compatibility_mode,
    plotter = NULL,
    direct_output = "plot_direct",
    compatibility_output = "plot_image",
    height = 400,
    session = shiny::getDefaultReactiveDomain()) {
  force(session)
  selected_plotter <- function() ezdyn_resolve_plotter(if (is.null(plotter)) NULL else plotter())
  # CSS width of an output as the browser reports it; 800 until it is known.
  output_width <- function(output_id) {
    width <- session$clientData[[paste0("output_", session$ns(output_id), "_width")]]
    if (is.null(width) || !is.finite(width) || width <= 0) 800 else floor(width)
  }

  # 1. Compatibility mode uses direct Shiny rendering for separate plot panels,
  # as tall as the chart needs at the output's width.
  direct_height <- shiny::reactive({
    shiny::req(compatibility_mode())
    graph_object <- graph()
    shiny::req(graph_object)
    if (!identical(selected_plotter(), "ggplot")) {
      return(height)
    }
    ceiling(72 * ezdyn_ggplot_height(graph_object, output_width(direct_output) / 72, height / 72))
  })
  output[[direct_output]] <- shiny::renderPlot({
    shiny::req(compatibility_mode())
    graph_object <- graph()
    shiny::req(graph_object)
    graph_object
  }, height = function() direct_height())

  # 2. Default mode writes a PNG drawn at the output's width.
  output[[compatibility_output]] <- shiny::renderImage({
    shiny::req(!compatibility_mode())
    graph_object <- graph()
    shiny::req(graph_object)
    if (identical(selected_plotter(), "ggplot")) {
      pixelratio <- session$clientData$pixelratio
      render_rbaplot(
        graph_object,
        plotter = "ggplot",
        width = output_width(compatibility_output),
        height = height,
        pixelratio = if (is.null(pixelratio)) 1 else pixelratio
      )
    } else {
      # A ggrba chart keeps ggsave_rba()'s own size, so it doesn't read the
      # output's width and isn't re-saved when the window is resized.
      render_rbaplot(graph_object, plotter = selected_plotter())
    }
  }, deleteFile = TRUE)

  invisible(NULL)
}
