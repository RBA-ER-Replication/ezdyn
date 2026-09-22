# Cash-rate slider component -----------------------------------------------
#
# Explicit, configuration-driven slider UI for Alternative Policy Paths.

#' Return the stable input ID for one cash-rate slider.
#'
#' @param index One-based forecast-quarter index.
#'
#' @return One character string.
alt_paths_slider_id <- function(index) {
  paste0("cash_rate_", index)
}

#' Return the stable input ID for one slider increment button.
#'
#' @param index One-based forecast-quarter index.
#'
#' @return One character string.
alt_paths_slider_increment_id <- function(index) {
  paste0(alt_paths_slider_id(index), "_plus")
}

#' Return the stable input ID for one slider decrement button.
#'
#' @param index One-based forecast-quarter index.
#'
#' @return One character string.
alt_paths_slider_decrement_id <- function(index) {
  paste0(alt_paths_slider_id(index), "_minus")
}

#' Format one forecast date as a year-quarter slider label.
#'
#' @param date One quarterly `Date` value.
#'
#' @return One label such as `"26-Q2"`.
alt_paths_slider_label <- function(date) {
  quarter <- (as.integer(format(date, "%m")) - 1L) %/% 3L + 1L
  sprintf("%s-Q%s", format(date, "%y"), quarter)
}

#' Return forecast-quarter dates from a validated dashboard configuration.
#'
#' @param config Validated dashboard configuration.
#'
#' @return Date vector.
alt_paths_forecast_dates <- function(config) {
  seq(config$timeline$forecast_start, config$timeline$forecast_end, by = "quarter")
}

#' Return baseline values for the configured policy instrument.
#'
#' @param baseline Canonical `ezdyn_baseline` object.
#' @param forecast_dates Forecast-quarter dates.
#' @param instrument_display_name Canonical instrument display name.
#'
#' @return Numeric vector ordered by `forecast_dates`.
alt_paths_instrument_values <- function(baseline, forecast_dates, instrument_display_name) {
  if (!inherits(baseline, "ezdyn_baseline") ||
      !(instrument_display_name %in% names(baseline))) {
    stop(
      sprintf("Baseline must contain instrument `%s`.", instrument_display_name),
      call. = FALSE
    )
  }

  values <- baseline[[instrument_display_name]][match(forecast_dates, baseline$date)]
  if (!is.numeric(values) || length(values) != length(forecast_dates) ||
      anyNA(values) || any(!is.finite(values))) {
    stop(
      sprintf("Baseline must provide finite forecast values for instrument `%s`.", instrument_display_name),
      call. = FALSE
    )
  }

  values
}

#' Build one named alternative policy path from cash-rate slider values.
#'
#' @param dates Forecast-quarter dates.
#' @param values Numeric slider values.
#' @param path_name Name of the alternative policy path.
#'
#' @return A wide alternative-path data frame.
alt_paths_slider_path <- function(dates, values, path_name = "Alternative") {
  if (!inherits(dates, "Date") || !is.numeric(values) ||
      length(dates) != length(values) || anyNA(values) || any(!is.finite(values)) ||
      !is.character(path_name) || length(path_name) != 1 || is.na(path_name) || !nzchar(path_name)) {
    stop("Slider dates, values, and path name are invalid.", call. = FALSE)
  }

  out <- data.frame(date = dates, check.names = FALSE)
  out[[path_name]] <- values
  out
}

#' Build cash-rate sliders and increment/decrement controls.
#'
#' @param ns A Shiny namespace function.
#' @param dates Forecast-quarter dates.
#' @param values Baseline instrument values for `dates`.
#' @param slider_preferences Validated dashboard slider preferences.
#'
#' @return Shiny UI.
cash_rate_sliders <- function(ns, dates, values, slider_preferences) {
  if (!is.numeric(values) || length(dates) != length(values)) {
    stop("Slider dates and baseline values must have equal length.", call. = FALSE)
  }

  lapply(seq_along(dates), function(index) {
    shiny::tags$div(
      class = "slider-item",
      shiny::tags$div(class = "slider-label", alt_paths_slider_label(dates[[index]])),
      shiny::tags$div(
        class = "slider-controls",
        shinyWidgets::actionBttn(
          ns(alt_paths_slider_increment_id(index)),
          label = "",
          icon = shiny::icon("plus"),
          size = "xs",
          block = TRUE,
          style = "fill"
        )
      ),
      shinyWidgets::noUiSliderInput(
        ns(alt_paths_slider_id(index)),
        label = NULL,
        min = values[[1]] - slider_preferences$r_slider_range,
        max = values[[1]] + slider_preferences$r_slider_range,
        value = values[[index]],
        step = slider_preferences$r_slider_step,
        direction = "rtl",
        color = "#428bca",
        orientation = "vertical"
      ),
      shiny::tags$div(
        class = "slider-controls",
        shinyWidgets::actionBttn(
          ns(alt_paths_slider_decrement_id(index)),
          label = "",
          icon = shiny::icon("minus"),
          size = "xs",
          block = TRUE,
          style = "fill"
        )
      )
    )
  })
}

#' Preload the noUiSlider HTML dependency before any slider first renders.
#'
#' `cash_rate_sliders()` only ever runs inside a server-side `renderUI()`, so
#' its `shinyWidgets::noUiSliderInput()` HTML dependency is otherwise only
#' attached to the page the first time that output renders - injected
#' dynamically over the websocket rather than present in the initial static
#' page. If another tab's dynamically-rendered widget (e.g. `DT` in the
#' Variable Dictionary tab) triggers its own dependency injection first, the
#' browser can end up not finishing/executing noUiSlider's script before
#' Shiny binds the later-rendered sliders, leaving them invisible. Mounting
#' one hidden, unused slider in the static UI guarantees noUiSlider's
#' dependency is already present in the page's initial load, regardless of
#' which tab a user visits first.
#'
#' @return Shiny UI containing one hidden placeholder slider.
cash_rate_slider_dependency <- function() {
  shiny::tags$div(
    style = "display: none;",
    shinyWidgets::noUiSliderInput("cash_rate_slider_dependency", label = NULL, min = 0, max = 1, value = 0)
  )
}

#' Return styles for the dashboard and cash-rate slider component.
#'
#' @return HTML head tags containing dashboard CSS.
cash_rate_slider_styles <- function() {
  htmltools::tags$head(
    htmltools::tags$style(shiny::HTML("\
.slider-container {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  gap: 10px;
  width: 100%;
  overflow: hidden;
  align-items: flex-start;
  margin-bottom: 24px;
}

.slider-container > .shiny-html-output {
  display: flex;
  flex: 1 1 100%;
  min-width: 0;
  justify-content: space-between;
  gap: 10px;
  align-items: flex-start;
}

.slider-item {
  flex: 1 1 0;
  min-width: 0;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 8px;
}

.slider-label {
  font-weight: 600;
  text-align: center;
}

.slider-controls {
  display: flex;
  justify-content: right;
  align-items: right;
  gap: 6px;
  width: 100%;
}

.noui-slider,
.noUi-vertical {
  height: 300px !important;
  width: 20px !important;
  margin-bottom: 0 !important;
}

h3 {
  background-color: #d1f4ff;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
  padding: 12px;
}

h4 {
  background-color: #e3f8ff;
  box-shadow: 0 4px 5px rgba(0, 0, 0, 0.2);
  padding: 12px;
}

.irf-subsection-title {
  color: #000;
  font-size: 16px;
  font-weight: 600;
  margin: 0 0 8px;
}

.shiny-image-output img {
  display: block;
  max-width: 100%;
  height: auto;
}

.shiny-image-output {
  margin-bottom: 16px;
}

.alt-path-plot {
  height: 400px;
  margin-bottom: 32px;
  overflow: hidden;
}

.alt-path-plot .shiny-image-output,
.alt-path-plot .shiny-plot-output {
  height: 400px !important;
}

.alt-path-plot .shiny-image-output img {
  width: 100%;
  height: 100%;
  max-height: 400px;
  object-fit: contain;
  object-position: top left;
}

.alt-path-actions {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 8px;
  margin: 0 0 16px;
  clear: both;
}
"))
  )
}
