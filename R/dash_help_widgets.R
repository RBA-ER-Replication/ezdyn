# Help widgets --------------------------------------------------------------
#
# Generic contextual-help UI components. Composition-specific modal content
# (e.g. DINGO's help-modal text) is supplied through dashboard configuration
# hooks (`configure_dashboard()`'s `help` and `optimal_policy$help`), not
# hardcoded here.

#' Build a popover info button.
#'
#' @param id Input identifier.
#' @param title Popover title.
#' @param msg Popover message.
#'
#' @return Shiny UI.
infoButton <- function(id, title, msg) {
  shiny::actionButton(id, bsicons::bs_icon("info-circle")) |>
    bslib::popover(msg, title = title)
}

#' Build a tooltip info icon.
#'
#' @param id Unused; retained for call-site symmetry with [infoButton()].
#' @param title Unused; retained for call-site symmetry with [infoButton()].
#' @param msg Tooltip message.
#'
#' @return Shiny UI.
infoTooltip <- function(id, title, msg) {
  bslib::tooltip(trigger = bsicons::bs_icon("info-circle"), msg)
}

#' Build a small circular help-modal trigger button.
#'
#' @param id Input identifier.
#' @param circle Whether to use a compact circular button style.
#'
#' @return Shiny UI.
help_button <- function(id, circle = TRUE) {
  if (circle) {
    shinyWidgets::circleButton(id, icon = bsicons::bs_icon("info-circle"), size = "xs")
  } else {
    shiny::actionButton(id, bsicons::bs_icon("info-circle"))
  }
}

#' Return a configured help modal, or a generic fallback modal.
#'
#' Dashboard modules call this instead of hardcoding modal content, so a
#' composition root (e.g. DINGO) can supply reviewed help text through
#' configuration hooks while the module itself stays generic.
#'
#' @param hook Optional function returning UI from [shiny::modalDialog()].
#'   `NULL` shows a generic fallback modal instead.
#' @param title Fallback modal title, used only when `hook` is `NULL`.
#'
#' @return Shiny UI returned by [shiny::modalDialog()].
dash_help_modal <- function(hook, title) {
  if (is.function(hook)) {
    return(hook())
  }

  shiny::modalDialog(
    title = title,
    easyClose = TRUE,
    shiny::p("No additional help is configured for this control.")
  )
}
