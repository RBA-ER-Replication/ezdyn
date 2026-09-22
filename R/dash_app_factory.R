# Dashboard application factory -------------------------------------------
#
# Construct a validated config with `configure_dashboard()` and pass it to
# `ezdyn_dashboard()`. This file has no source-time data loading or app launch.

# Require an already-validated configuration at the app boundary.
dashboard_validate_config <- function(config) {
  if (!inherits(config, "ezdyn_dashboard_config")) {
    stop(
      "`config` must be created by `configure_dashboard()` before building the dashboard.",
      call. = FALSE
    )
  }

  config
}

# Build the configuration-driven dashboard UI without reading model data.
dashboard_ui <- function(config) {
  shiny::fluidPage(
    cash_rate_slider_styles(),
    cash_rate_slider_dependency(),
    shiny::titlePanel(config$name),
    shiny::tabsetPanel(
      id = "active_tab",
      if (isTRUE(config$overview$enabled)) {
        shiny::tabPanel("Overview", value = "overview", UITab_Overview("overview", config$overview))
      },
      shiny::tabPanel("Alternative Policy Paths", value = "alt_paths", UITab_AltPaths("alt_paths", config)),
      shiny::tabPanel("Impulse Response Library", value = "irfs", UITab_IRF("irfs", config)),
      shiny::tabPanel("Historical Shock Decompositions", value = "hsd", UITab_HistShockDecomp("hsd", config)),
      #shiny::tabPanel("Variable Dictionary", value = "dictionary", UITab_Dictionary("dictionary", config)),
      if (isTRUE(config$optimal_policy$enabled)) {
        shiny::tabPanel("Optimal Policy", value = "optimal_policy", UITab_OptimalPolicy("optimal_policy", config))
      }
    )
  )
}

# Build the configuration-driven dashboard server without global state.
dashboard_server <- function(config) {
  force(config)

  function(input, output, session) {
    if (!is.null(config$startup_modal)) {
      shiny::showModal(config$startup_modal)
    }

    if (isTRUE(config$overview$enabled)) {
      ServerTab_Overview("overview", config$overview)
    }
    ServerTab_IRF("irfs", config)
    ServerTab_HistShockDecomp("hsd", config)
    ServerTab_AltPaths("alt_paths", config)
    #ServerTab_Dictionary("dictionary", config)
    if (isTRUE(config$optimal_policy$enabled)) {
      ServerTab_OptimalPolicy("optimal_policy", config)
    }

    invisible(config)
  }
}

#' Build an ezdyn dashboard application from validated configuration.
#'
#' This function creates and returns a Shiny application. It does not load
#' models, read workbooks, source configuration, knit documents, or call
#' [shiny::runApp()].
#'
#' @param config A validated `ezdyn_dashboard_config` from
#'   [configure_dashboard()].
#'
#' @return A `shiny.appobj`.
#'
#' @export
ezdyn_dashboard <- function(config) {
  config <- dashboard_validate_config(config)

  shiny::shinyApp(
    ui = dashboard_ui(config),
    server = dashboard_server(config)
  )
}
