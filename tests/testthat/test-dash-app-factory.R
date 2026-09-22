testthat::test_that("ezdyn_dashboard builds a validated app without launching", {
  config <- configure_dashboard(
    models = list(Fixture = dashboard_test_model()),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff",
    startup_modal = shiny::modalDialog("Fixture startup message")
  )
  app <- ezdyn_dashboard(config)

  testthat::expect_s3_class(app, "shiny.appobj")
  testthat::expect_true(is.function(dashboard_server(config)))
  app_html <- htmltools::renderTags(dashboard_ui(config))$html
  testthat::expect_false(grepl("overview-content", app_html, fixed = TRUE))
  testthat::expect_match(app_html, "alt_paths-baseline_name")
  testthat::expect_match(app_html, "hsd-model")
  # The Variable Dictionary tab is temporarily disabled in dash_app_factory.R
  # (its tabPanel()/ServerTab_Dictionary() calls are commented out); its
  # module code and dedicated tests still exist for a future re-enable.
  testthat::expect_false(grepl("dictionary-models", app_html, fixed = TRUE))
  testthat::expect_false(grepl("Variable Dictionary", app_html, fixed = TRUE))
  testthat::expect_false(grepl("Optimal Policy", app_html, fixed = TRUE))

  testthat::expect_error(ezdyn_dashboard(list(not = "a config")), "configure_dashboard")
})

testthat::test_that("Overview tab is included only when a custom overview is configured", {
  config <- configure_dashboard(
    models = list(Fixture = dashboard_test_model()),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff",
    overview = list(ui = function() shiny::p("Fixture overview"))
  )

  app_html <- htmltools::renderTags(dashboard_ui(config))$html
  testthat::expect_match(app_html, "overview-content")
  testthat::expect_match(app_html, "Overview")
})

testthat::test_that("Optimal Policy tab is included only when configured", {
  config <- configure_dashboard(
    models = list(Fixture = dashboard_test_model()),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff",
    optimal_policy = list(
      policy_model = "Fixture",
      forecast_end = as.Date("2024-06-01"),
      preloss_start = as.Date("2023-12-01"),
      loss_variable_choices = c("Inflation (year-ended)" = "infl_obs_ye", "Cash rate changes" = "dr"),
      constraint_variable_choices = c("Inflation (year-ended)" = "infl_obs_ye", "Cash rate" = "r_obs"),
      default_response_variables = "r_obs",
      default_loss_variables = "infl_obs_ye"
    )
  )

  html <- htmltools::renderTags(dashboard_ui(config))$html
  testthat::expect_match(html, "Optimal Policy")
  testthat::expect_match(html, "optimal_policy-response_variables")
  testthat::expect_match(html, "optimal_policy-preloss_start")
  testthat::expect_match(html, "optimal_policy-constraint_r_obs_lower_enabled")
  testthat::expect_match(html, "optimal_policy-constraint_infl_obs_ye_upper_enabled")
  if (requireNamespace("ggrba", quietly = TRUE)) {
    testthat::expect_match(html, "optimal_policy-plotter")
  }
})

testthat::test_that("cash-rate slider styles are pure HTML tags with the legacy selectors", {
  styles <- cash_rate_slider_styles()
  style_text <- htmltools::renderTags(styles)$head

  testthat::expect_s3_class(styles, "shiny.tag")
  testthat::expect_match(style_text, "\\.slider-container")
  testthat::expect_match(style_text, "\\.slider-container > \\.shiny-html-output")
  testthat::expect_match(style_text, "\\.alt-path-plot")
  testthat::expect_match(style_text, "height: 400px")
  testthat::expect_match(style_text, "object-fit: contain")
  testthat::expect_match(style_text, "\\.noui-slider")
  testthat::expect_match(style_text, "\\.noUi-vertical")
})

testthat::test_that("Overview and HSD tab modules use namespaced configuration", {
  overview_ui <- UITab_Overview("overview", list(html_path = NULL, ui = NULL))
  overview_html <- htmltools::renderTags(overview_ui)$html
  testthat::expect_match(overview_html, "overview-content")

  testthat::expect_no_error(shiny::testServer(
    ServerTab_Overview,
    args = list(
      id = "overview",
      overview_config = list(html_path = "missing-overview.html", ui = NULL)
    ),
    {
      testthat::expect_match(paste(output$content, collapse = ""), "Overview content is not available")
    }
  ))

  overview_path <- tempfile(fileext = ".html")
  writeLines(c("<!DOCTYPE html>", "<html><body><p>Configured overview</p></body></html>"), overview_path)
  testthat::expect_no_error(shiny::testServer(
    ServerTab_Overview,
    args = list(id = "overview", overview_config = list(html_path = overview_path, ui = NULL)),
    {
      testthat::expect_match(paste(output$content, collapse = ""), "Configured overview")
      testthat::expect_match(paste(output$content, collapse = ""), "iframe")
    }
  ))

  config <- dashboard_test_config(hsd = list(models = "Fixture", display_names = "Cash Rate"))
  hsd_ui <- UITab_HistShockDecomp("hsd", config)
  hsd_html <- htmltools::renderTags(hsd_ui)$html
  testthat::expect_match(hsd_html, "hsd-model")
  testthat::expect_match(hsd_html, "hsd-display_names")
  testthat::expect_match(hsd_html, "Cash Rate")
  testthat::expect_match(hsd_html, "hsd-interactive")
  testthat::expect_match(hsd_html, "Interactive chart \\(slower\\)")
  testthat::expect_false(grepl('id="hsd-interactive"[^>]*checked', hsd_html))
  testthat::expect_equal(hsd_selected_values("Cash Rate", c("Cash Rate", "GDP Growth")), "Cash Rate")
  testthat::expect_equal(unname(hsd_selected_values("Missing", c("Cash Rate", "GDP Growth"))), "Cash Rate")

  preamble_config <- dashboard_test_config(hsd = list(
    models = "Fixture",
    preamble_ui = function() shiny::p("Fixture HSD preamble")
  ))
  preamble_html <- htmltools::renderTags(UITab_HistShockDecomp("hsd", preamble_config))$html
  testthat::expect_match(preamble_html, "Fixture HSD preamble")
  no_preamble_html <- htmltools::renderTags(UITab_HistShockDecomp("hsd", dashboard_test_config()))$html
  testthat::expect_false(grepl("Fixture HSD preamble", no_preamble_html, fixed = TRUE))
})
