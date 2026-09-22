testthat::test_that("Alternative Paths uses configured slider dates and preferences", {
  config <- dashboard_test_config()
  dates <- alt_paths_forecast_dates(config)
  values <- alt_paths_instrument_values(
    config$baselines$Staff,
    dates,
    config$instrument_display_name
  )
  slider_html <- htmltools::renderTags(
    cash_rate_sliders(shiny::NS("alt"), dates, values, config$slider_preferences)
  )$html

  testthat::expect_equal(dates, seq(config$timeline$forecast_start, config$timeline$forecast_end, by = "quarter"))
  testthat::expect_equal(values, c(4.2, 4.3))
  testthat::expect_match(slider_html, "alt-cash_rate_1")
  testthat::expect_match(slider_html, "alt-cash_rate_2")
  testthat::expect_match(slider_html, "alt-cash_rate_1_plus")
  testthat::expect_match(slider_html, "alt-cash_rate_2_minus")
  testthat::expect_match(slider_html, "24-Q1")
  testthat::expect_match(slider_html, "24-Q2")
  testthat::expect_equal(alt_paths_slider_label(as.Date("2026-06-01")), "26-Q2")
  testthat::expect_equal(
    alt_paths_slider_path(dates, values),
    data.frame(date = dates, Alternative = values, check.names = FALSE)
  )
})

testthat::test_that("Alternative Paths UI exposes configured controls and public ezdyn APIs", {
  config <- dashboard_test_config()

  ui_html <- htmltools::renderTags(UITab_AltPaths("alt", config))$html
  testthat::expect_match(ui_html, "alt-models")
  testthat::expect_match(ui_html, "Model\\(s\\):")
  testthat::expect_match(ui_html, "alt-baseline_name")
  testthat::expect_match(ui_html, "alt-cash_rate_sliders")
  testthat::expect_match(ui_html, "alt-open_upload")
  testthat::expect_match(ui_html, "alt-download_data")
  testthat::expect_match(ui_html, "alt-download_graph")
  testthat::expect_match(ui_html, "alt-help_anticipated")
  testthat::expect_match(ui_html, "alt-help_cognitive_discounting")
  testthat::expect_match(ui_html, "Use anticipated shocks when supported")
  testthat::expect_match(ui_html, "Cash Rate path:")
  testthat::expect_match(ui_html, "alt-lambda")
  testthat::expect_match(ui_html, "min=\"0\"")
  testthat::expect_match(ui_html, "max=\"1\"")
  testthat::expect_match(ui_html, "alt-date_range")
  testthat::expect_match(ui_html, "js-range-slider")
  testthat::expect_match(ui_html, "alt-show_table")
  testthat::expect_match(ui_html, "alt-use_compatibility_plot")
  testthat::expect_match(ui_html, "alt-plot_direct")
  testthat::expect_match(ui_html, "alt-plot_image")
  testthat::expect_match(ui_html, "padding: 0 24px;")
  testthat::expect_match(ui_html, "col-sm-8")
  testthat::expect_match(ui_html, "alt-path-plot")
  testthat::expect_match(ui_html, "alt-path-actions")
  testthat::expect_match(ui_html, "well alt-path-actions")
  testthat::expect_false(grepl("id=\"alt-show_table\"[^>]*checked", ui_html))

  long_timeline <- config$timeline
  long_timeline$data_start <- as.Date("2020-03-01")
  testthat::expect_equal(
    alt_paths_default_plot_range(long_timeline),
    c(as.Date("2022-03-01"), config$timeline$forecast_end)
  )
  testthat::expect_equal(alt_paths_plot_title("Cash Rate"), "Cash Rate — Response to Alternative Policy Path")
  testthat::expect_equal(
    alt_paths_plot_title(c("Cash Rate", "Inflation")),
    "Responses to Alternative Policy Path"
  )
  model_input_only <- dashboard_test_config()
  model_input_only$baseline_upload$allow_display_name_upload <- FALSE
  testthat::expect_no_error(UITab_AltPaths("alt", model_input_only))
})

testthat::test_that("Alternative Paths shows a configured or generic anticipated-shocks help modal", {
  config <- dashboard_test_config()

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      session$setInputs(models = "Fixture", baseline_name = "Staff", help_anticipated = 1)
      # No `help$anticipated_shocks` hook is configured, so a generic fallback
      # modal is shown rather than erroring.
      testthat::expect_no_error(session$setInputs(help_anticipated = 2))
    }
  )

  custom_modal <- function() shiny::modalDialog("Custom anticipated-shocks help")
  hooked_config <- dashboard_test_config(help = list(anticipated_shocks = custom_modal))
  testthat::expect_identical(hooked_config$help$anticipated_shocks, custom_modal)
  testthat::expect_equal(
    htmltools::renderTags(dash_help_modal(hooked_config$help$anticipated_shocks, "Anticipated shocks"))$html,
    htmltools::renderTags(custom_modal())$html
  )
  testthat::expect_match(
    htmltools::renderTags(dash_help_modal(NULL, "Anticipated shocks"))$html,
    "Anticipated shocks"
  )
})

testthat::test_that("Alternative Paths assigns unique names to uploaded baselines", {
  testthat::expect_equal(
    alt_paths_upload_name("  Downside case  ", "Staff forecasts"),
    "Downside case"
  )
  testthat::expect_error(alt_paths_upload_name("", "Staff forecasts"), "Provide a name")
  testthat::expect_error(alt_paths_upload_name("Staff forecasts", "Staff forecasts"), "already exists")
})

testthat::test_that("Alternative Paths templates preserve canonical baseline data", {
  baseline <- dashboard_test_baseline()
  output <- tempfile(fileext = ".xlsx")

  alt_paths_baseline_template(output, baseline)
  actual <- readxl::read_excel(output)
  testthat::expect_equal(names(actual), names(baseline))
  testthat::expect_equal(actual[["Cash Rate"]], baseline[["Cash Rate"]])
})

testthat::test_that("Alternative Paths validates uploaded baseline timeline coverage", {
  config <- dashboard_test_config()
  incomplete_baseline <- dashboard_test_baseline()[-1, , drop = FALSE]

  testthat::expect_error(
    dashboard_validate_baseline_coverage(incomplete_baseline, config$timeline, "Uploaded baseline"),
    "cover every quarter"
  )
})

testthat::test_that("Alternative Paths exports retain baseline history outside the plot range", {
  config <- dashboard_test_config()

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      session$setInputs(
        models = "Fixture",
        baseline_name = "Staff",
        display_names = "Cash Rate",
        use_cd = FALSE,
        lambda = 0,
        date_range = c(as.Date("2024-03-01"), as.Date("2024-06-01")),
        cash_rate_1 = 4.2,
        cash_rate_2 = 4.3
      )

      output <- export_paths()
      testthat::expect_true("2023-12-01" %in% names(output))
      testthat::expect_true(any(!is.na(output[["2023-12-01"]])))

      table <- table_paths()
      testthat::expect_false("Mnemonic" %in% names(table))
      testthat::expect_false("Baseline Name" %in% names(table))
      testthat::expect_false("2023-12-01" %in% names(table))
      testthat::expect_true(all(c("2024-03-01", "2024-06-01") %in% names(table)))
      baseline_row <- table[table[["Path Name"]] == "Baseline", , drop = FALSE]
      testthat::expect_equal(nrow(baseline_row), 1)
      testthat::expect_equal(baseline_row[["2024-03-01"]], 4.2)
      testthat::expect_equal(baseline_row[["2024-06-01"]], 4.3)
    }
  )
})

testthat::test_that("Alternative Paths honours the selected plot style", {
  config <- dashboard_test_config()

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      session$setInputs(
        models = "Fixture",
        baseline_name = "Staff",
        display_names = "Cash Rate",
        use_cd = FALSE,
        lambda = 0,
        date_range = c(as.Date("2024-03-01"), as.Date("2024-06-01")),
        cash_rate_1 = 4.2,
        cash_rate_2 = 4.3,
        plotter = "ggplot"
      )

      graph <- plotted_paths()$graph
      testthat::expect_s3_class(graph, "ggplot")
      testthat::expect_true(grepl("%", graph$labels$y, fixed = TRUE))

      if (requireNamespace("ggrba", quietly = TRUE)) {
        session$setInputs(plotter = "ggrba")
        rba_graph <- plotted_paths()$graph
        testthat::expect_s3_class(rba_graph, "ggplot")
        # The ggrba backend never overrides the y-axis label via labs(), so it
        # keeps ggplot2's raw aesthetic-derived default ("value") instead of
        # the ggplot backend's unit-annotated label.
        testthat::expect_equal(rba_graph$labels$y, "value")
      }
    }
  )
})

testthat::test_that("Alternative Paths selects a metadata-declared derived (year-ended) variable like any other", {
  config <- dashboard_test_config(include_gdp_growth = TRUE, include_derived_ye = TRUE)

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      session$setInputs(
        models = "Fixture",
        baseline_name = "Staff",
        display_names = c("Cash Rate", "GDP Growth (YE)", "Inflation (YE)"),
        use_cd = FALSE,
        lambda = 0,
        date_range = c(as.Date("2023-12-01"), as.Date("2024-06-01")),
        cash_rate_1 = 4.2,
        cash_rate_2 = 4.3
      )

      paths <- selected_paths()
      testthat::expect_setequal(
        unique(paths$display_name),
        c("Cash Rate", "GDP Growth (YE)", "Inflation (YE)")
      )
      testthat::expect_true(all(!is.na(paths$value[paths$display_name == "GDP Growth (YE)"])))
      testthat::expect_true(all(!is.na(paths$value[paths$display_name == "Inflation (YE)"])))
    }
  )
})

testthat::test_that("Alternative Paths waits for dynamic slider inputs before calculating", {
  config <- dashboard_test_config()

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      testthat::expect_error(alt_paths(), class = "shiny.silent.error")
    }
  )
})

testthat::test_that("Alternative Paths scopes response choices and results to the selected models", {
  config <- configure_dashboard(
    models = list(
      DINGO = dashboard_test_model("DINGO", include_gdp_growth = TRUE),
      MARTIN = dashboard_test_model("MARTIN")
    ),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline(include_gdp_growth = TRUE)),
    default_baseline = "Staff"
  )

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      # "GDP Growth" is only provided by DINGO, so it is hidden while an
      # incompatible model (MARTIN) is also selected...
      session$setInputs(models = c("DINGO", "MARTIN"), baseline_name = "Staff")
      testthat::expect_false("GDP Growth" %in% responses())

      # ...and offered, and calculable, once only DINGO is selected.
      session$setInputs(models = "DINGO")
      testthat::expect_true("GDP Growth" %in% responses())

      session$setInputs(
        display_names = "GDP Growth",
        use_cd = FALSE,
        lambda = 0,
        date_range = c(as.Date("2023-12-01"), as.Date("2024-06-01")),
        cash_rate_1 = 4.2,
        cash_rate_2 = 4.3
      )
      paths <- selected_paths()
      testthat::expect_setequal(unique(paths$display_name), "GDP Growth")
      testthat::expect_setequal(stats::na.omit(paths$model_name), "DINGO")
    }
  )
})

testthat::test_that("Alternative Paths requires at least one selected model", {
  config <- dashboard_test_config()

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      session$setInputs(baseline_name = "Staff")
      testthat::expect_error(calculated_paths(), "Select at least one model")
    }
  )
})

testthat::test_that("Alternative Paths shows a friendly message for a model missing the instrument", {
  incompatible_model <- dashboard_test_model("NoRate")
  incompatible_model$M_$varmeta <- incompatible_model$M_$varmeta[
    incompatible_model$M_$varmeta$dynare_name != "r_obs",
    ,
    drop = FALSE
  ]
  config <- configure_dashboard(
    models = list(Fixture = dashboard_test_model(), NoRate = incompatible_model),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff",
    alt_paths = list(models = c("Fixture", "NoRate"))
  )

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      session$setInputs(
        models = "NoRate",
        baseline_name = "Staff",
        use_cd = FALSE,
        lambda = 0,
        cash_rate_1 = 4.2,
        cash_rate_2 = 4.3
      )
      testthat::expect_error(calculated_paths(), class = "shiny.silent.error")
    }
  )
})

testthat::test_that("Alternative Paths initializes its scenario from the selected baseline", {
  staff <- dashboard_test_baseline()
  alternative_baseline <- staff
  alternative_baseline[["Cash Rate"]] <- staff[["Cash Rate"]] + 1
  config <- configure_dashboard(
    models = list(Fixture = dashboard_test_model()),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = staff, Alternative = alternative_baseline),
    default_baseline = "Staff"
  )

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      session$setInputs(
        models = "Fixture",
        baseline_name = "Alternative",
        display_names = "Cash Rate",
        use_cd = FALSE,
        lambda = 0,
        date_range = c(as.Date("2023-12-01"), as.Date("2024-06-01")),
        cash_rate_1 = 5.2,
        cash_rate_2 = 5.3
      )

      output <- export_paths()
      testthat::expect_equal(output[["2024-03-01"]], 5.2)
      testthat::expect_equal(output[["2024-06-01"]], 5.3)
    }
  )
})

testthat::test_that("Alternative Paths opens the upload modal with module-local IDs", {
  config <- dashboard_test_config()

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      session$setInputs(models = "Fixture", baseline_name = "Staff")
      testthat::expect_no_error(session$setInputs(open_upload = 1))
    }
  )

  model_input_config <- dashboard_test_config()
  model_input_config$baseline_upload$allow_display_name_upload <- FALSE
  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = model_input_config),
    {
      session$setInputs(models = "Fixture", baseline_name = "Staff")
      testthat::expect_no_error(session$setInputs(open_upload = 1))
    }
  )
})

testthat::test_that("Alternative Paths retains multiple named uploaded baselines", {
  config <- dashboard_test_config()
  workbook <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(dashboard_test_baseline(), workbook)

  shiny::testServer(
    ServerTab_AltPaths,
    args = list(id = "alt", config = config),
    {
      session$setInputs(
        models = "Fixture",
        baseline_name = "Staff",
        uploaded_baseline = list(datapath = workbook),
        upload_interpretation = "display",
        uploaded_baseline_name = "Downside",
        import_baseline = 1
      )
      testthat::expect_true("Downside" %in% names(baseline_registry()))

      session$setInputs(
        uploaded_baseline_name = "Upside",
        import_baseline = 2
      )
      testthat::expect_true(all(c("Downside", "Upside") %in% names(baseline_registry())))
    }
  )
})
