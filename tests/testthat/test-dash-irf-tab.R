testthat::test_that("Scenario Mode filters configured pretty IRFs without solver calls", {
  scenarios <- rbind(
    dashboard_test_scenarios(),
    transform(
      dashboard_test_scenarios(),
      scenario = "Other Scenario",
      shock = "Other shock",
      display_name = "Other response"
    )
  )
  selected <- irf_filter_scenarios(
    scenarios,
    models = "Fixture",
    scenario_names = "Test Scenario",
    shocks = "Test shock",
    display_names = "Cash Rate"
  )

  testthat::expect_equal(nrow(selected), 2)
  testthat::expect_true(all(selected$scenario == "Test Scenario"))
  testthat::expect_equal(irf_scenario_footnote(selected), "Fixture scenario.")
  partial_message <- irf_scenario_message(
    list(DINGO = dashboard_test_model("DINGO"), MARTIN = dashboard_test_model("MARTIN")),
    transform(selected, model_name = "DINGO")
  )
  testthat::expect_match(partial_message, "MARTIN")
  testthat::expect_match(partial_message, "DINGO")
})

testthat::test_that("Scenario Mode plots available models and reports unavailable selected models", {
  scenarios <- transform(dashboard_test_scenarios(), model_name = "DINGO")
  config <- configure_dashboard(
    models = list(DINGO = dashboard_test_model("DINGO"), MARTIN = dashboard_test_model("MARTIN")),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff",
    irf = list(
      scenarios = scenarios,
      defaults = list(models = c("DINGO", "MARTIN"), display_names = "Cash Rate")
    )
  )

  testthat::expect_no_error(shiny::testServer(
    ServerTab_IRF,
    args = list(id = "irfs", config = config),
    {
      session$setInputs(
        mode = "scenario",
        models = c("DINGO", "MARTIN"),
        scenario = "Test Scenario",
        scenario_shocks = "Test shock",
        scenario_display_names = "Cash Rate"
      )
      result <- mode_result()
      testthat::expect_identical(unique(result$data$model_name), "DINGO")
      testthat::expect_match(result$message, "MARTIN")
    }
  ))
})

testthat::test_that("IRF tab honours the selected plot style and mirrors the footnote as a caption in ggplot mode", {
  # dashboard_test_scenarios() has no `unit_symbol_irf`, so add one locally
  # rather than changing the shared fixture used by other scenario tests.
  scenarios <- transform(dashboard_test_scenarios(), unit_symbol_irf = "ppt")
  config <- dashboard_test_config(
    irf = list(
      scenarios = scenarios,
      defaults = list(models = "Fixture")
    )
  )

  shiny::testServer(
    ServerTab_IRF,
    args = list(id = "irfs", config = config),
    {
      session$setInputs(
        mode = "scenario",
        models = "Fixture",
        scenario = "Test Scenario",
        scenario_shocks = "Test shock",
        scenario_display_names = "Cash Rate",
        plotter = "ggplot"
      )
      out <- plot()
      testthat::expect_s3_class(out$graph, "ggplot")
      testthat::expect_equal(out$graph$labels$caption, "Fixture scenario.")

      if (requireNamespace("ggrba", quietly = TRUE)) {
        session$setInputs(plotter = "ggrba")
        out_rba <- plot()
        testthat::expect_s3_class(out_rba$graph, "ggplot")
        testthat::expect_match(out_rba$graph$labels$caption, "Fixture scenario.", fixed = TRUE)
      }
    }
  )
})

testthat::test_that("IRF UI exposes all three namespaced modes", {
  scenarios <- rbind(
    dashboard_test_scenarios(),
    transform(
      dashboard_test_scenarios(),
      scenario = "Other Scenario",
      shock = "Other shock",
      display_name = "Other response"
    )
  )
  config <- dashboard_test_config(
    irf = list(
      scenarios = scenarios,
      defaults = list(models = "Fixture")
    )
  )

  ui_html <- htmltools::renderTags(UITab_IRF("irfs", config))$html
  testthat::expect_match(ui_html, "irfs-models")
  testthat::expect_match(ui_html, "irfs-mode")
  testthat::expect_match(ui_html, "irfs-scenario")
  testthat::expect_match(ui_html, "irfs-scenario_shocks")
  testthat::expect_match(ui_html, "irfs-scenario_display_names")
  testthat::expect_match(ui_html, "irfs-target_display_name")
  testthat::expect_match(ui_html, "irfs-target_shock")
  testthat::expect_match(ui_html, "irfs-direct_shocks")
  testthat::expect_match(ui_html, "irfs-show_plot_advanced_options")
  testthat::expect_match(ui_html, "irfs-use_compatibility_plot")
  testthat::expect_match(ui_html, "irfs-custom_title")
  testthat::expect_match(ui_html, "irfs-target_use_cd")
  testthat::expect_match(ui_html, "irfs-target_lambda")
  testthat::expect_match(ui_html, "irfs-plot_direct")
  testthat::expect_match(ui_html, "irfs-plot_image")
  testthat::expect_match(ui_html, "Mode:")
  testthat::expect_true(grepl("Default (Pre-prepared scenarios)", ui_html, fixed = TRUE))
  testthat::expect_true(grepl("Custom IRF (raw shocks)", ui_html, fixed = TRUE))
  testthat::expect_match(ui_html, "Response variables to plot:")
  testthat::expect_match(ui_html, "Shock and target")
  testthat::expect_match(ui_html, "irf-subsection-title")
  testthat::expect_false(grepl("<h4>Shock and target</h4>", ui_html, fixed = TRUE))
  testthat::expect_true(grepl(
    "Shock size (impact on first-quarter of target variable):",
    ui_html,
    fixed = TRUE
  ))
  testthat::expect_false(grepl("irfs-target_scale_targets", ui_html))
  testthat::expect_match(ui_html, "Plot compatibility mode")
  testthat::expect_false(grepl("id=\"irfs-use_compatibility_plot\"[^>]*checked", ui_html))
  testthat::expect_match(ui_html, "Downloads")
  testthat::expect_match(ui_html, "Test Scenario")
  testthat::expect_match(ui_html, "Other Scenario")
})

testthat::test_that("Custom graph titles override the generated IRF title", {
  testthat::expect_equal(
    irf_title_or_default("Policy alternative", "Generated title"),
    "Policy alternative"
  )
  testthat::expect_equal(irf_title_or_default("   ", "Generated title"), "Generated title")
})

testthat::test_that("IRF defaults select Inflation and group shocks by display name", {
  config <- dashboard_test_config(irf = list(defaults = list(display_names = "Inflation")))
  model <- dashboard_test_model()

  testthat::expect_equal(config$irf$defaults$display_names, "Inflation")
  testthat::expect_equal(
    irf_preserve_selection(
      config$irf$defaults$display_names,
      irf_common_choices(list(Fixture = model), irf_model_display_names)
    ),
    "Inflation"
  )
  selector <- irf_shock_selector(list(Fixture = model))
  testthat::expect_equal(
    selector$choices[["Fixture"]],
    stats::setNames("model::Fixture::eps_r", "Monetary Policy Shock")
  )
  testthat::expect_equal(irf_default_shock_selection(selector), "model::Fixture::eps_r")
})

testthat::test_that("IRF response choices group named variables separately from raw variable codes", {
  model <- dashboard_test_model()
  model$oo_$irf <- rbind(
    model$oo_$irf,
    data.frame(t = 1:4, shock = "eps_r", resp_var = "y_va", value = 0.1 / (1:4))
  )

  response_choices <- irf_response_choices(list(Fixture = model))
  testthat::expect_equal(
    response_choices$choices[["Named variables"]],
    stats::setNames(c("Cash Rate", "Inflation"), c("Cash Rate", "Inflation"))
  )
  testthat::expect_equal(response_choices$choices[["Variable codes"]], stats::setNames("y_va", "y_va"))
  testthat::expect_setequal(response_choices$values, c("Cash Rate", "Inflation", "y_va"))

  scenario_data <- rbind(
    dashboard_test_scenarios(),
    transform(dashboard_test_scenarios(), dynare_name = "y_va", display_name = "y_va")
  )
  scenario_choices <- irf_scenario_response_choices(scenario_data)
  testthat::expect_equal(scenario_choices$choices[["Named variables"]], stats::setNames("Cash Rate", "Cash Rate"))
  testthat::expect_equal(scenario_choices$choices[["Variable codes"]], stats::setNames("y_va", "y_va"))
  testthat::expect_length(irf_scenario_response_choices(data.frame())$values, 0)
})

testthat::test_that("IRF shock selector matches by display name and resolves native shocks per model", {
  dingo <- dashboard_test_model("DINGO", shock_name = "eps_dingo", shock_description = "monetary policy shock")
  martin <- dashboard_test_model("MARTIN", shock_name = "eps_martin", shock_description = "monetary policy shock")
  other <- dashboard_test_model("Other", shock_name = "eps_other", shock_description = "demand shock")
  selector <- irf_shock_selector(list(DINGO = dingo, MARTIN = martin, Other = other))

  testthat::expect_false("Common shocks" %in% names(selector$choices))
  testthat::expect_equal(
    selector$choices[["DINGO"]],
    stats::setNames("model::DINGO::eps_dingo", "Monetary Policy Shock")
  )
  testthat::expect_equal(
    selector$choices[["MARTIN"]],
    stats::setNames("model::MARTIN::eps_martin", "Monetary Policy Shock")
  )

  common_pair <- list(
    DINGO = dashboard_test_model("DINGO", shock_name = "eps_r", shock_description = "monetary policy shock"),
    MARTIN = dashboard_test_model("MARTIN", shock_name = "eps_r", shock_description = "monetary policy shock")
  )
  common_selector <- irf_shock_selector(common_pair)
  testthat::expect_true("Common shocks" %in% names(common_selector$choices))
  resolved <- irf_resolve_model_shocks(common_selector, irf_default_shock_selection(common_selector))
  testthat::expect_true(resolved$valid)
  testthat::expect_equal(unname(resolved$shocks), c("eps_r", "eps_r"))
})
