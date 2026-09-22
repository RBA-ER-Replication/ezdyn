testthat::test_that("Optimal Policy builds multiple configured bounds", {
  input <- list(
    elb = TRUE,
    elb_level = 0.1,
    loss_horizon = 4L,
    instrument_horizon = 4L,
    constraint_r_obs_lower_enabled = TRUE,
    constraint_r_obs_lower_start = "1",
    constraint_r_obs_lower_end = "2",
    constraint_r_obs_lower_value = 0.25,
    constraint_infl_obs_ye_upper_enabled = TRUE,
    constraint_infl_obs_ye_upper_start = "2",
    constraint_infl_obs_ye_upper_end = "4",
    constraint_infl_obs_ye_upper_value = 3,
    constraint_unemp_lower_enabled = FALSE,
    constraint_unemp_upper_enabled = FALSE,
    constraint_r_obs_upper_enabled = FALSE,
    constraint_dr_lower_enabled = FALSE,
    constraint_dr_upper_enabled = FALSE
  )
  config <- list(optimal_policy = list(
    elb_variable = "r_obs",
    constraint_variable_choices = c(
      "Inflation (year-ended)" = "infl_obs_ye", "Unemployment" = "unemp",
      "Cash rate" = "r_obs", "Cash rate changes" = "dr"
    )
  ))

  actual <- optimal_policy_constraints(input, config)
  cash_rate_lower <- Filter(function(constraint) constraint$variable == "r_obs" && constraint$direction == "lower" && !isTRUE(constraint$historical), actual)[[1]]
  inflation_upper <- Filter(function(constraint) constraint$variable == "infl_obs_ye" && constraint$direction == "upper", actual)[[1]]

  testthat::expect_length(actual, 3)
  testthat::expect_equal(cash_rate_lower$period, 1:2)
  testthat::expect_equal(inflation_upper$period, 2:4)
  testthat::expect_equal(inflation_upper$direction, "upper")
})

testthat::test_that("Optimal Policy widget ids stay valid when choice values are display names", {
  testthat::expect_equal(optimal_policy_widget_id("Inflation (year-ended)"), "Inflation_year_ended_")
  testthat::expect_equal(optimal_policy_constraint_id("Cash Rate", "lower", "enabled"), "constraint_Cash_Rate_lower_enabled")

  input <- list(
    elb = FALSE,
    loss_horizon = 4L,
    instrument_horizon = 4L,
    constraint_Cash_Rate_lower_enabled = TRUE,
    constraint_Cash_Rate_lower_start = "1",
    constraint_Cash_Rate_lower_end = "2",
    constraint_Cash_Rate_lower_value = 0.1,
    constraint_Cash_Rate_upper_enabled = FALSE
  )
  config <- list(optimal_policy = list(
    constraint_variable_choices = c("Cash rate" = "Cash Rate")
  ))

  actual <- optimal_policy_constraints(input, config)

  testthat::expect_length(actual, 1)
  testthat::expect_equal(actual[[1]]$variable, "Cash Rate")
  testthat::expect_equal(actual[[1]]$period, 1:2)
})

testthat::test_that("Optimal Policy rejects bounds outside its loss horizon", {
  input <- list(
    elb = FALSE,
    loss_horizon = 2L,
    instrument_horizon = 2L,
    constraint_r_obs_lower_enabled = TRUE,
    constraint_r_obs_lower_start = "1",
    constraint_r_obs_lower_end = "3",
    constraint_r_obs_lower_value = 0,
    constraint_infl_obs_ye_lower_enabled = FALSE,
    constraint_infl_obs_ye_upper_enabled = FALSE,
    constraint_unemp_lower_enabled = FALSE,
    constraint_unemp_upper_enabled = FALSE,
    constraint_r_obs_upper_enabled = FALSE,
    constraint_dr_lower_enabled = FALSE,
    constraint_dr_upper_enabled = FALSE
  )
  config <- list(optimal_policy = list(
    elb_variable = "r_obs",
    constraint_variable_choices = c(
      "Inflation (year-ended)" = "infl_obs_ye", "Unemployment" = "unemp",
      "Cash rate" = "r_obs", "Cash rate changes" = "dr"
    )
  ))

  testthat::expect_error(optimal_policy_constraints(input, config), "within the loss evaluation horizon")
})

testthat::test_that("Optimal Policy resolves selected responses to display names", {
  model <- list(M_ = list(varmeta = data.frame(
    dynare_name = c("infl_obs", "r_obs"),
    display_name = c("Trimmed Mean Inflation", "Cash Rate")
  )))

  actual <- optimal_policy_response_display_names(model, c("r_obs", "infl_obs"))

  testthat::expect_equal(actual, c("Cash Rate", "Trimmed Mean Inflation"))
})

testthat::test_that("Optimal Policy maps the Commitment control to timeless commitment", {
  input <- list(
    strategy_modes = c("timeless", "disc"),
    loss_horizon = 4L,
    instrument_horizon = 4L,
    response_variables = "r_obs",
    loss_variables = "infl_obs",
    weight_infl_obs = 1,
    discount_factor = 0.99
  )
  config <- list(optimal_policy = list(instrument_variable = "r_obs", instrument_shock = "eps_r"))

  actual <- optimal_policy_strategies(input, list(), config)

  testthat::expect_identical(vapply(actual, `[[`, character(1), "commit"), c("timeless", "disc"))
  testthat::expect_identical(vapply(actual, `[[`, character(1), "output_name"), c("Optimal (DINGO, commitment)", "Optimal (DINGO, discretion)"))
})

testthat::test_that("Optimal Policy uses the shared two-year plot-range convention", {
  timeline <- list(data_start = as.Date("2024-03-01"), forecast_start = as.Date("2026-09-01"))

  actual <- optimal_policy_default_plot_range(timeline, as.Date("2028-12-01"))

  testthat::expect_equal(actual, as.Date(c("2024-09-01", "2028-12-01")))
})

testthat::test_that("Optimal Policy loss-variable inputs use configured target/weight defaults", {
  config <- list(optimal_policy = list(
    loss_variable_choices = c("Inflation" = "infl_obs", "Cash rate changes" = "dr"),
    loss_variable_defaults = c(infl_obs = 0.625),
    loss_variable_weight_defaults = c(dr = 0.5)
  ))

  html <- htmltools::renderTags(shiny::tagList(optimal_policy_loss_inputs(shiny::NS("policy"), config)))$html

  testthat::expect_match(html, "value=\"0.625\"")
  testthat::expect_match(html, "value=\"0.5\"")

  no_defaults_config <- list(optimal_policy = list(
    loss_variable_choices = c("Inflation" = "infl_obs"),
    loss_variable_defaults = stats::setNames(numeric(0), character(0)),
    loss_variable_weight_defaults = stats::setNames(numeric(0), character(0))
  ))
  fallback_html <- htmltools::renderTags(shiny::tagList(
    optimal_policy_loss_inputs(shiny::NS("policy"), no_defaults_config)
  ))$html
  testthat::expect_match(fallback_html, "value=\"0\"")
  testthat::expect_match(fallback_html, "value=\"1\"")
})

testthat::test_that("Optimal Policy weight/target ids and lookups agree when choice values are display names", {
  config <- list(optimal_policy = list(
    loss_variable_choices = c("Inflation (year-ended)" = "Inflation (year-ended)"),
    loss_variable_defaults = stats::setNames(numeric(0), character(0)),
    loss_variable_weight_defaults = stats::setNames(numeric(0), character(0))
  ))

  html <- htmltools::renderTags(shiny::tagList(optimal_policy_loss_inputs(shiny::NS("policy"), config)))$html
  expected_id <- paste0("policy-weight_", optimal_policy_widget_id("Inflation (year-ended)"))

  testthat::expect_match(html, expected_id, fixed = TRUE)

  input <- list(
    strategy_modes = "disc", loss_horizon = 4L, instrument_horizon = 4L,
    response_variables = "Cash Rate", loss_variables = "Inflation (year-ended)",
    discount_factor = 0.99
  )
  input[[paste0("weight_", optimal_policy_widget_id("Inflation (year-ended)"))]] <- 2
  strategy_config <- list(optimal_policy = list(instrument_variable = "Cash Rate", instrument_shock = "Monetary policy shock"))

  strategies <- optimal_policy_strategies(input, list(), strategy_config)

  testthat::expect_equal(strategies[[1]]$loss_weights[["Inflation (year-ended)"]], 2)
})

testthat::test_that("Optimal Policy UI uses configured description and a button-triggered loading indicator", {
  model <- list(M_ = list(varmeta = data.frame(
    dynare_name = c("r_obs", "infl_obs"),
    display_name = c("Cash Rate", "Inflation")
  )))
  config <- list(
    models = list(DINGO = model),
    timeline = list(data_start = as.Date("2024-03-01"), forecast_start = as.Date("2026-09-01")),
    baselines = list(Staff = "placeholder"),
    default_baseline = "Staff",
    optimal_policy = list(
      policy_model = "DINGO", forecast_start = as.Date("2026-09-01"),
      forecast_end = as.Date("2028-12-01"), preloss_start = as.Date("2024-03-01"), lambda = 0.8,
      loss_variable_choices = c("Inflation (year-ended)" = "infl_obs_ye", "Cash rate changes" = "dr"),
      constraint_variable_choices = c("Inflation (year-ended)" = "infl_obs_ye", "Cash rate" = "r_obs"),
      default_response_variables = "r_obs",
      default_loss_variables = "infl_obs_ye",
      loss_variable_defaults = stats::setNames(numeric(0), character(0)),
      loss_variable_weight_defaults = stats::setNames(numeric(0), character(0)),
      description_ui = NULL,
      help = list()
    )
  )

  html <- as.character(UITab_OptimalPolicy("policy", config))

  testthat::expect_match(html, "Generate graph")
  testthat::expect_match(html, "Generating optimal policy graph")
  testthat::expect_match(html, "alt-path-plot optimal-policy-plot")
  testthat::expect_match(html, "well alt-path-actions")
  testthat::expect_match(html, "This tab lets you generate optimal policy paths under a range of assumptions")
  testthat::expect_match(html, "Loss function variables", fixed = TRUE)
  testthat::expect_match(html, "policy-help_strategy")
  testthat::expect_match(html, "policy-help_loss_variables")
  testthat::expect_match(html, "policy-help_loss_horizon")
  testthat::expect_match(html, "policy-help_instrument_horizon")
  testthat::expect_match(html, "policy-help_constraints")
  testthat::expect_match(html, "policy-help_commitment_start")
  testthat::expect_match(html, "policy-baseline_name")
  testthat::expect_match(html, "policy-active_baseline")
  testthat::expect_match(html, "policy-open_upload")

  config$optimal_policy$description_ui <- function() shiny::p("Fixture policy description")
  hooked_html <- as.character(UITab_OptimalPolicy("policy", config))
  testthat::expect_match(hooked_html, "Fixture policy description")
  testthat::expect_false(grepl("This tab lets you generate optimal policy paths", hooked_html, fixed = TRUE))
})
