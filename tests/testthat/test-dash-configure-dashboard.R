testthat::test_that("configure_dashboard returns a validated explicit configuration", {
  model <- dashboard_test_model()
  config <- configure_dashboard(
    name = "Fixture dashboard",
    models = list(Fixture = model),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff",
    irf = list(scenarios = dashboard_test_scenarios())
  )

  testthat::expect_s3_class(config, "ezdyn_dashboard_config")
  testthat::expect_equal(config$slider_preferences$r_slider_range, 3)
  testthat::expect_equal(config$slider_preferences$r_slider_step, 0.005)
  testthat::expect_equal(config$slider_preferences$cr_button_step, 0.25)
  testthat::expect_equal(config$baseline_upload$default_source_model, "Fixture")
  testthat::expect_true(config$baseline_upload$allow_display_name_upload)
})

testthat::test_that("configure_dashboard rejects invalid slider and scenario contracts", {
  model <- dashboard_test_model()
  arguments <- list(
    models = list(Fixture = model),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )

  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(r_slider_step = 0))),
    "r_slider_step"
  )
  testthat::expect_error(
    do.call(
      configure_dashboard,
      c(arguments, list(irf = list(scenarios = data.frame(t = 1))))
    ),
    "irf\\$scenarios"
  )
  invalid_scenario <- dashboard_test_scenarios()
  invalid_scenario$scenario <- NULL
  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(irf = list(scenarios = invalid_scenario)))),
    "irf\\$scenarios"
  )
})

testthat::test_that("configure_dashboard trusts MOO and baseline boundary classes", {
  model <- dashboard_test_model()
  baseline <- structure(list(imported = TRUE), class = "ezdyn_baseline")

  testthat::expect_silent(
    configure_dashboard(
      models = list(Fixture = model),
      timeline = dashboard_test_timeline(),
      baselines = list(Staff = baseline),
      default_baseline = "Staff"
    )
  )

  testthat::expect_error(
    configure_dashboard(
      models = list(Fixture = unclass(model)),
      timeline = dashboard_test_timeline(),
      baselines = list(Staff = dashboard_test_baseline()),
      default_baseline = "Staff"
    ),
    "returned by ezdyn"
  )
})

testthat::test_that("configure_dashboard accepts a custom startup modal", {
  startup_modal <- shiny::modalDialog("Read this before continuing")
  arguments <- list(
    models = list(Fixture = dashboard_test_model()),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )

  config <- do.call(configure_dashboard, c(arguments, list(startup_modal = startup_modal)))
  testthat::expect_identical(config$startup_modal, startup_modal)
  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(startup_modal = "not modal UI"))),
    "startup_modal"
  )
})

testthat::test_that("configure_dashboard requires at least one enabled HSD model", {
  arguments <- list(
    models = list(Fixture = dashboard_test_model()),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )

  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(hsd = list(models = character())))),
    "enabled-model"
  )
})

testthat::test_that("hsd$models silently drops models with no historical shock decomposition data", {
  no_hsd_model <- dashboard_test_model(label = "NoHSD")
  no_hsd_model$oo_$SmoothedShocks <- NULL
  no_hsd_model$oo_$SmoothedVariables <- NULL
  arguments <- list(
    models = list(Fixture = dashboard_test_model(), NoHSD = no_hsd_model),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )

  # Default `hsd$models` (all registered models) narrows to only the eligible one.
  config <- do.call(configure_dashboard, arguments)
  testthat::expect_equal(config$hsd$models, "Fixture")

  # Explicitly requesting both narrows the same way, with no error.
  config <- do.call(configure_dashboard, c(arguments, list(hsd = list(models = c("Fixture", "NoHSD")))))
  testthat::expect_equal(config$hsd$models, "Fixture")

  # Requesting only the ineligible model leaves nothing to show, so it errors clearly.
  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(hsd = list(models = "NoHSD")))),
    "historical shock decomposition data"
  )
})

testthat::test_that("configure_dashboard accepts an hsd$preamble_ui hook", {
  arguments <- list(
    models = list(Fixture = dashboard_test_model()),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )
  preamble_ui <- function() shiny::p("Fixture preamble")

  config <- do.call(configure_dashboard, c(arguments, list(hsd = list(preamble_ui = preamble_ui))))
  testthat::expect_identical(config$hsd$preamble_ui, preamble_ui)
  testthat::expect_null(dashboard_test_config()$hsd$preamble_ui)
  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(hsd = list(preamble_ui = "not a function")))),
    "hsd"
  )
})

testthat::test_that("configure_dashboard validates Alternative Paths model selection", {
  arguments <- list(
    models = list(DINGO = dashboard_test_model("DINGO"), MARTIN = dashboard_test_model("MARTIN")),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )

  config <- do.call(configure_dashboard, arguments)
  testthat::expect_equal(config$alt_paths$models, c("DINGO", "MARTIN"))
  testthat::expect_equal(config$alt_paths$defaults$models, c("DINGO", "MARTIN"))

  config <- do.call(configure_dashboard, c(arguments, list(alt_paths = list(models = "DINGO"))))
  testthat::expect_equal(config$alt_paths$models, "DINGO")
  testthat::expect_equal(config$alt_paths$defaults$models, "DINGO")

  config <- do.call(
    configure_dashboard,
    c(arguments, list(alt_paths = list(defaults = list(models = "MARTIN"))))
  )
  testthat::expect_equal(config$alt_paths$models, c("DINGO", "MARTIN"))
  testthat::expect_equal(config$alt_paths$defaults$models, "MARTIN")

  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(alt_paths = list(models = "Unknown")))),
    "alt_paths\\$models"
  )
  testthat::expect_error(
    do.call(
      configure_dashboard,
      c(arguments, list(alt_paths = list(models = "DINGO", defaults = list(models = "MARTIN"))))
    ),
    "alt_paths\\$defaults\\$models"
  )
})

testthat::test_that("configure_dashboard validates Variable Dictionary model selection", {
  arguments <- list(
    models = list(DINGO = dashboard_test_model("DINGO"), MARTIN = dashboard_test_model("MARTIN")),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )

  config <- do.call(configure_dashboard, arguments)
  testthat::expect_equal(config$dictionary$models, c("DINGO", "MARTIN"))
  testthat::expect_equal(config$dictionary$defaults$models, c("DINGO", "MARTIN"))

  config <- do.call(configure_dashboard, c(arguments, list(dictionary = list(models = "DINGO"))))
  testthat::expect_equal(config$dictionary$models, "DINGO")
  testthat::expect_equal(config$dictionary$defaults$models, "DINGO")

  config <- do.call(
    configure_dashboard,
    c(arguments, list(dictionary = list(defaults = list(models = "MARTIN"))))
  )
  testthat::expect_equal(config$dictionary$models, c("DINGO", "MARTIN"))
  testthat::expect_equal(config$dictionary$defaults$models, "MARTIN")

  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(dictionary = list(models = "Unknown")))),
    "dictionary\\$models"
  )
  testthat::expect_error(
    do.call(
      configure_dashboard,
      c(arguments, list(dictionary = list(models = "DINGO", defaults = list(models = "MARTIN"))))
    ),
    "dictionary\\$defaults\\$models"
  )
})

testthat::test_that("configure_dashboard accepts a top-level help$anticipated_shocks hook", {
  arguments <- list(
    models = list(Fixture = dashboard_test_model()),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )
  anticipated_shocks <- function() shiny::modalDialog("Fixture help")

  config <- do.call(configure_dashboard, c(arguments, list(help = list(anticipated_shocks = anticipated_shocks))))
  testthat::expect_identical(config$help$anticipated_shocks, anticipated_shocks)
  testthat::expect_null(dashboard_test_config()$help$anticipated_shocks)
  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(help = list(unsupported_key = identity)))),
    "Help hooks"
  )
})

testthat::test_that("configure_dashboard validates explicit optimal-policy settings", {
  model <- dashboard_test_model()
  arguments <- list(
    models = list(DINGO = model, MARTIN = model),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )
  policy_config <- list(
    policy_model = "DINGO",
    comparison_model = "MARTIN",
    forecast_end = as.Date("2024-12-01"),
    preloss_start = as.Date("2023-12-01"),
    instrument_variable = "r_obs",
    instrument_shock = "eps_r",
    use_cd = TRUE,
    lambda = 0.8,
    loss_variable_choices = c("Inflation" = "infl_obs"),
    constraint_variable_choices = c("Cash Rate" = "r_obs")
  )

  config <- do.call(configure_dashboard, c(arguments, list(optimal_policy = policy_config)))
  testthat::expect_true(config$optimal_policy$enabled)
  testthat::expect_equal(config$optimal_policy$comparison_model, "MARTIN")
  testthat::expect_equal(config$optimal_policy$forecast_end, as.Date("2024-12-01"))
  testthat::expect_false(dashboard_test_config()$optimal_policy$enabled)
  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(optimal_policy = list(forecast_end = as.Date("2024-05-01"))))),
    "optimal_policy"
  )
})

testthat::test_that("configure_dashboard validates optimal-policy help/description/loss-variable hooks", {
  model <- dashboard_test_model()
  arguments <- list(
    models = list(DINGO = model),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )
  description_ui <- function() shiny::p("Fixture policy description")
  help_cd <- function() shiny::modalDialog("Fixture CD help")
  policy_config <- list(
    policy_model = "DINGO",
    forecast_end = as.Date("2024-12-01"),
    preloss_start = as.Date("2023-12-01"),
    loss_variable_choices = c("Inflation" = "infl_obs"),
    constraint_variable_choices = c("Cash Rate" = "r_obs"),
    description_ui = description_ui,
    help = list(cd = help_cd),
    loss_variable_defaults = c(infl_obs = 0.625),
    loss_variable_weight_defaults = c(dr = 0.5)
  )

  config <- do.call(configure_dashboard, c(arguments, list(optimal_policy = policy_config)))
  testthat::expect_identical(config$optimal_policy$description_ui, description_ui)
  testthat::expect_identical(config$optimal_policy$help$cd, help_cd)
  testthat::expect_null(config$optimal_policy$help$strategy)
  testthat::expect_equal(unname(config$optimal_policy$loss_variable_defaults["infl_obs"]), 0.625)
  testthat::expect_equal(unname(config$optimal_policy$loss_variable_weight_defaults["dr"]), 0.5)

  invalid_config <- policy_config
  invalid_config$help <- list(unsupported_key = identity)
  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(optimal_policy = invalid_config))),
    "Help hooks"
  )
})
