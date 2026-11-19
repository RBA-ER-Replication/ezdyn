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

testthat::test_that("configure_dashboard validates header links", {
  arguments <- list(
    models = list(Fixture = dashboard_test_model()),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )
  links <- c("Read the guide" = "https://example.com/guide.pdf")

  testthat::expect_null(do.call(configure_dashboard, arguments)$header_links)
  config <- do.call(configure_dashboard, c(arguments, list(header_links = links)))
  testthat::expect_identical(config$header_links, links)
  for (invalid in list("https://example.com", c(Guide = NA_character_), c(Guide = ""), list(Guide = "https://example.com"))) {
    testthat::expect_error(
      do.call(configure_dashboard, c(arguments, list(header_links = invalid))),
      "header_links"
    )
  }
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

testthat::test_that("configure_dashboard sets Alternative Paths' default responses and anticipation parameter", {
  arguments <- list(
    models = list(DINGO = dashboard_test_model("DINGO")),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )

  config <- do.call(configure_dashboard, arguments)
  testthat::expect_null(config$alt_paths$defaults$display_names)
  testthat::expect_equal(config$alt_paths$defaults$lambda, 0.8)

  config <- do.call(configure_dashboard, c(arguments, list(alt_paths = list(defaults = list(display_names = "Cash Rate", lambda = 0.5)))))
  testthat::expect_equal(config$alt_paths$defaults$display_names, "Cash Rate")
  testthat::expect_equal(config$alt_paths$defaults$lambda, 0.5)

  for (invalid in list(list(lambda = 2), list(lambda = "high"), list(display_names = 1))) {
    testthat::expect_error(
      do.call(configure_dashboard, c(arguments, list(alt_paths = list(defaults = invalid)))),
      "alt_paths\\$defaults` contains invalid"
    )
  }
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
    models = c("DINGO", "MARTIN"),
    defaults = list(models = "MARTIN"),
    baseline_model = "MARTIN",
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
  testthat::expect_equal(config$optimal_policy$models, c("DINGO", "MARTIN"))
  testthat::expect_equal(config$optimal_policy$defaults$models, "MARTIN")
  testthat::expect_equal(config$optimal_policy$baseline_model, "MARTIN")
  testthat::expect_equal(config$optimal_policy$forecast_end, as.Date("2024-12-01"))
  testthat::expect_true(config$optimal_policy$use_cd)
  testthat::expect_equal(config$optimal_policy$lambda, 0.8)

  # Models and the baseline model default to every model and the first model.
  default_config <- do.call(configure_dashboard, c(arguments, list(
    optimal_policy = policy_config[setdiff(names(policy_config), c("models", "defaults", "baseline_model"))]
  )))
  testthat::expect_equal(default_config$optimal_policy$models, c("DINGO", "MARTIN"))
  testthat::expect_equal(default_config$optimal_policy$defaults$models, c("DINGO", "MARTIN"))
  testthat::expect_equal(default_config$optimal_policy$baseline_model, "DINGO")

  testthat::expect_false(dashboard_test_config()$optimal_policy$enabled)
  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(optimal_policy = list(forecast_end = as.Date("2024-05-01"))))),
    "optimal_policy"
  )
  # The baseline model must be one of the Optimal Policy models.
  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(optimal_policy = utils::modifyList(policy_config, list(models = "DINGO", defaults = list(models = "DINGO")))))),
    "baseline_model"
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

testthat::test_that("configure_dashboard labels unnamed loss and constraint choices with their values", {
  model <- dashboard_test_model()
  arguments <- list(
    models = list(DINGO = model),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline()),
    default_baseline = "Staff"
  )
  policy_config <- list(
    policy_model = "DINGO",
    forecast_end = as.Date("2024-12-01"),
    preloss_start = as.Date("2023-12-01"),
    loss_variable_choices = c("Inflation", "Cash rate changes" = "dr"),
    constraint_variable_choices = "Cash Rate"
  )

  config <- do.call(configure_dashboard, c(arguments, list(optimal_policy = policy_config)))
  testthat::expect_identical(config$optimal_policy$loss_variable_choices, c("Inflation" = "Inflation", "Cash rate changes" = "dr"))
  testthat::expect_identical(config$optimal_policy$constraint_variable_choices, c("Cash Rate" = "Cash Rate"))

  for (invalid in list(character(0), list("Inflation"), c("Inflation", NA))) {
    policy_config$loss_variable_choices <- invalid
    testthat::expect_error(do.call(configure_dashboard, c(arguments, list(optimal_policy = policy_config))), "choice-list")
  }

  policy_config$loss_variable_choices <- "Inflation"
  policy_config$loss_variable_vintages <- data.frame(date = as.Date("2023-12-01"), Inflation_gap_loss = 0)
  testthat::expect_error(
    do.call(configure_dashboard, c(arguments, list(optimal_policy = policy_config))),
    "needs `vintage` and `date` columns"
  )
})
