testthat::test_that("optimal commitment returns labelled baseline and policy paths", {
  model <- load_dynare_irf_fixture()
  dates <- seq(as.Date("2024-03-01"), by = "quarter", length.out = 4)
  baseline <- data.frame(date = dates, r_obs = 4, infl_obs = 0)
  strategy <- list(
    output_name = "Commitment",
    output_variables = c("r_obs", "infl_obs"),
    loss_variables = "infl_obs",
    loss_weights = 1,
    discount_factor = 0.99,
    T_loss = 4L,
    T_instrument = 4L,
    instrument_variable = "r_obs",
    commit = "commit"
  )

  output <- get_policy_scenario(
    policy_strategy = strategy,
    baseline = baseline,
    model = model,
    forecast_start = dates[[1]],
    forecast_end = dates[[4]]
  )

  testthat::expect_setequal(unique(output$path_name), c("Baseline", "Commitment"))
  testthat::expect_setequal(unique(output$dynare_name), c("r_obs", "infl_obs"))
  testthat::expect_true(all(c("display_name", "display_unit", "model_name", "value") %in% names(output)))
})

testthat::test_that("policy horizons must fit the supplied baseline forecast", {
  model <- load_dynare_irf_fixture()
  baseline <- data.frame(date = seq(as.Date("2024-03-01"), by = "quarter", length.out = 2), r_obs = 4, infl_obs = 0)
  strategy <- list(
    output_name = "Commitment", output_variables = "r_obs", loss_variables = "infl_obs",
    loss_weights = 1, discount_factor = 0.99, T_loss = 3L, T_instrument = 2L, instrument_variable = "r_obs", commit = "commit"
  )

  testthat::expect_error(
    get_policy_scenario(strategy, baseline, model, forecast_start = baseline$date[[1]], forecast_end = baseline$date[[2]]),
    "within the supplied forecast horizon"
  )
})

testthat::test_that("optimal policy output retains pre-optimisation baseline history", {
  model <- load_dynare_irf_fixture()
  dates <- seq(as.Date("2023-09-01"), by = "quarter", length.out = 5)
  baseline <- data.frame(date = dates, r_obs = 4, infl_obs = 0)
  strategy <- list(
    output_name = "Commitment", output_variables = "r_obs", loss_variables = "infl_obs",
    loss_weights = 1, discount_factor = 0.99, T_loss = 4L, T_instrument = 4L,
    instrument_variable = "r_obs", commit = "commit"
  )

  actual <- get_policy_scenario(
    strategy, baseline, model,
    forecast_start = dates[[2]], forecast_end = dates[[5]]
  )

  testthat::expect_true(any(actual$path_name == "Baseline" & actual$date == dates[[1]]))
})

testthat::test_that("anticipated-policy fallback is explicit", {
  irf <- expand.grid(
    t = 1:4,
    shock = "eps_r",
    resp_var = c("r_obs", "infl_obs"),
    KEEP.OUT.ATTRS = FALSE
  )
  irf$value <- 1
  model <- custom_moo(
    irf = irf,
    model_name = "Fallback fixture",
    meta = NULL
  )
  model$M_$varmeta <- data.frame(
    dynare_name = c("r_obs", "infl_obs"),
    display_name = c("Cash Rate", "Inflation"),
    units = c("Per cent", "Percentage points"),
    scale_factor = I(list(1, 1))
  )
  model$M_$exo.names <- "eps_r"
  testthat::expect_warning(
    build_policy_irfs(model, c("r_obs", "infl_obs"), 4, 2, use_cd = TRUE, lambda = 0.8),
    "using unanticipated"
  )
})

testthat::test_that("policy IRFs derive year-ended GDP growth from quarterly growth", {
  irf <- array(
    c(1, 2, 3, 4),
    dim = c(1, 4, 1),
    dimnames = list("gdp_growth", NULL, "eps_r")
  )
  varmeta <- data.frame(
    dynare_name = "gdp_growth_ye",
    derived_from = "gdp_growth",
    derived_transform = "year_ended_sum",
    stringsAsFactors = FALSE
  )

  actual <- ezdyn_subset_irf_variables(irf, "gdp_growth_ye", varmeta)

  testthat::expect_equal(as.numeric(actual), c(1, 3, 6, 10))
})

testthat::test_that("policy IRFs derive the cash-rate change and its acceleration from r_obs", {
  irf <- array(
    c(1, 3, 6, 10),
    dim = c(1, 4, 1),
    dimnames = list("r_obs", NULL, "eps_r")
  )
  varmeta <- data.frame(
    dynare_name = c("dr", "ddr"),
    derived_from = c("r_obs", "r_obs"),
    derived_transform = c("diff", "diff2"),
    stringsAsFactors = FALSE
  )

  actual <- ezdyn_subset_irf_variables(irf, c("dr", "ddr"), varmeta)

  testthat::expect_equal(as.numeric(actual["dr", , ]), c(1, 2, 3, 4))
  testthat::expect_equal(as.numeric(actual["ddr", , ]), c(1, 1, 1, 1))
})

testthat::test_that("policy IRFs apply the source's own scale_factor before a real transform, not just for aliases", {
  irf <- array(
    c(1, 3, 6, 10),
    dim = c(1, 4, 1),
    dimnames = list("r_obs", NULL, "eps_r")
  )
  varmeta <- data.frame(
    dynare_name = c("dr", "r_obs"),
    derived_from = c("r_obs", NA_character_),
    derived_transform = c("diff", NA_character_),
    scale_factor = I(list(1, 2)),
    stringsAsFactors = FALSE
  )

  actual <- ezdyn_subset_irf_variables(irf, "dr", varmeta)

  testthat::expect_equal(as.numeric(actual), 2 * c(1, 2, 3, 4))
})

testthat::test_that("policy IRFs alias a variable to a differently-named raw source with no arithmetic", {
  irf <- array(
    c(1, 2, 3, 4),
    dim = c(1, 4, 1),
    dimnames = list("unemp1", NULL, "eps_r")
  )
  varmeta <- data.frame(
    dynare_name = c("unemp", "unemp1"),
    derived_from = c("unemp1", NA_character_),
    derived_transform = c(NA_character_, NA_character_),
    scale_factor = I(list(1, 1)),
    stringsAsFactors = FALSE
  )

  actual <- ezdyn_subset_irf_variables(irf, "unemp", varmeta)

  testthat::expect_equal(as.numeric(actual), c(1, 2, 3, 4))
})

testthat::test_that("policy IRFs apply an alias source's own scale_factor, not the target's", {
  irf <- array(
    c(1, 2, 3, 4),
    dim = c(1, 4, 1),
    dimnames = list("unemp1", NULL, "eps_r")
  )
  varmeta <- data.frame(
    dynare_name = c("unemp", "unemp1"),
    derived_from = c("unemp1", NA_character_),
    derived_transform = c(NA_character_, NA_character_),
    scale_factor = I(list(1, 100)),
    stringsAsFactors = FALSE
  )

  actual <- ezdyn_subset_irf_variables(irf, "unemp", varmeta)

  testthat::expect_equal(as.numeric(actual), c(100, 200, 300, 400))
})

testthat::test_that("build_policy_irfs composes the source's and the derived variable's own scale_factor exactly once", {
  irf <- expand.grid(
    t = 1:4,
    shock = "eps_r",
    resp_var = "r_obs",
    KEEP.OUT.ATTRS = FALSE
  )
  irf$value <- 1
  model <- custom_moo(irf = irf, model_name = "Scaled derived fixture", meta = NULL)
  model$M_$varmeta <- data.frame(
    dynare_name = c("r_obs", "dr"),
    display_name = c("Cash Rate", "Cash Rate Change"),
    units = c("Per cent", "Change"),
    derived_from = c(NA_character_, "r_obs"),
    derived_transform = c(NA_character_, "diff"),
    scale_factor = I(list(2, 3)),
    stringsAsFactors = FALSE
  )
  model$M_$exo.names <- "eps_r"

  actual <- build_policy_irfs(model, c("r_obs", "dr"), horizon = 4, instrument_horizon = 1)

  expected <- 3 * ezdyn_linear_filter(2 * rep(1, 4), c(1, -1))
  testthat::expect_equal(as.numeric(actual["dr", , 1]), expected)
})

testthat::test_that("timeless policy history ignores dates outside its required window", {
  dates <- seq(as.Date("2024-03-01"), by = "quarter", length.out = 4)
  vintages <- data.frame(
    date = dates,
    vintage = rep(as.Date("2024-03-01"), length(dates)),
    infl_obs = seq_along(dates)
  )

  actual <- ezdyn_policy_history(
    vintages = vintages,
    variables = "infl_obs",
    preloss_start = as.Date("2024-03-01"),
    forecast_start = as.Date("2024-06-01"),
    horizon = 2,
    current = matrix(c(5, 6), nrow = 1, dimnames = list("infl_obs", NULL))
  )

  testthat::expect_equal(dim(actual), c(1, 3, 2))
  testthat::expect_equal(unname(actual["infl_obs", , 1]), 1:3)
})

testthat::test_that("discretionary policy implements time-varying lower bounds", {
  dY <- array(
    c(1, 0, 0, 1),
    dim = c(1, 2, 2),
    dimnames = list("r_obs", NULL, NULL)
  )
  constraints <- list(
    list(direction = "lower", variable = "r_obs", period = 1:2, value = 0)
  )
  matrices <- ezdyn_policy_discretion_constraints(constraints, dY)
  shocks <- ezdyn_policy_discretion(
    Y0 = matrix(c(-1, -1), nrow = 1, dimnames = list("r_obs", NULL)),
    dY = dY,
    weights = 1,
    discount_factor = 0.99,
    constraints = matrices
  )

  testthat::expect_equal(shocks, c(1, 1), tolerance = 1e-6)
})

testthat::test_that("commitment constraints are respected and infeasible bounds error", {
  dY <- array(c(1, 0, 0, 1), dim = c(1, 2, 2), dimnames = list("r_obs", NULL, NULL))
  Y0 <- matrix(c(-1, -1), nrow = 1, dimnames = list("r_obs", NULL))
  constraints <- list(list(direction = "lower", variable = "r_obs", period = 1:2, value = 0))
  shocks <- ezdyn_policy_commit(Y0, dY, 1, 0.99, ezdyn_policy_constraints(constraints, dY, Y0))

  testthat::expect_true(all(ezdyn_apply_policy_shocks(dY, shocks) + Y0 >= -1e-6))
  testthat::expect_error(
    ezdyn_policy_commit(Y0, dY, 1, 0.99, ezdyn_policy_constraints(list(list(direction = "lower", variable = "r_obs", period = 1, value = Inf)), dY, Y0))
  )
})

# Custom-MOO fixture with different dynare codes to DINGO-style models, but the
# SAME display names/shock description - used to prove `get_policy_scenario()`
# no longer requires two models being compared to share raw variable codes.
policy_fixture_model <- function(response_codes, shock_code) {
  irf <- expand.grid(
    t = 1:4,
    shock = shock_code,
    resp_var = response_codes,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  irf$value <- ifelse(irf$resp_var == response_codes[[1]], 1 / irf$t, 0.25 / irf$t)
  model <- list(
    M_ = list(
      model_name = "Fixture",
      endo.vars = response_codes,
      exo.vars = shock_code,
      varmeta = data.frame(
        dynare_name = response_codes,
        display_name = c("Cash Rate", "Inflation"),
        units = c("Per cent", "Percentage points"),
        unit_symbol_baseline = c("%", "ppt"),
        default = TRUE,
        stringsAsFactors = FALSE
      ),
      shock_meta = data.frame(shock = shock_code, description = "Monetary policy shock", stringsAsFactors = FALSE)
    ),
    oo_ = list(irf = irf)
  )
  class(model$M_) <- "custom_moo"
  class(model$oo_) <- "custom_moo"
  class(model) <- "custom_moo"
  model
}

testthat::test_that("get_policy_scenario() resolves display names against a canonical, display-name-keyed baseline", {
  model <- load_dynare_irf_fixture()
  dates <- seq(as.Date("2024-03-01"), by = "quarter", length.out = 4)
  code_baseline <- data.frame(date = dates, r_obs = 4, infl_obs = 0)
  canonical_baseline <- data.frame(date = dates, `Cash Rate` = 4, `Inflation` = 0, check.names = FALSE)
  code_strategy <- list(
    output_name = "Commitment", output_variables = c("r_obs", "infl_obs"), loss_variables = "infl_obs",
    loss_weights = 1, discount_factor = 0.99, T_loss = 4L, T_instrument = 4L, instrument_variable = "r_obs", commit = "commit"
  )
  display_strategy <- list(
    output_name = "Commitment", output_variables = c("Cash Rate", "Inflation"), loss_variables = "Inflation",
    loss_weights = 1, discount_factor = 0.99, T_loss = 4L, T_instrument = 4L, instrument_variable = "Cash Rate",
    instrument_shock = "Monetary policy shock", commit = "commit"
  )

  expected <- get_policy_scenario(code_strategy, code_baseline, model, forecast_start = dates[[1]], forecast_end = dates[[4]])
  actual <- get_policy_scenario(display_strategy, canonical_baseline, model, forecast_start = dates[[1]], forecast_end = dates[[4]])

  testthat::expect_equal(actual$value, expected$value)
  testthat::expect_setequal(actual$display_name, c("Cash Rate", "Inflation"))
})

testthat::test_that("get_policy_scenario() resolves `_gap_loss` loss columns from a display-name baseline", {
  model <- load_dynare_irf_fixture()
  dates <- seq(as.Date("2024-03-01"), by = "quarter", length.out = 4)
  canonical_baseline <- data.frame(date = dates, `Cash Rate` = 4, `Inflation` = 0, check.names = FALSE)
  canonical_baseline[["Inflation_gap_loss"]] <- canonical_baseline[["Inflation"]] - 2
  strategy <- list(
    output_name = "Commitment", output_variables = "Cash Rate", loss_variables = "Inflation_gap_loss",
    loss_weights = 1, discount_factor = 0.99, T_loss = 4L, T_instrument = 4L, instrument_variable = "Cash Rate", commit = "commit"
  )

  actual <- get_policy_scenario(strategy, canonical_baseline, model, forecast_start = dates[[1]], forecast_end = dates[[4]])

  testthat::expect_setequal(unique(actual$path_name), c("Baseline", "Commitment"))
})

testthat::test_that("the same display-name policy_strategy and baseline run against two models with different codes", {
  baseline <- data.frame(date = seq(as.Date("2024-03-01"), by = "quarter", length.out = 4), `Cash Rate` = 4, `Inflation` = 0, check.names = FALSE)
  strategy <- list(
    output_name = "Commitment", output_variables = c("Cash Rate", "Inflation"), loss_variables = "Inflation",
    loss_weights = 1, discount_factor = 0.99, T_loss = 4L, T_instrument = 4L, instrument_variable = "Cash Rate",
    instrument_shock = "Monetary policy shock", commit = "commit"
  )
  model_a <- policy_fixture_model(c("r_obs", "infl_obs"), "eps_r")
  model_b <- policy_fixture_model(c("cr", "pi"), "eps_mp")

  actual_a <- get_policy_scenario(strategy, baseline, model_a, forecast_start = baseline$date[[1]], forecast_end = baseline$date[[4]])
  actual_b <- get_policy_scenario(strategy, baseline, model_b, forecast_start = baseline$date[[1]], forecast_end = baseline$date[[4]])

  testthat::expect_setequal(actual_a$display_name, c("Cash Rate", "Inflation"))
  testthat::expect_setequal(actual_b$display_name, c("Cash Rate", "Inflation"))
})
