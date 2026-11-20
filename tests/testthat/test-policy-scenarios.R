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

testthat::test_that("commitment leaves a policy shock with no effect on the loss at zero", {
  # The second shock arrives in the last period and the loss variable only
  # responds with a lag, so it can't affect the loss.
  dY <- array(
    c(1, 1, 0, 0),
    dim = c(1, 2, 2),
    dimnames = list("infl_obs", NULL, NULL)
  )

  shocks <- ezdyn_policy_commit(
    Y0 = matrix(c(1, 1), nrow = 1, dimnames = list("infl_obs", NULL)),
    dY = dY,
    weights = 1,
    discount_factor = 1,
    constraints = list(A_ineq = matrix(0, 0, 2), b_ineq = numeric())
  )

  testthat::expect_equal(shocks, c(-1, 0))
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

# Responses at t to a shock in period s, for a shock whose effect `irf`
# doesn't depend on when it arrives.
policy_test_shifted_irf <- function(irf, horizon, instruments) {
  dY <- array(0, c(1, horizon, instruments), list("infl_obs", NULL, NULL))
  for (s in seq_len(instruments)) {
    periods <- s:horizon
    dY[1, periods, s] <- irf[periods - s + 1]
  }
  dY
}

testthat::test_that("discretion allows a loss horizon longer than the instrument horizon", {
  horizon <- 4L
  instruments <- 2L
  beta <- 0.9
  Y0 <- c(1, 0.8, 0.5, 0.2)
  dY <- policy_test_shifted_irf(c(1, 0.6, 0.3, 0.1), horizon, instruments)
  no_constraints <- ezdyn_policy_discretion_constraints(list(), dY)

  shocks <- ezdyn_policy_discretion(
    Y0 = matrix(Y0, nrow = 1, dimnames = list("infl_obs", NULL)),
    dY = dY, weights = 1, discount_factor = beta, constraints = no_constraints
  )

  # Brute-force backward induction: the period-2 policymaker minimises the
  # loss over periods 2-4 given the first shock; the period-1 policymaker
  # minimises the loss over periods 1-4, anticipating that reaction.
  loss_from <- function(period, s) {
    values <- Y0 + as.numeric(dY[1, , ] %*% s)
    sum(beta^(seq_len(horizon - period + 1) - 1) * values[period:horizon]^2)
  }
  second <- function(s1) stats::optimize(function(s2) loss_from(2, c(s1, s2)), c(-50, 50), tol = 1e-12)$minimum
  s1 <- stats::optimize(function(s1) loss_from(1, c(s1, second(s1))), c(-50, 50), tol = 1e-12)$minimum

  testthat::expect_length(shocks, instruments)
  testthat::expect_equal(shocks, c(s1, second(s1)), tolerance = 1e-5)
})

testthat::test_that("discretion with one instrument period matches commitment over a longer loss horizon", {
  dY <- array(
    c(1, 0.5, 0.6, 0.4, 0.3, 0.2),
    dim = c(2, 3, 1),
    dimnames = list(c("infl_obs", "y_obs"), NULL, NULL)
  )
  Y0 <- matrix(c(1, -0.5, 0.6, 0.2, 0.3, 0.1), nrow = 2, dimnames = list(c("infl_obs", "y_obs"), NULL))

  discretion <- ezdyn_policy_discretion(Y0, dY, c(1, 0.5), 0.99, ezdyn_policy_discretion_constraints(list(), dY))
  commitment <- ezdyn_policy_commit(Y0, dY, c(1, 0.5), 0.99, list(A_ineq = matrix(0, 0, 1), b_ineq = numeric()))

  testthat::expect_equal(discretion, commitment, tolerance = 1e-8)
})

testthat::test_that("discretion explains why a loss horizon shorter than the instrument horizon is rejected", {
  dY <- policy_test_shifted_irf(c(1, 0.5, 0.25), 3L, 3L)[, 1:2, , drop = FALSE]

  testthat::expect_error(
    ezdyn_policy_discretion(
      Y0 = matrix(c(1, 1), nrow = 1, dimnames = list("infl_obs", NULL)),
      dY = dY, weights = 1, discount_factor = 0.99,
      constraints = ezdyn_policy_discretion_constraints(list(), dY)
    ),
    "loss horizon is 2 and the instrument horizon is 3.*quarters 3 to 3 would have no loss left to minimise"
  )
})

testthat::test_that("get_policy_scenario() runs discretion with a longer loss horizon and rejects a shorter one", {
  model <- load_dynare_irf_fixture()
  dates <- seq(as.Date("2024-03-01"), by = "quarter", length.out = 4)
  baseline <- data.frame(date = dates, r_obs = 4, infl_obs = 1)
  strategy <- list(
    output_name = "Discretion", output_variables = c("r_obs", "infl_obs"), loss_variables = "infl_obs",
    loss_weights = 1, discount_factor = 0.99, T_loss = 4L, T_instrument = 2L, instrument_variable = "r_obs", commit = "disc"
  )
  run <- function(strategy) {
    get_policy_scenario(strategy, baseline, model, forecast_start = dates[[1]], forecast_end = dates[[4]])
  }

  output <- run(strategy)
  testthat::expect_setequal(unique(output$path_name), c("Baseline", "Discretion"))
  testthat::expect_false(anyNA(output$value))

  strategy$T_loss <- 1L
  testthat::expect_error(run(strategy), "loss horizon is 1 and the instrument horizon is 2")
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

# A timeless-commitment setup shared by two models with different codes: a
# display-name baseline with its own gap column, and one past vintage
# (2023-12-01) keyed by display names.
policy_timeless_fixture <- function() {
  dates <- seq(as.Date("2024-03-01"), by = "quarter", length.out = 4)
  baseline <- data.frame(date = dates, `Cash Rate` = 4, `Inflation` = 3, check.names = FALSE)
  baseline[["Inflation_gap_loss"]] <- baseline[["Inflation"]] - 2.5
  vintage_dates <- seq(as.Date("2023-12-01"), by = "quarter", length.out = 5)
  list(
    dates = dates,
    baseline = baseline,
    vintages = data.frame(
      vintage = as.Date("2023-12-01"), date = vintage_dates,
      `Inflation_gap_loss` = c(1, 0.8, 0.6, 0.4, 0.2), `Cash Rate` = 4,
      check.names = FALSE
    ),
    strategy = list(
      output_name = "Commitment", output_variables = c("Cash Rate", "Inflation"), loss_variables = "Inflation_gap_loss",
      loss_weights = 1, discount_factor = 0.99, T_loss = 3L, T_instrument = 3L, instrument_variable = "Cash Rate",
      instrument_shock = "Monetary policy shock", commit = "timeless"
    )
  )
}

testthat::test_that("timeless commitment reads display-name `_gap_loss` vintages for models with different codes", {
  fixture <- policy_timeless_fixture()
  run <- function(model) {
    get_policy_scenario(
      fixture$strategy, fixture$baseline, model,
      forecast_start = fixture$dates[[1]], forecast_end = fixture$dates[[4]],
      loss_variable_vintages = fixture$vintages, preloss_start = as.Date("2023-12-01")
    )
  }

  actual_a <- run(policy_fixture_model(c("r_obs", "infl_obs"), "eps_r"))
  actual_b <- run(policy_fixture_model(c("cr", "pi"), "eps_mp"))

  testthat::expect_setequal(unique(actual_a$path_name), c("Baseline", "Commitment"))
  testthat::expect_equal(actual_a$value, actual_b$value)
})

testthat::test_that("timeless commitment's history holds the display-name vintages' gaps under the model's codes", {
  fixture <- policy_timeless_fixture()
  model <- policy_fixture_model(c("cr", "pi"), "eps_mp")
  originals <- list(pi_gap_loss = "Inflation_gap_loss", cr = "Cash Rate")

  vintages <- ezdyn_policy_vintages(fixture$vintages, "pi_gap_loss", "cr", TRUE, originals, model$M_)
  history <- ezdyn_policy_history(
    vintages, "pi_gap_loss", as.Date("2023-12-01"), as.Date("2024-03-01"), 3,
    matrix(c(0.5, 0.5, 0.5), nrow = 1, dimnames = list("pi_gap_loss", NULL))
  )

  testthat::expect_true(all(c("pi_gap_loss", "cr") %in% names(vintages)))
  # The 2023-12-01 vintage over 2023-12-01 to 2024-09-01, then the current
  # (2024-03-01) baseline's gaps from its first quarter.
  testthat::expect_equal(unname(history["pi_gap_loss", , 1]), c(1, 0.8, 0.6, 0.4))
  testthat::expect_equal(unname(history["pi_gap_loss", 2:4, 2]), c(0.5, 0.5, 0.5))
})

testthat::test_that("timeless commitment names the vintage columns it is missing", {
  fixture <- policy_timeless_fixture()
  model <- policy_fixture_model(c("r_obs", "infl_obs"), "eps_r")
  run <- function(strategy, vintages) {
    get_policy_scenario(
      strategy, fixture$baseline, model,
      forecast_start = fixture$dates[[1]], forecast_end = fixture$dates[[4]],
      loss_variable_vintages = vintages, preloss_start = as.Date("2023-12-01")
    )
  }

  # A level column is not a gap column.
  levels_only <- fixture$vintages
  names(levels_only)[names(levels_only) == "Inflation_gap_loss"] <- "Inflation"
  testthat::expect_error(run(fixture$strategy, levels_only), "`Inflation_gap_loss` (one `<loss variable>_gap_loss` column per loss variable", fixed = TRUE)
  testthat::expect_error(run(fixture$strategy, NULL), "needs loss-variable vintages.*`Inflation_gap_loss`")

  # The instrument's level is only needed for a historical (ELB) constraint.
  no_rate <- fixture$vintages[setdiff(names(fixture$vintages), "Cash Rate")]
  testthat::expect_no_error(run(fixture$strategy, no_rate))
  elb <- fixture$strategy
  elb$constraints <- list(list(direction = "lower", variable = "Cash Rate", period = 1:3, value = 0.1, historical = TRUE))
  testthat::expect_error(run(elb, no_rate), "`Cash Rate` (the instrument's level", fixed = TRUE)
  testthat::expect_no_error(run(elb, fixture$vintages))

  short <- fixture$vintages[fixture$vintages$date <= as.Date("2024-03-01"), ]
  testthat::expect_error(run(fixture$strategy, short), "vintage 2023-12-01 has no row for 2024-06-01 to 2024-09-01", fixed = TRUE)
})

# Run the timeless fixture with the given vintages and commitment start.
policy_timeless_run <- function(vintages, preloss_start = as.Date("2023-12-01"), strategy = NULL) {
  fixture <- policy_timeless_fixture()
  get_policy_scenario(
    if (is.null(strategy)) fixture$strategy else strategy, fixture$baseline,
    policy_fixture_model(c("r_obs", "infl_obs"), "eps_r"),
    forecast_start = fixture$dates[[1]], forecast_end = fixture$dates[[4]],
    loss_variable_vintages = vintages, preloss_start = preloss_start
  )
}

testthat::test_that("timeless commitment from the forecast start needs no vintages", {
  fixture <- policy_timeless_fixture()
  commit <- fixture$strategy
  commit$commit <- "commit"
  expected <- policy_timeless_run(NULL, NULL, commit)

  testthat::expect_equal(policy_timeless_run(NULL, NULL)$value, expected$value)
  testthat::expect_equal(policy_timeless_run(data.frame(x = 1), fixture$dates[[1]])$value, expected$value)
  testthat::expect_error(policy_timeless_run(fixture$vintages, as.Date("2024-06-01")), "`preloss_start` (2024-06-01) is after `forecast_start` (2024-03-01)", fixed = TRUE)
})

testthat::test_that("timeless commitment reads vintage dates in any form, on any day of the quarter", {
  fixture <- policy_timeless_fixture()
  expected <- policy_timeless_run(fixture$vintages)$value
  with_keys <- function(vintage, date) {
    vintages <- fixture$vintages
    vintages$vintage <- vintage
    vintages$date <- date
    vintages
  }
  days <- format(fixture$vintages$date)
  quarter_starts <- seq(as.Date("2023-10-01"), by = "quarter", length.out = 5)

  variants <- list(
    utc = with_keys(as.POSIXct("2023-12-01", tz = "UTC"), as.POSIXct(days, tz = "UTC")),
    sydney = with_keys(as.POSIXct("2023-12-01", tz = "Australia/Sydney"), as.POSIXct(days, tz = "Australia/Sydney")),
    quarter_start = with_keys(as.Date("2023-10-01"), quarter_starts),
    mid_quarter = with_keys(as.Date("2023-11-15"), quarter_starts + 40),
    strings = with_keys("2023Q4", c("2023Q4", "2024Q1", "2024Q2", "2024Q3", "2024Q4")),
    iso = with_keys("2023-12-01", days)
  )
  for (name in names(variants)) {
    testthat::expect_equal(policy_timeless_run(variants[[name]])$value, expected, label = name)
  }
  testthat::expect_equal(policy_timeless_run(fixture$vintages, as.Date("2023-10-17"))$value, expected)
})

testthat::test_that("timeless commitment rejects blank, duplicate and unreadable vintage dates", {
  fixture <- policy_timeless_fixture()
  vintages <- fixture$vintages

  blank_vintage <- vintages
  blank_vintage$vintage[2] <- NA
  testthat::expect_error(policy_timeless_run(blank_vintage), "The vintages' `vintage` column is blank in row(s) 2.", fixed = TRUE)
  blank_date <- vintages
  blank_date$date[c(3, 5)] <- NA
  testthat::expect_error(policy_timeless_run(blank_date), "The vintages' `date` column is blank in row(s) 3, 5.", fixed = TRUE)

  repeated <- vintages[2, ]
  repeated$Inflation_gap_loss <- 99
  duplicate <- rbind(vintages, repeated)
  testthat::expect_error(policy_timeless_run(duplicate), "more than one row for vintage 2023-12-01, date 2024-03-01 (rows 2, 6)", fixed = TRUE)
  # Two days in the same quarter are the same quarter.
  repeated$date <- as.Date("2024-02-01")
  same_quarter <- rbind(vintages, repeated)
  testthat::expect_error(policy_timeless_run(same_quarter), "date 2024-03-01 (rows 2, 6)", fixed = TRUE)

  serial <- vintages
  serial$vintage <- 45261
  testthat::expect_error(policy_timeless_run(serial), "not numeric values (e.g. Excel date serial numbers", fixed = TRUE)
  unreadable <- vintages
  unreadable$date <- c("2023-12-01", "Mar 2024", "2024-06-01", "2024-09-01", "2024-12-01")
  testthat::expect_error(policy_timeless_run(unreadable), "\"Mar 2024\" in row 2", fixed = TRUE)
})

testthat::test_that("incomplete vintages are reported by column, vintage and quarter", {
  fixture <- policy_timeless_fixture()

  blank <- fixture$vintages
  blank$Inflation_gap_loss[2:3] <- NA
  testthat::expect_error(policy_timeless_run(blank), "`Inflation_gap_loss` in vintage 2023-12-01 is blank for 2024-03-01 to 2024-06-01", fixed = TRUE)

  # Every problem is listed at once, including a vintage with no rows.
  gappy <- fixture$vintages[-4, ]
  gappy$Inflation_gap_loss[2] <- NA
  error <- tryCatch(
    ezdyn_policy_history(
      gappy, "Inflation_gap_loss", as.Date("2023-09-01"), as.Date("2024-03-01"), 3,
      matrix(0, 1, 3, dimnames = list("Inflation_gap_loss", NULL))
    ),
    error = identity
  )
  testthat::expect_s3_class(error, "ezdyn_policy_vintage_error")
  testthat::expect_match(conditionMessage(error), "vintage 2023-09-01 has no rows", fixed = TRUE)
  testthat::expect_match(conditionMessage(error), "vintage 2023-12-01 has no row for 2024-09-01", fixed = TRUE)
  testthat::expect_match(conditionMessage(error), "`Inflation_gap_loss` in vintage 2023-12-01 is blank for 2024-03-01", fixed = TRUE)

  text <- fixture$vintages
  text$Inflation_gap_loss <- as.character(text$Inflation_gap_loss)
  testthat::expect_error(policy_timeless_run(text), "`Inflation_gap_loss` must be numeric", fixed = TRUE)
})

testthat::test_that("a failing strategy is omitted with a warning, and the rest still run", {
  fixture <- policy_timeless_fixture()
  models <- list(A = policy_fixture_model(c("r_obs", "infl_obs"), "eps_r"))
  good <- fixture$strategy
  good$commit <- "commit"
  bad <- good
  bad$loss_weights <- c(1, 2)
  run <- function(strategies) {
    get_policy_scenarios(
      strategies, models, fixture$baseline,
      forecast_start = fixture$dates[[1]], forecast_end = fixture$dates[[4]]
    )
  }

  expected <- run(list(good))
  testthat::expect_warning(actual <- run(list(bad, good)), "Omitted `Optimal (A)` for model `A`: `loss_weights` must be", fixed = TRUE)
  testthat::expect_equal(actual, expected)
  testthat::expect_error(run(list(bad)), "Omitted `Optimal (A)` for model `A`: `loss_weights` must be", fixed = TRUE)
})
