testthat::test_that("get_irf_target_gabaix returns the Dynare fixture's complete targeted IRFs", {
  dynare <- load_dynare_irf_fixture()

  raw <- get_irf_target_gabaix(
    dynare$M_, dynare$oo_,
    horizon = irf_fixture_horizon,
    target = irf_fixture_target,
    shock_timing = irf_fixture_shock_timing,
    lambda = irf_fixture_gabaix_lambda, 
    scale_targets = FALSE
  )
  pretty <- get_irf_target_gabaix(
    dynare$M_, dynare$oo_,
    horizon = irf_fixture_horizon,
    target = irf_fixture_target,
    shock_timing = irf_fixture_shock_timing,
    lambda = irf_fixture_gabaix_lambda,
    pretty = TRUE,
    shock_nickname = "Partially anticipated policy rate", 
    scale_targets = FALSE
  )

  testthat::expect_equal(
    raw,
    read_irf_expected("get-irf-target-gabaix-raw"),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_equal(
    pretty,
    read_irf_expected("get-irf-target-gabaix-pretty"),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_equal(nrow(raw), irf_fixture_horizon)
  testthat::expect_setequal(colnames(raw), dynare$M_$endo.names)
  testthat::expect_equal(
    raw[seq_along(irf_fixture_target$r_obs), "r_obs"],
    unname(irf_fixture_target$r_obs),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_true(all(c("t", "shock", "dynare_name", "value", "model_name", "gabaix_lambda") %in% names(pretty)))
  testthat::expect_true(all(pretty$shock == "Partially anticipated policy rate"))
  testthat::expect_true(all(pretty$gabaix_lambda == irf_fixture_gabaix_lambda))
})

testthat::test_that("get_irf_target_gabaix accepts unconstrained target periods", {
  dynare <- load_dynare_irf_fixture()
  sparse_target <- list(r_obs = c(0.1, NA_real_, 0.2))

  actual <- get_irf_target_gabaix(
    dynare$M_, dynare$oo_,
    horizon = irf_fixture_horizon,
    target = sparse_target,
    shock_timing = list(eps_r = c(1, 3)),
    lambda = irf_fixture_gabaix_lambda, 
    scale_targets = FALSE
  )

  constrained_periods <- which(!is.na(sparse_target$r_obs))
  testthat::expect_equal(
    actual[constrained_periods, "r_obs"],
    sparse_target$r_obs[constrained_periods],
    tolerance = irf_fixture_tolerance
  )
})