testthat::test_that("get_irf_target returns the Dynare fixture's complete targeted IRFs", {
  dynare <- load_dynare_irf_fixture()

  raw <- get_irf_target(
    dynare$M_, dynare$oo_,
    horizon = irf_fixture_horizon,
    target = irf_fixture_target,
    shock_timing = irf_fixture_shock_timing, 
    scale_targets = FALSE
  )
  pretty <- get_irf_target(
    dynare$M_, dynare$oo_,
    horizon = irf_fixture_horizon,
    target = irf_fixture_target,
    shock_timing = irf_fixture_shock_timing,
    pretty = TRUE,
    shock_nickname = "Targeted policy rate", 
    scale_targets = FALSE
  )

  testthat::expect_equal(
    raw,
    read_irf_expected("get-irf-target-raw"),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_equal(
    pretty,
    read_irf_expected("get-irf-target-pretty"),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_equal(nrow(raw), irf_fixture_horizon)
  testthat::expect_setequal(colnames(raw), dynare$M_$endo.names)
  testthat::expect_equal(
    raw[seq_along(irf_fixture_target$r_obs), "r_obs"],
    unname(irf_fixture_target$r_obs),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_true(all(c("t", "shock", "dynare_name", "value", "model_name") %in% names(pretty)))
  testthat::expect_true(all(pretty$shock == "Targeted policy rate"))
})

testthat::test_that("get_irf_target rejects a non-square target system", {
  dynare <- load_dynare_irf_fixture()

  testthat::expect_error(
    get_irf_target(
      dynare$M_, dynare$oo_,
      horizon = irf_fixture_horizon,
      target = list(r_obs = c(0.1, 0.2)),
      shock_timing = list(eps_r = 1), 
      scale_targets = FALSE
    ),
    "Mh_flat should be square"
  )
})