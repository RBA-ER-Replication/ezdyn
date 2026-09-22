testthat::test_that("get_irf returns the Dynare fixture's complete raw IRF", {
  dynare <- load_dynare_irf_fixture()

  testthat::expect_s3_class(dynare, "dynare")
  testthat::expect_s3_class(dynare$M_, "dynare")
  testthat::expect_s3_class(dynare$oo_, "dynare")

  actual <- get_irf(
    dynare$M_, dynare$oo_,
    shock_names = c("eps_r", "eps_psi"),
    horizon = irf_fixture_horizon
  )

  testthat::expect_equal(
    actual,
    read_irf_expected("get-irf-dynare-raw"),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_type(actual, "double")
  testthat::expect_length(dim(actual), 3)
  testthat::expect_named(dimnames(actual), c("response_var", "t", "shock_var"))
  testthat::expect_equal(dim(actual)[[2]], irf_fixture_horizon)
  testthat::expect_equal(dimnames(actual)$shock_var, c("eps_r", "eps_psi"))
  testthat::expect_setequal(dimnames(actual)$response_var, dynare$M_$endo.names)
})

testthat::test_that("get_irf returns the Dynare fixture's complete wide and pretty IRFs", {
  dynare <- load_dynare_irf_fixture()

  wide <- get_irf(
    dynare$M_, dynare$oo_,
    shock_names = c("eps_r", "eps_psi"),
    horizon = irf_fixture_horizon,
    data_frame = TRUE
  )
  pretty <- get_irf(
    dynare$M_, dynare$oo_,
    shock_names = c("eps_r", "eps_psi"),
    horizon = irf_fixture_horizon,
    pretty = TRUE
  )

  testthat::expect_equal(
    wide,
    read_irf_expected("get-irf-dynare-wide"),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_equal(
    pretty,
    read_irf_expected("get-irf-dynare-pretty"),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_named(wide, c("t", "shock", dynare$M_$endo.names), ignore.order = TRUE)
  testthat::expect_true(all(c("t", "shock", "dynare_name", "value", "model_name") %in% names(pretty)))
  testthat::expect_true(all(c("display_name", "display_unit", "unit_symbol_irf", "unit_symbol_baseline") %in% names(pretty)))
  testthat::expect_true(all(pretty$model_name == "Dynare fixture"))
})

testthat::test_that("get_irf requires matching supported model-object classes", {
  dynare <- load_dynare_irf_fixture()
  moo <- load_custom_moo_irf_fixture()

  testthat::expect_error(
    get_irf("MARTIN", NULL, shock_names = "eps_r", horizon = 1L),
    "matching `dynare` or `custom_moo` classes"
  )
  testthat::expect_error(
    get_irf(dynare$M_, moo$oo_, shock_names = "eps_r", horizon = 1L),
    "matching `dynare` or `custom_moo` classes"
  )
  testthat::expect_error(
    get_irf(list(), list(), shock_names = "eps_r", horizon = 1L),
    "matching `dynare` or `custom_moo` classes"
  )
})