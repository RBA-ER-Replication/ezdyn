testthat::test_that("get_irf returns the Custom Moo fixture's complete IRFs", {
  moo <- load_custom_moo_irf_fixture()
  shock_names <- c("eps_r_1", "eps_r_2")

  testthat::expect_s3_class(moo, "custom_moo")
  testthat::expect_s3_class(moo$M_, "custom_moo")
  testthat::expect_s3_class(moo$oo_, "custom_moo")
  
  raw <- get_irf(
    moo$M_, moo$oo_,
    shock_names = shock_names,
    horizon = irf_fixture_horizon
  )
  wide <- get_irf(
    moo$M_, moo$oo_,
    shock_names = shock_names,
    horizon = irf_fixture_horizon,
    data_frame = TRUE
  )
  pretty <- get_irf(
    moo$M_, moo$oo_,
    shock_names = shock_names,
    horizon = irf_fixture_horizon,
    pretty = TRUE
  )

  testthat::expect_equal(
    raw,
    read_irf_expected("get-irf-custom-moo-raw"),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_equal(
    wide,
    read_irf_expected("get-irf-custom-moo-wide"),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_equal(
    pretty,
    read_irf_expected("get-irf-custom-moo-pretty"),
    tolerance = irf_fixture_tolerance
  )
  testthat::expect_named(dimnames(raw), c("response_var", "t", "shock_var"))
  testthat::expect_equal(dim(raw)[[2]], irf_fixture_horizon)
  testthat::expect_equal(dimnames(raw)$shock_var, shock_names)
  testthat::expect_true(all(c("t", "shock", "dynare_name", "value", "model_name") %in% names(pretty)))
  testthat::expect_true(all(pretty$model_name == "IRF fixture"))
})

testthat::test_that("get_irf warns and truncates Custom Moo IRFs beyond the available horizon", {
  moo <- load_custom_moo_irf_fixture()
  available_horizon <- max(moo$oo_$irf$t)

  testthat::expect_warning(
    actual <- get_irf(
      moo$M_, moo$oo_,
      shock_names = "eps_r",
      horizon = available_horizon + 1L
    ),
    "Requested horizon exceeds available IRF data"
  )

  testthat::expect_equal(dim(actual)[[2]], available_horizon)
  testthat::expect_equal(as.integer(dimnames(actual)$t), seq_len(available_horizon))
})
