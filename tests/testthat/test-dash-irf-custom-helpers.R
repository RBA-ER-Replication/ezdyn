testthat::test_that("irf_run_direct combines pretty IRFs per model with asymmetric native shock codes", {
  dingo <- dashboard_test_model("DINGO", shock_name = "eps_dingo", shock_description = "Monetary policy shock")
  martin <- dashboard_test_model("MARTIN", shock_name = "eps_martin", shock_description = "Monetary policy shock")
  models <- list(DINGO = dingo, MARTIN = martin)
  model_shocks <- c(DINGO = "eps_dingo", MARTIN = "eps_martin")

  actual <- irf_run_direct(models, model_shocks, horizon = 4)

  testthat::expect_setequal(unique(actual$model_name), c("DINGO", "MARTIN"))
  testthat::expect_true(all(actual$irf_mode == "direct"))
  testthat::expect_equal(
    actual |> dplyr::filter(model_name == "DINGO") |> dplyr::select(-irf_mode),
    get_irf(dingo$M_, dingo$oo_, "eps_dingo", horizon = 4, pretty = TRUE) |>
      irf_normalize_pretty() |>
      dplyr::mutate(model_name = "DINGO")
  )
  testthat::expect_equal(
    actual |> dplyr::filter(model_name == "MARTIN") |> dplyr::select(-irf_mode),
    get_irf(martin$M_, martin$oo_, "eps_martin", horizon = 4, pretty = TRUE) |>
      irf_normalize_pretty() |>
      dplyr::mutate(model_name = "MARTIN")
  )
})

testthat::test_that("irf_run_direct attributes an invalid model shock to the failing model", {
  models <- list(
    DINGO = dashboard_test_model("DINGO", shock_name = "eps_dingo", shock_description = "Monetary policy shock")
  )

  testthat::expect_error(
    irf_run_direct(models, c(DINGO = "not_a_real_shock"), horizon = 4),
    "does not provide displayed shock"
  )
})

testthat::test_that("irf_run_targeted combines targeted IRFs without cognitive discounting", {
  dingo <- dashboard_test_model("DINGO", shock_name = "eps_dingo", shock_description = "Monetary policy shock")
  martin <- dashboard_test_model("MARTIN", shock_name = "eps_martin", shock_description = "Monetary policy shock")
  models <- list(DINGO = dingo, MARTIN = martin)
  model_shocks <- c(DINGO = "eps_dingo", MARTIN = "eps_martin")

  actual <- irf_run_targeted(
    models,
    target_display_name = "Cash Rate",
    target_value = 1,
    model_shocks = model_shocks,
    horizon = 4,
    use_cd = FALSE,
    lambda = 0
  )

  testthat::expect_setequal(unique(actual$model_name), c("DINGO", "MARTIN"))
  testthat::expect_true(all(actual$irf_mode == "targeted"))
  testthat::expect_true(all(!actual$effective_use_cd))
})

testthat::test_that("irf_run_targeted falls back per model when cognitive discounting is unsupported", {
  dingo <- dashboard_test_model("DINGO", shock_name = "eps_r", shock_description = "Monetary policy shock", anticipated_shock = TRUE)
  martin <- dashboard_test_model("MARTIN", shock_name = "eps_r", shock_description = "Monetary policy shock", anticipated_shock = FALSE)
  models <- list(DINGO = dingo, MARTIN = martin)
  model_shocks <- c(DINGO = "eps_r", MARTIN = "eps_r")

  actual <- NULL
  testthat::expect_warning(
    actual <- irf_run_targeted(
      models,
      target_display_name = "Cash Rate",
      target_value = 1,
      model_shocks = model_shocks,
      horizon = 4,
      use_cd = TRUE,
      lambda = 0.8
    ),
    "does not support anticipated shock"
  )

  testthat::expect_true(actual$effective_use_cd[actual$model_name == "DINGO"][[1]])
  testthat::expect_false(actual$effective_use_cd[actual$model_name == "MARTIN"][[1]])
})

testthat::test_that("irf_run_targeted attributes a resolution failure to the failing model", {
  dingo <- dashboard_test_model("DINGO", shock_name = "eps_dingo", shock_description = "Monetary policy shock", include_gdp_growth = TRUE)
  martin <- dashboard_test_model("MARTIN", shock_name = "eps_martin", shock_description = "Monetary policy shock", include_gdp_growth = FALSE)
  models <- list(DINGO = dingo, MARTIN = martin)
  model_shocks <- c(DINGO = "eps_dingo", MARTIN = "eps_martin")

  testthat::expect_error(
    irf_run_targeted(
      models,
      target_display_name = "GDP Growth",
      target_value = 1,
      model_shocks = model_shocks,
      horizon = 4,
      use_cd = FALSE,
      lambda = 0
    ),
    "Custom targeted IRF failed for model `MARTIN`"
  )
})
