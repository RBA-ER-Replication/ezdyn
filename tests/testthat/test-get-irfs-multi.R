multi_model_fixture_models <- function() {
  list(
    Dynare = load_dynare_irf_fixture(),
    IRF = load_custom_moo_irf_fixture()
  )
}

# A custom-MOO fixture exposing only the unanticipated "eps_r" shock (no
# "eps_r_1"/"eps_r_2"/... anticipated codes), used to confirm
# `get_irfs_target_gabaix()` errors per model rather than silently falling
# back to an unanticipated IRF.
build_no_anticipated_shocks_model <- function() {
  irf <- utils::read.csv(irf_fixture_path("irf", "test_model_irf.csv"), check.names = FALSE)
  irf <- irf[irf$shock == "eps_r", ]
  read_moo(
    irf = irf[, c("t", "shock", "value", "resp_var")],
    model_name = "No anticipated shocks fixture",
    meta = irf_fixture_path("irf", "test_metadata_irf.xlsx")
  )
}

testthat::test_that("get_irfs combines pretty IRFs across models, keyed by the models list name", {
  models <- multi_model_fixture_models()

  combined <- get_irfs(models, shock_names = "eps_r", horizon = irf_fixture_horizon)

  expected_dynare <- get_irf(models$Dynare$M_, models$Dynare$oo_, "eps_r", irf_fixture_horizon, pretty = TRUE) |>
    dplyr::mutate(model_name = "Dynare")
  expected_irf <- get_irf(models$IRF$M_, models$IRF$oo_, "eps_r", irf_fixture_horizon, pretty = TRUE) |>
    dplyr::mutate(model_name = "IRF")

  testthat::expect_setequal(unique(combined$model_name), c("Dynare", "IRF"))
  testthat::expect_equal(combined |> dplyr::filter(model_name == "Dynare"), expected_dynare)
  testthat::expect_equal(combined |> dplyr::filter(model_name == "IRF"), expected_irf)
})

testthat::test_that("get_irfs with pretty = FALSE returns a named list of per-model results", {
  models <- multi_model_fixture_models()

  actual <- get_irfs(models, shock_names = "eps_r", horizon = irf_fixture_horizon, pretty = FALSE)

  testthat::expect_named(actual, c("Dynare", "IRF"))
  testthat::expect_equal(
    actual$Dynare,
    get_irf(models$Dynare$M_, models$Dynare$oo_, "eps_r", irf_fixture_horizon, pretty = FALSE)
  )
  testthat::expect_equal(
    actual$IRF,
    get_irf(models$IRF$M_, models$IRF$oo_, "eps_r", irf_fixture_horizon, pretty = FALSE)
  )
})

testthat::test_that("get_irfs hard-stops and attributes a resolution failure to the failing model", {
  models <- multi_model_fixture_models()

  testthat::expect_error(
    get_irfs(models, shock_names = "eps_r_star", horizon = irf_fixture_horizon),
    "get_irfs\\(\\) failed for model `IRF`"
  )
})

testthat::test_that("get_irfs_target combines targeted IRFs, resolved independently for each model", {
  models <- multi_model_fixture_models()

  combined <- get_irfs_target(
    models,
    horizon = irf_fixture_horizon,
    target = irf_fixture_target,
    shock_timing = irf_fixture_shock_timing,
    shock_nickname = "Targeted policy rate",
    scale_targets = FALSE
  )

  expected_dynare <- get_irf_target(
    models$Dynare$M_, models$Dynare$oo_, irf_fixture_horizon, irf_fixture_target, irf_fixture_shock_timing,
    pretty = TRUE, shock_nickname = "Targeted policy rate", scale_targets = FALSE
  ) |> dplyr::mutate(model_name = "Dynare")
  expected_irf <- get_irf_target(
    models$IRF$M_, models$IRF$oo_, irf_fixture_horizon, irf_fixture_target, irf_fixture_shock_timing,
    pretty = TRUE, shock_nickname = "Targeted policy rate", scale_targets = FALSE
  ) |> dplyr::mutate(model_name = "IRF")

  testthat::expect_setequal(unique(combined$model_name), c("Dynare", "IRF"))
  testthat::expect_equal(combined |> dplyr::filter(model_name == "Dynare"), expected_dynare)
  testthat::expect_equal(combined |> dplyr::filter(model_name == "IRF"), expected_irf)
})

testthat::test_that("get_irfs_target with pretty = FALSE returns a named list of per-model results", {
  models <- multi_model_fixture_models()

  actual <- get_irfs_target(
    models,
    horizon = irf_fixture_horizon,
    target = irf_fixture_target,
    shock_timing = irf_fixture_shock_timing,
    scale_targets = FALSE,
    pretty = FALSE
  )

  testthat::expect_named(actual, c("Dynare", "IRF"))
  testthat::expect_equal(
    actual$Dynare,
    get_irf_target(
      models$Dynare$M_, models$Dynare$oo_, irf_fixture_horizon, irf_fixture_target, irf_fixture_shock_timing,
      scale_targets = FALSE
    )
  )
  testthat::expect_equal(
    actual$IRF,
    get_irf_target(
      models$IRF$M_, models$IRF$oo_, irf_fixture_horizon, irf_fixture_target, irf_fixture_shock_timing,
      scale_targets = FALSE
    )
  )
})

testthat::test_that("get_irfs_target lets the same model be compared twice under different labels", {
  dynare <- load_dynare_irf_fixture()
  models <- list(DINGO = dynare, Comparison = dynare)

  combined <- get_irfs_target(
    models,
    horizon = irf_fixture_horizon,
    target = irf_fixture_target,
    shock_timing = irf_fixture_shock_timing,
    scale_targets = FALSE
  )

  testthat::expect_setequal(unique(combined$model_name), c("DINGO", "Comparison"))
  testthat::expect_equal(
    combined |> dplyr::filter(model_name == "DINGO") |> dplyr::select(-model_name),
    combined |> dplyr::filter(model_name == "Comparison") |> dplyr::select(-model_name)
  )
})

testthat::test_that("get_irfs_target attributes a resolution failure to the failing model", {
  models <- multi_model_fixture_models()

  testthat::expect_error(
    get_irfs_target(
      models,
      horizon = irf_fixture_horizon,
      target = list(gdp_growth = c(0.1, 0.2)),
      shock_timing = irf_fixture_shock_timing,
      scale_targets = FALSE
    ),
    "get_irfs_target\\(\\) failed for model `IRF`"
  )
})

testthat::test_that("get_irfs_target_gabaix combines partially anticipated targeted IRFs across models", {
  models <- multi_model_fixture_models()

  combined <- get_irfs_target_gabaix(
    models,
    horizon = irf_fixture_horizon,
    target = irf_fixture_target,
    shock_timing = irf_fixture_shock_timing,
    lambda = irf_fixture_gabaix_lambda,
    shock_nickname = "Partially anticipated policy rate",
    scale_targets = FALSE
  )

  expected_dynare <- get_irf_target_gabaix(
    models$Dynare$M_, models$Dynare$oo_, irf_fixture_horizon, irf_fixture_target, irf_fixture_shock_timing,
    irf_fixture_gabaix_lambda, pretty = TRUE, shock_nickname = "Partially anticipated policy rate", scale_targets = FALSE
  ) |> dplyr::mutate(model_name = "Dynare")
  expected_irf <- get_irf_target_gabaix(
    models$IRF$M_, models$IRF$oo_, irf_fixture_horizon, irf_fixture_target, irf_fixture_shock_timing,
    irf_fixture_gabaix_lambda, pretty = TRUE, shock_nickname = "Partially anticipated policy rate", scale_targets = FALSE
  ) |> dplyr::mutate(model_name = "IRF")

  testthat::expect_equal(combined |> dplyr::filter(model_name == "Dynare"), expected_dynare)
  testthat::expect_equal(combined |> dplyr::filter(model_name == "IRF"), expected_irf)
  testthat::expect_true(all(combined$gabaix_lambda == irf_fixture_gabaix_lambda))
})

testthat::test_that("get_irfs_target_gabaix has no automatic fallback and errors per model when anticipated shocks are unavailable", {
  models <- list(
    Dynare = load_dynare_irf_fixture(),
    NoAnticipated = build_no_anticipated_shocks_model()
  )

  testthat::expect_error(
    get_irfs_target_gabaix(
      models,
      horizon = irf_fixture_horizon,
      target = irf_fixture_target,
      shock_timing = irf_fixture_shock_timing,
      lambda = irf_fixture_gabaix_lambda,
      scale_targets = FALSE
    ),
    "get_irfs_target_gabaix\\(\\) failed for model `NoAnticipated`"
  )
})

testthat::test_that("get_irfs/get_irfs_target/get_irfs_target_gabaix validate `models` like get_alt_paths", {
  testthat::expect_error(
    get_irfs(list(), shock_names = "eps_r", horizon = irf_fixture_horizon),
    "non-empty named list"
  )
  testthat::expect_error(
    get_irfs_target(
      list(load_dynare_irf_fixture()),
      horizon = irf_fixture_horizon, target = irf_fixture_target, shock_timing = irf_fixture_shock_timing
    ),
    "unique, non-empty names"
  )
})

