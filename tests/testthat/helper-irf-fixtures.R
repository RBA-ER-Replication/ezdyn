irf_fixture_horizon <- 8L
irf_fixture_tolerance <- 1e-6
irf_fixture_target <- list(r_obs = c(0.1, 0.2))
irf_fixture_shock_timing <- list(eps_r = 1:2)
irf_fixture_gabaix_lambda <- 0.8

irf_fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}

load_dynare_irf_fixture <- function() {
  read_dynare(
    path = irf_fixture_path("dynare", "test_model_dynare.json"),
    path_meta = irf_fixture_path("dynare", "test_metadata_dynare.xlsx"),
    model_name = "Dynare fixture"
  )
}

load_custom_moo_irf_fixture <- function() {
  irf <- utils::read.csv(
    irf_fixture_path("irf", "test_model_irf.csv"),
    check.names = FALSE
  )

  read_moo(
    irf = irf[, c("t", "shock", "value", "resp_var")],
    model_name = "IRF fixture",
    meta = irf_fixture_path("irf", "test_metadata_irf.xlsx")
  )
}

read_irf_expected <- function(name) {
  readRDS(irf_fixture_path("expected", paste0(name, ".rds")))
}