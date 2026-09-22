# Regenerate only after reviewing an intentional change to the model fixtures or
# IRF implementation. This script is never run as part of the test suite.
if (Sys.getenv("EZDYN_UPDATE_IRF_BASELINES") != "true") {
  stop("Set EZDYN_UPDATE_IRF_BASELINES=true to regenerate reviewed IRF test baselines.")
}

script_argument <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_argument[grepl("^--file=", script_argument)])
package_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
fixtures_path <- file.path(package_root, "tests", "testthat", "fixtures")
expected_path <- file.path(fixtures_path, "expected")

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The `pkgload` package is required to generate test baselines.")
}

pkgload::load_all(package_root, quiet = TRUE)

horizon <- 8L
target <- list(r_obs = c(0.1, 0.2))
shock_timing <- list(eps_r = 1:2)
gabaix_lambda <- 0.8

write_expected <- function(name, object) {
  saveRDS(object, file.path(expected_path, paste0(name, ".rds")))
}

dynare <- read_dynare(
  path = file.path(fixtures_path, "dynare", "test_model_dynare.json"),
  path_meta = file.path(fixtures_path, "dynare", "test_metadata_dynare.xlsx"),
  model_name = "Dynare fixture"
)

write_expected(
  "get-irf-dynare-raw",
  get_irf(dynare$M_, dynare$oo_, c("eps_r", "eps_psi"), horizon)
)
write_expected(
  "get-irf-dynare-wide",
  get_irf(dynare$M_, dynare$oo_, c("eps_r", "eps_psi"), horizon, data_frame = TRUE)
)
write_expected(
  "get-irf-dynare-pretty",
  get_irf(dynare$M_, dynare$oo_, c("eps_r", "eps_psi"), horizon, pretty = TRUE)
)

custom_irf <- utils::read.csv(
  file.path(fixtures_path, "irf", "test_model_irf.csv"),
  check.names = FALSE
)
moo <- read_moo(
  irf = custom_irf[, c("t", "shock", "value", "resp_var")],
  model_name = "IRF fixture",
  meta = file.path(fixtures_path, "irf", "test_metadata_irf.xlsx")
)

write_expected(
  "get-irf-custom-moo-raw",
  get_irf(moo$M_, moo$oo_, c("eps_r_1", "eps_r_2"), horizon)
)
write_expected(
  "get-irf-custom-moo-wide",
  get_irf(moo$M_, moo$oo_, c("eps_r_1", "eps_r_2"), horizon, data_frame = TRUE)
)
write_expected(
  "get-irf-custom-moo-pretty",
  get_irf(moo$M_, moo$oo_, c("eps_r_1", "eps_r_2"), horizon, pretty = TRUE)
)

write_expected(
  "get-irf-target-raw",
  get_irf_target(dynare$M_, dynare$oo_, horizon, target, shock_timing)
)
write_expected(
  "get-irf-target-pretty",
  get_irf_target(
    dynare$M_, dynare$oo_, horizon, target, shock_timing,
    pretty = TRUE,
    shock_nickname = "Targeted policy rate", 
    scale_targets = FALSE
  )
)
write_expected(
  "get-irf-target-gabaix-raw",
  get_irf_target_gabaix(
    dynare$M_, dynare$oo_, horizon, target, shock_timing, gabaix_lambda, 
    scale_targets = FALSE
  )
)
write_expected(
  "get-irf-target-gabaix-pretty",
  get_irf_target_gabaix(
    dynare$M_, dynare$oo_, horizon, target, shock_timing, gabaix_lambda,
    pretty = TRUE,
    shock_nickname = "Partially anticipated policy rate", 
    scale_targets = FALSE
  )
)