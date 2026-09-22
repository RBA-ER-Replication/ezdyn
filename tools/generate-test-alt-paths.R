# Regenerate only after reviewing an intentional change to the model fixtures or
# IRF implementation. This script is never run as part of the test suite.
if (Sys.getenv("EZDYN_UPDATE_ALT_PATHS") != "true") {
  stop("Set EZDYN_UPDATE_ALT_PATHS=true to regenerate reviewed alternative test paths.")
}

script_argument <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_argument[grepl("^--file=", script_argument)])
package_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
fixtures_path <- file.path(package_root, "tests", "testthat", "fixtures")
expected_path <- file.path(fixtures_path, "expected")

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The `pkgload` package is required to generate test alternative paths.")
}

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("The `readxl` package is required to read xlsx test fixtures.")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("The `dplyr` package is required to process xlsx test fixtures.")
}

pkgload::load_all(package_root, quiet = TRUE)

forecast_start <- as.Date("2024-03-01")
forecast_end <- as.Date("2025-12-01")
data_start <- as.Date("1992-03-01")
use_cd <- TRUE
gabaix_lambda <- 0.8

write_expected <- function(name, object) {
  saveRDS(object, file.path(expected_path, paste0(name, ".rds")))
}

dynare <- read_dynare(
  path = file.path(fixtures_path, "dynare", "test_model_dynare.json"),
  path_meta = file.path(fixtures_path, "dynare", "test_metadata_dynare.xlsx"),
  model_name = "Dynare fixture"
)
models <- list(DINGO = dynare)

raw_baseline <- readxl::read_xlsx(
  file.path(fixtures_path, "alt_paths", "test_data_matrix.xlsx")
) |>
  dplyr::mutate(date = as.Date(date))
baseline <- import_baseline(
  input = raw_baseline,
  source_model = dynare,
  models = models
)

alt_paths <- readxl::read_xlsx(
  file.path(fixtures_path, "alt_paths", "test_supplied_alt_path.xlsx")
) |>
  dplyr::mutate(date = as.Date(Date)) |>
  dplyr::select(-Date)

paths <- get_alt_paths(
  alt_paths = alt_paths,
  models = models,
  baseline = baseline,
  use_cd = use_cd,
  lambda = gabaix_lambda,
  instrument_shock_name = "eps_r",
  forecast_start = forecast_start,
  forecast_end = forecast_end,
  data_start = data_start
)

write_expected(
  "get-alt-path",
  paths
)

write_expected(
    "get-alt-paths-long-format",
    format_alt_paths_long(
      paths,
      baseline_name_value = "Baseline",
      forecasts_start_date = forecast_start,
      output_start_date = data_start,
      output_end_date = forecast_end
    )
)
