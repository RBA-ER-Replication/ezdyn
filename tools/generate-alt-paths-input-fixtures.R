# Regenerate only after reviewing an intentional change to the alt-paths input
# fixtures. This script is never run as part of the test suite.
#
# The two workbooks below hold synthetic placeholder series (smooth
# deterministic curves, not observed or forecast data) so the package ships no
# real historical or forecast economic data alongside its tests. Column names
# and date ranges match what `get_alt_paths()`/`import_baseline()` require:
# a complete quarterly `date` sequence and finite numeric series.
if (Sys.getenv("EZDYN_UPDATE_ALT_PATHS_INPUTS") != "true") {
  stop("Set EZDYN_UPDATE_ALT_PATHS_INPUTS=true to regenerate the alt-paths input fixtures.")
}

script_argument <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_argument[grepl("^--file=", script_argument)])
package_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
fixtures_path <- file.path(package_root, "tests", "testthat", "fixtures", "alt_paths")

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("The `writexl` package is required to generate the alt-paths input fixtures.")
}

# 1. Baseline data matrix: quarterly synthetic series, 1992 Q1 to 2025 Q4
# (matches the date range the existing alt-paths/baseline-import tests use).
dates <- seq(as.Date("1992-03-01"), as.Date("2025-12-01"), by = "quarter")
t <- seq_along(dates) - 1

baseline <- data.frame(
  date = dates,
  gdp_growth = round(2.5 + 1 * sin(2 * pi * t / 16 + 2), 2),
  infl_obs = round(2 + 1.5 * sin(2 * pi * t / 20 + 1), 2),
  r_obs = round(pmax(0.1, 5 + 3 * sin(2 * pi * t / 40) - 0.01 * t), 2),
  unemp1 = round(pmax(3, 6 + 1.5 * sin(2 * pi * t / 30 + 3)), 2)
)

writexl::write_xlsx(baseline, file.path(fixtures_path, "test_data_matrix.xlsx"))

# 2. Supplied alternative path: one quarterly cash-rate scenario over the
# fixtures' forecast horizon, 2024 Q1 to 2025 Q4.
alt_dates <- seq(as.Date("2024-03-01"), as.Date("2025-12-01"), by = "quarter")
alt_baseline_rate <- baseline$r_obs[baseline$date %in% alt_dates]

alt_path <- data.frame(
  Date = alt_dates,
  `Test Path` = round(alt_baseline_rate - 0.25 * seq_along(alt_dates), 2),
  check.names = FALSE
)

writexl::write_xlsx(alt_path, file.path(fixtures_path, "test_supplied_alt_path.xlsx"))

cat("Wrote synthetic alt-paths input fixtures to", fixtures_path, "\n")
