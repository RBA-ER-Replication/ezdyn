# Read the real-time vintages (build_us_baseline.R's inputs/us_vintages.xlsx)
# for Optimal Policy's timeless commitment, or return `NULL`, so the dashboard
# runs with ordinary commitment, if there's no vintages workbook or it's out
# of date (its latest vintage isn't the quarter before `forecast_start`, so
# it was built for a different baseline).
us_read_vintages <- function(path, forecast_start) {
  if (!file.exists(path)) {
    message("No ", path, ", so Optimal Policy's Commitment is ordinary, not timeless, commitment. Build it with build_baselines/build_us_baseline.R.")
    return(NULL)
  }
  vintages <- openxlsx2::read_xlsx(path, sheet = "vintages")
  vintages$vintage <- as.Date(vintages$vintage)
  vintages$date <- as.Date(vintages$date)
  expected_last <- seq(forecast_start, by = "-1 quarter", length.out = 2)[[2]]
  if (max(vintages$vintage) != expected_last) {
    warning(
      path, " doesn't match the dashboard's timeline (its latest vintage is ", max(vintages$vintage),
      ", not ", expected_last, "), so it's ignored and Commitment is ordinary commitment. Rebuild it with",
      " build_baselines/build_us_baseline.R and update `timeline` in usa_dashboard.R.",
      call. = FALSE
    )
    return(NULL)
  }
  vintages
}