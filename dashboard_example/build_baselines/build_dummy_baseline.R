# Build a complete baseline of dummy data, inputs/dummy_baseline.xlsx, so the
# dashboard runs without the FRED data. It has the same variables, display
# names and units as build_us_baseline.R's FRED baseline, and the dates the
# FRED data would have today (worked out from today's date, without calling
# FRED): history from 1954Q3 to the last quarter with published GDP data, then
# a forecast to 48 quarters after it. The values are made up - smooth cycles
# around a steady state in the history, returning to it over the forecast -
# and are for illustration only.
#
# Run with the working directory set to dashboard_example/ (the folder with
# app.R): Rscript build_baselines/build_dummy_baseline.R
# It prints the matching timeline for usa_dashboard.R.

if (!file.exists("app.R")) stop("Set the working directory to dashboard_example/ (the folder containing app.R).", call. = FALSE)

output_path <- "inputs/dummy_baseline.xlsx"
forecast_quarters <- 48  # as in build_us_baseline.R
alt_paths_quarters <- 10 # as in build_us_baseline.R
return_quarters <- 12    # the forecast's return to steady state, in quarters

# 1. Dates, as the FRED data would have them today. A quarter's GDP is first
# published about a month after it ends; quarters are dated the first day of
# their last month.
published <- Sys.Date() - 35
publishing_quarter <- as.Date(sprintf("%s-%02d-01", format(published, "%Y"), 3 * ((as.integer(format(published, "%m")) + 2) %/% 3)))
last_data <- seq(publishing_quarter, by = "-1 quarter", length.out = 2)[[2]]
forecast_start <- seq(last_data, by = "quarter", length.out = 2)[[2]]
data_end <- seq(last_data, by = "quarter", length.out = forecast_quarters + 1)[[forecast_quarters + 1]]
dates <- seq(as.Date("1954-09-01"), data_end, by = "quarter")

# 2. Values in display units: the steady state matches usa_dashboard.R's
# Optimal Policy targets; unemployment moves against the cycle.
steady <- c(
  `Output Growth` = 0.5, `Consumption Growth` = 0.5, `Investment Growth` = 0.5, `Hours Worked` = 0,
  Inflation = 0.5, `Real Wage Growth` = 0.4, `Nominal Interest Rate` = 3, `Unemployment Rate` = 4.2
)
amplitude <- c(
  `Output Growth` = 0.5, `Consumption Growth` = 0.4, `Investment Growth` = 2, `Hours Worked` = 2,
  Inflation = 0.25, `Real Wage Growth` = 0.3, `Nominal Interest Rate` = 2, `Unemployment Rate` = -1.5
)
n_history <- sum(dates <= last_data)
cycle <- sin(2 * pi * seq_len(n_history) / 32)
glide <- pmax(0, 1 - seq_len(length(dates) - n_history) / return_quarters)

baseline <- data.frame(date = dates, check.names = FALSE)
for (name in names(steady)) {
  history <- steady[[name]] + amplitude[[name]] * cycle
  baseline[[name]] <- c(history, steady[[name]] + (history[[n_history]] - steady[[name]]) * glide)
}

writexl::write_xlsx(list(baseline = baseline), output_path)
message("Wrote ", output_path, ": ", format(dates[[1]]), " to ", format(data_end), ", data to ", format(last_data))

timeline <- list(
  data_start = dates[[1]],
  forecast_start = forecast_start,
  forecast_end = seq(forecast_start, by = "quarter", length.out = alt_paths_quarters)[[alt_paths_quarters]],
  data_end = data_end
)
cat(
  "\nThe matching timeline for usa_dashboard.R:\n",
  sprintf("timeline <- list(%s)\n\n", paste(sprintf("%s = as.Date(\"%s\")", names(timeline), vapply(timeline, format, "")), collapse = ", ")),
  sep = ""
)
