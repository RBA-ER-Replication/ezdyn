# Fetch the FRED data behind the US baseline and save it, unprocessed, to
# inputs/us_fred_raw.rds, so build_us_baseline.R can rebuild the baseline
# without calling FRED again. Re-run this after each data release or SEP.
#
# It fetches the data as known today and as known at the end of each of the
# previous `vintage_quarters` quarters (FRED's real-time data, ALFRED), each
# with:
#   history    - the Smets-Wouters (2007) source series and the unemployment
#                rate, quarterly averages (FRED dates quarters by their first
#                day)
#   sep        - the Summary of Economic Projections medians for each
#                projection year
#   longer_run - the SEP's longer-run medians
#
# Needs a FRED API key (https://fred.stlouisfed.org/docs/api/api_key.html) in
# the FRED_API_KEY environment variable. Keep it out of this repo: add the line
#   FRED_API_KEY=<your key>
# to your user-level ~/.Renviron (usethis::edit_r_environ()) and restart R.
#
# Run with the working directory set to dashboard_example/ (the folder with
# app.R): Rscript build_baselines/fetch_us_fred.R

if (!file.exists("app.R")) stop("Set the working directory to dashboard_example/ (the folder containing app.R).", call. = FALSE)

library(dplyr)
library(lubridate)
library(fredr)

if (!nzchar(Sys.getenv("FRED_API_KEY"))) {
  stop("FRED_API_KEY is not set - add it to your user-level ~/.Renviron (see header).", call. = FALSE)
}

output_path <- "inputs/us_fred_raw.rds"
vintage_quarters <- 12 # earliest Optimal Policy commitment start, in quarters back
as_of_today <- Sys.Date() - 1

history_ids <- c(
  gdp = "GDPC1",                # real GDP
  deflator = "GDPDEF",          # GDP implicit price deflator
  consumption = "PCEC",         # nominal personal consumption expenditures
  investment = "FPI",           # nominal fixed private investment
  population = "CNP16OV",       # civilian noninstitutional population, 16+
  employment = "CE16OV",        # civilian employment, 16+
  weekly_hours = "PRS85006023", # nonfarm business average weekly hours
  compensation = "COMPNFB",     # nonfarm business hourly compensation
  fed_funds = "FEDFUNDS",       # effective federal funds rate
  unemployment = "UNRATE"       # unemployment rate
)
# SEP medians: Q4/Q4 GDP growth and PCE inflation, Q4 unemployment and
# year-end fed funds rate. Each also has a longer-run series (ID + "LR").
sep_ids <- c(gdp = "GDPC1MD", inflation = "PCECTPIMD", unemployment = "UNRATEMD", fed_funds = "FEDTARMD")

# Fetch FRED series as known on `as_of` into one wide table, one column per
# named ID.
fetch <- function(ids, as_of, ...) {
  purrr::imap(ids, \(id, name) {
    fredr(id, ..., realtime_start = as_of, realtime_end = as_of) |>
      transmute(date, !!name := value)
  }) |>
    purrr::reduce(full_join, by = "date") |>
    arrange(date)
}

# Everything build_us_baseline.R needs, as known on `as_of`.
fetch_as_of <- function(as_of) {
  message("Fetching FRED data as of ", as_of)
  list(
    as_of = as_of,
    history = fetch(history_ids, as_of, frequency = "q", aggregation_method = "avg"),
    sep = fetch(sep_ids, as_of),
    longer_run = purrr::map(sep_ids, \(id) {
      fredr(paste0(id, "LR"), realtime_start = as_of, realtime_end = as_of) |> select(date, value)
    })
  )
}

current <- fetch_as_of(as_of_today)

# Vintages: the end of each of the `vintage_quarters` quarters before the
# first forecast quarter (the quarter after the last with every series).
# Each is dated, as in the dashboard, by its quarter's last month.
last_data <- max(tidyr::drop_na(current$history)$date) + months(2)
forecast_start <- last_data + months(3)
vintage_dates <- seq(forecast_start, by = "-1 quarter", length.out = vintage_quarters + 1)[-1]
vintages <- purrr::map(vintage_dates, \(vintage) {
  c(list(vintage = vintage), fetch_as_of(vintage + months(1) - 1))
})

saveRDS(list(fetched = Sys.time(), current = current, vintages = vintages), output_path)
message("Wrote ", output_path, ": data as of ", as_of_today, " and ", length(vintages), " vintages (", min(vintage_dates), " to ", max(vintage_dates), ")")
