# Build the USA dashboard's inputs from the FRED data saved by fetch_us_fred.R
# (inputs/us_fred_raw.rds): history for the seven Smets-Wouters (2007)
# observables and the unemployment rate, extended with the Fed's Summary of
# Economic Projections (SEP) medians. See README.md for the data definitions
# and forecast assumptions. It doesn't call FRED, so it can be re-run freely
# after changes to the processing. It writes two workbooks (neither tracked
# in git):
#   inputs/us_baseline.xlsx - the data
#   inputs/us_vintages.xlsx - real-time vintages for Optimal Policy's timeless
#                             commitment (optional)
# and prints the matching timeline and loss-function targets, ready to paste
# into usa_dashboard.R.
#
# Everything is built in the dashboard's display names and units (as in
# inputs/variable_metadata.xlsx), except `model_data`, which converts to
# Smets-Wouters' own Dynare names and definitions.
#
# us_baseline.xlsx sheets:
#   baseline   - the dashboard's baseline, as close to the published data as
#                the model's units allow: headline (not per person) growth,
#                hours per person relative to their level once unemployment
#                reaches the SEP longer-run rate (the model's steady state),
#                the fed funds rate, and the unemployment rate (for Gali
#                Unemployment (2010))
#   model_data - Smets-Wouters' own data definitions (per person growth,
#                hours demeaned over the history, the quarterly rate `robs`),
#                for re-running the model over current data and forecasts
#
# Printed for usa_dashboard.R:
#   timeline   - data_start, forecast_start, forecast_end (the end of the
#                Alternative Paths window, `alt_paths_quarters` after
#                forecast_start) and data_end (the end of the data, including
#                the longer-run extension), as ezdyn::configure_dashboard()
#                uses them
#   targets    - the current loss-function targets (2% a year inflation, the
#                latest SEP longer-run unemployment rate and GDP growth, no
#                rate change), the Optimal Policy tab's default targets
#
# us_vintages.xlsx sheets (optional: without it, the dashboard's Optimal
# Policy tab uses ordinary rather than timeless commitment):
#   vintage_levels - `baseline` as known at the end of each of the previous
#                `vintage_quarters` quarters (data to the quarter before, plus
#                that quarter's SEP), with that vintage's loss-function
#                targets (`<display name> target`). `vintage` is the quarter
#                it was made in (its first forecast quarter)
#   vintages   - the loss-function gaps in each vintage, for Optimal Policy's
#                timeless commitment: one `<loss variable>_gap_loss` column per
#                loss variable, the gap from its target as it stood at the
#                time, plus the nominal rate's level for the ELB
#
# Dates follow the dashboard convention: a quarter is dated the first day of
# its last month (Q3 = YYYY-09-01).
#
# Run with the working directory set to dashboard_example/ (the folder with
# app.R), after build_baselines/fetch_us_fred.R:
#   Rscript build_baselines/build_us_baseline.R

if (!file.exists("app.R")) stop("Set the working directory to dashboard_example/ (the folder containing app.R).", call. = FALSE)

library(dplyr)
library(lubridate)

fred_path <- "inputs/us_fred_raw.rds"
output_path <- "inputs/us_baseline.xlsx"
vintages_path <- "inputs/us_vintages.xlsx"
forecast_quarters <- 48 # data horizon, long enough for the Optimal Policy tab
alt_paths_quarters <- 10 # the dashboard's forecast: one Alternative Paths slider per quarter
inflation_target <- 2   # the Fed's inflation target, per cent a year

if (!file.exists(fred_path)) {
  stop(fred_path, " doesn't exist - run build_baselines/fetch_us_fred.R first.", call. = FALSE)
}
fred <- readRDS(fred_path)

# Quarterly log growth x 100.
dlog <- function(x) 100 * c(NA, diff(log(x)))

# Quarterly log growth x 100 at `annual` per cent a year.
quarterly_growth <- function(annual) 100 * log1p(annual / 100) / 4

# Data and SEP forecasts from one fetch (`fred`, as known on `fred$as_of`; see
# fetch_us_fred.R), running to `end` (by default `forecast_quarters` after the
# last data), in display names and units: headline growth rates (quarterly
# log changes x 100), 100 x log hours per person, the fed funds rate and the
# unemployment rate (per cent), plus population growth for `model_data`'s
# per-person growth.
build_us_data <- function(fred, end = NULL) {
  # 1. History ------------------------------------------------------------
  # Smets & Wouters (2007) source series and the unemployment rate, monthly
  # series averaged to quarterly, each quarter re-dated by its last month.
  # The sample runs over every quarter where all seven observables are
  # available.
  raw <- fred$history |>
    mutate(date = date + months(2))

  history <- raw |>
    transmute(
      date,
      `Output Growth` = dlog(gdp),
      `Consumption Growth` = dlog(consumption / deflator),
      `Investment Growth` = dlog(investment / deflator),
      `Hours Worked` = 100 * log(weekly_hours * employment / population),
      Inflation = dlog(deflator),
      `Real Wage Growth` = dlog(compensation / deflator),
      `Nominal Interest Rate` = fed_funds,
      `Unemployment Rate` = unemployment,
      `Population Growth` = dlog(population)
    ) |>
    tidyr::drop_na()

  raw <- filter(raw, date %in% history$date)
  data_end <- max(history$date)
  last_q4 <- max(history$date[month(history$date) == 12])

  # 2. Fed forecasts ------------------------------------------------------
  # SEP medians: Q4/Q4 GDP growth and PCE inflation, Q4 unemployment and
  # year-end fed funds rate for each projection year. Longer-run values
  # apply from the year after the last projection year.
  sep <- fred$sep |>
    tidyr::drop_na() |>
    mutate(year = year(date), .keep = "unused")
  projection_end <- make_date(max(sep$year), 12, 1)
  longer_run <- purrr::map(fred$longer_run, \(series) last(na.omit(series$value)))
  sep <- bind_rows(sep, tibble(year = max(sep$year) + 1, !!!longer_run))

  # 3. Forecast -----------------------------------------------------------
  # Paths run linearly from the last observation through each year's Q4 SEP
  # anchor, so growth in the current year makes up the remainder of the
  # Fed's Q4/Q4 projection. Assumptions for series the SEP doesn't cover:
  #   - GDP deflator inflation follows the PCE inflation projection
  #   - consumption and investment grow in line with output, and real wages
  #     with output per person (balanced growth, as in the model's steady
  #     state)
  #   - population grows at its latest year-ended rate
  #   - hours per person move with the employment rate (1 - unemployment
  #     rate)
  forecast_dates <- if (is.null(end)) {
    seq(data_end, by = "quarter", length.out = forecast_quarters + 1)[-1]
  } else {
    seq(data_end, end, by = "quarter")[-1]
  }
  anchor_years <- seq(year(last_q4) + 1, year(max(forecast_dates)))
  anchors <- sep[match(pmin(anchor_years, max(sep$year)), sep$year), ]
  if (anyNA(anchors)) stop("The SEP as of ", fred$as_of, " does not cover ", year(last_q4) + 1, ".", call. = FALSE)

  quarter_index <- function(date) 4 * year(date) + quarter(date)

  to_anchors <- function(from, targets) {
    approx(c(quarter_index(data_end), 4 * anchor_years + 4), c(from, targets), xout = quarter_index(forecast_dates))$y
  }

  # Quarterly log growth (x100) implied by Q4/Q4 growth targets.
  growth_path <- function(level, q4q4) {
    log_level <- 100 * log(level)
    targets <- log_level[raw$date == last_q4] + cumsum(100 * log1p(q4q4 / 100))
    diff(c(last(log_level), to_anchors(last(log_level), targets)))
  }

  unemployment <- to_anchors(last(raw$unemployment), anchors$unemployment)

  forecast <- tibble(
    date = forecast_dates,
    `Output Growth` = growth_path(raw$gdp, anchors$gdp),
    `Consumption Growth` = `Output Growth`,
    `Investment Growth` = `Output Growth`,
    `Hours Worked` = last(history$`Hours Worked`) + 100 * log((100 - unemployment) / (100 - last(raw$unemployment))),
    Inflation = growth_path(raw$deflator, anchors$inflation),
    `Population Growth` = 100 * diff(log(tail(raw$population, 5)), lag = 4) / 4,
    `Real Wage Growth` = `Output Growth` - `Population Growth`,
    `Nominal Interest Rate` = to_anchors(last(raw$fed_funds), anchors$fed_funds),
    `Unemployment Rate` = unemployment
  )

  list(
    data = bind_rows(history, forecast),
    forecast_start = min(forecast_dates),
    projection_end = projection_end,
    # The Fed's NAIRU and potential growth estimates as known on `as_of`: the
    # SEP's longer-run unemployment rate and GDP growth (per cent a year)
    # medians.
    nairu = longer_run$unemployment,
    potential_growth = longer_run$gdp
  )
}

# Dashboard units: headline growth, hours per person relative to their level
# once unemployment reaches its longer-run rate (the end of the data).
headline <- function(data) {
  data |>
    mutate(`Hours Worked` = `Hours Worked` - last(`Hours Worked`)) |>
    select(-`Population Growth`)
}

# One build's dashboard data, with its loss-function targets as known then
# (`<display name> target`): the Fed's 2% a year inflation target (0.5% a
# quarter), and its NAIRU and potential growth (as quarterly and year-ended
# log growth x 100, each rounded to 2 significant figures) estimates.
dashboard_levels <- function(us) {
  headline(us$data) |>
    mutate(
      `Inflation target` = inflation_target / 4,
      `Unemployment Rate target` = us$nairu,
      `Output Growth target` = signif(quarterly_growth(us$potential_growth), 2),
      `Output Growth (year-ended) target` = signif(4 * quarterly_growth(us$potential_growth), 2)
    )
}

# 4. Current data ---------------------------------------------------------
current <- build_us_data(fred$current)
data_end <- max(current$data$date)

# Smets-Wouters' own definitions and Dynare names: per-person growth, hours
# demeaned over the history, and the quarterly rate `robs` (fed funds / 4).
model_data <- current$data |>
  transmute(
    date,
    dy = `Output Growth` - `Population Growth`,
    dc = `Consumption Growth` - `Population Growth`,
    dinve = `Investment Growth` - `Population Growth`,
    labobs = `Hours Worked` - mean(`Hours Worked`[date < current$forecast_start]),
    pinfobs = Inflation,
    dw = `Real Wage Growth`,
    robs = `Nominal Interest Rate` / 4
  )

timeline <- list(
  data_start = min(current$data$date),
  forecast_start = current$forecast_start,
  forecast_end = seq(current$forecast_start, by = "quarter", length.out = alt_paths_quarters)[[alt_paths_quarters]],
  data_end = data_end
)

# Loss-function gaps for one vintage, named `<display name>_gap_loss` as
# ezdyn::get_policy_scenario() expects: each variable less its target at the
# time (from `dashboard_levels()`).
#   - Inflation: less 0.5% a quarter (2% a year)
#   - Nominal Interest Rate Changes: the quarterly change in the rate,
#     targeting no change
#   - Unemployment Rate: less the Fed's NAIRU estimate, in percentage points.
#     Used by Gali Unemployment (2010)
#   - Output Growth: less the Fed's potential growth estimate
#   - Output Growth (year-ended): the sum of the last four quarters' Output
#     Growth (as the dashboard derives it), less the potential growth
#     estimate over a year
# The rate itself (`Nominal Interest Rate`) is kept for the ELB constraint.
# `levels` must start at least three quarters before `vintage`, so the first
# rate change and year-ended growth are defined.
loss_gaps <- function(levels, vintage) {
  levels |>
    transmute(
      date,
      `Inflation_gap_loss` = Inflation - `Inflation target`,
      `Nominal Interest Rate Changes_gap_loss` = `Nominal Interest Rate` - lag(`Nominal Interest Rate`),
      `Unemployment Rate_gap_loss` = `Unemployment Rate` - `Unemployment Rate target`,
      `Output Growth_gap_loss` = `Output Growth` - `Output Growth target`,
      `Output Growth (year-ended)_gap_loss` = `Output Growth` + lag(`Output Growth`) + lag(`Output Growth`, 2) +
        lag(`Output Growth`, 3) - `Output Growth (year-ended) target`,
      `Nominal Interest Rate`
    ) |>
    filter(date >= vintage) |>
    mutate(vintage, .before = 1)
}

# The current data and loss-function targets. The targets are the Optimal
# Policy tab's default targets, so the current quarter's gaps match the
# vintages'.
current_levels <- dashboard_levels(current)
targets <- tibble(
  variable = c("Inflation", "Unemployment Rate", "Output Growth", "Output Growth (year-ended)", "Nominal Interest Rate Changes"),
  target = c(
    current_levels$`Inflation target`[[1]], current_levels$`Unemployment Rate target`[[1]],
    current_levels$`Output Growth target`[[1]], current_levels$`Output Growth (year-ended) target`[[1]], 0
  )
)

writexl::write_xlsx(list(baseline = select(current_levels, -ends_with(" target")), model_data = model_data), output_path)
message("Wrote ", output_path)
cat(
  "\nThe matching timeline and targets for usa_dashboard.R:\n",
  sprintf("timeline <- list(%s)\n", paste(sprintf("%s = as.Date(\"%s\")", names(timeline), vapply(timeline, format, "")), collapse = ", ")),
  sprintf("targets <- c(%s)\n\n", paste(sprintf("`%s` = %s", targets$variable, signif(targets$target, 3)), collapse = ", ")),
  sep = ""
)

# 6. Vintages -------------------------------------------------------------
# A separate workbook, only needed for Optimal Policy's timeless commitment:
# without it the dashboard still runs, with ordinary commitment from the
# forecast start. Each vintage (fetched as known at the end of its quarter)
# runs from its own quarter to the current `data_end`, covering every loss
# horizon Optimal Policy allows. Each holds its `baseline` and the targets
# its losses are measured from.
if (length(fred$vintages) == 0) {
  message("No vintages in ", fred_path, ", so ", vintages_path, " wasn't written (delete any older copy).")
} else {
  vintage_data <- purrr::map(fred$vintages, \(v) {
    list(vintage = v$vintage, levels = dashboard_levels(build_us_data(v, data_end)))
  })
  vintage_levels <- purrr::map(vintage_data, \(v) {
    v$levels |> filter(date >= v$vintage) |> mutate(vintage = v$vintage, .before = 1)
  }) |>
    bind_rows()
  vintages <- purrr::map(vintage_data, \(v) loss_gaps(v$levels, v$vintage)) |>
    bind_rows()
  if (anyNA(vintages)) stop("The loss-function vintages are incomplete.", call. = FALSE)
  writexl::write_xlsx(list(vintages = vintages, vintage_levels = vintage_levels), vintages_path)
  message("Wrote ", vintages_path, ": ", length(vintage_data), " vintages, ", min(vintages$vintage), " to ", max(vintages$vintage))
}
