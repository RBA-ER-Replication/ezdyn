# Build US data for the dashboard (inputs/us_baseline.xlsx) from FRED:
# history for the seven Smets-Wouters (2007) observables, extended with the
# Fed's Summary of Economic Projections (SEP) medians. See README.md for the
# data definitions and forecast assumptions.
#
# Output sheets, with the observables under their Dynare names:
#   baseline   - the dashboard's baseline, as close to the published data as
#                the model's units allow: headline (not per person) growth,
#                and hours per person relative to their level once
#                unemployment reaches the SEP longer-run rate (the model's
#                steady state). Also the unemployment rate (`unemployment`,
#                per cent), the dashboard's "Unemployment Rate" for Gali
#                Unemployment (2010)
#   model_data - Smets-Wouters' own data definitions (per person growth,
#                hours demeaned over the history), for re-running the model
#                over current data and forecasts
#   timeline   - data_start, data_end, forecast_start, forecast_end, as used by
#                ezdyn::configure_dashboard() (data_end is the end of the
#                data, including the longer-run extension)
#   targets    - the current loss-function targets in display units (2% a year
#                inflation, the latest SEP longer-run unemployment rate and
#                GDP growth, no rate change), the Optimal Policy tab's default
#                targets
#   vintage_levels - `baseline` as known at the end of each of the previous
#                `vintage_quarters` quarters (data to the quarter before, plus
#                that quarter's SEP), with the unemployment rate and that
#                SEP's longer-run unemployment rate (`nairu`) and GDP growth
#                (`potential_growth`, per cent a year). `vintage` is the
#                quarter it was made in (its first forecast quarter)
#   vintages   - the loss-function gaps in each vintage, for Optimal Policy's
#                timeless commitment: one `<loss variable>_gap_loss` column per
#                loss variable (named by its dashboard display name, in
#                dashboard units), the gap from its target as it stood at the
#                time, plus the nominal rate's level for the ELB
#
# Dates follow the dashboard convention: a quarter is dated the first day of
# its last month (Q3 = YYYY-09-01).
#
# Needs a FRED API key (https://fred.stlouisfed.org/docs/api/api_key.html) in
# the FRED_API_KEY environment variable. Keep it out of this repo: add the line
#   FRED_API_KEY=<your key>
# to your user-level ~/.Renviron (usethis::edit_r_environ()) and restart R.
#
# Run from this directory: Rscript build_us_baseline.R

library(dplyr)
library(lubridate)
library(fredr)

if (!nzchar(Sys.getenv("FRED_API_KEY"))) {
  stop("FRED_API_KEY is not set - add it to your user-level ~/.Renviron (see header).", call. = FALSE)
}

output_path <- "inputs/us_baseline.xlsx"
forecast_quarters <- 48 # data horizon, long enough for the Optimal Policy tab
vintage_quarters <- 12  # earliest Optimal Policy commitment start, in quarters back
inflation_target <- 2   # the Fed's inflation target, per cent a year

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

dlog <- function(x) 100 * c(NA, diff(log(x)))

# Data and SEP forecasts as known on `as_of`, running to `end` (by default
# `forecast_quarters` after the last data). `dy`, `dc` and `dinve` are headline
# growth, with population growth in `dpop`; `labobs` is 100 x log hours per
# person.
build_us_data <- function(as_of, end = NULL) {
  # 1. History ------------------------------------------------------------
  # Smets & Wouters (2007) source series, monthly series averaged to
  # quarterly. The sample runs over every quarter where all seven
  # observables are available.
  raw <- fetch(
    c(
      gdp = "GDPC1",                # real GDP
      deflator = "GDPDEF",          # GDP implicit price deflator
      consumption = "PCEC",         # nominal personal consumption expenditures
      investment = "FPI",           # nominal fixed private investment
      population = "CNP16OV",       # civilian noninstitutional population, 16+
      employment = "CE16OV",        # civilian employment, 16+
      weekly_hours = "PRS85006023", # nonfarm business average weekly hours
      compensation = "COMPNFB",     # nonfarm business hourly compensation
      fed_funds = "FEDFUNDS",       # effective federal funds rate
      unemployment = "UNRATE"       # links hours to the SEP unemployment path
    ),
    as_of,
    frequency = "q",
    aggregation_method = "avg"
  ) |>
    mutate(date = date + months(2))

  history <- raw |>
    transmute(
      date,
      dy = dlog(gdp),
      dc = dlog(consumption / deflator),
      dinve = dlog(investment / deflator),
      labobs = 100 * log(weekly_hours * employment / population),
      pinfobs = dlog(deflator),
      dw = dlog(compensation / deflator),
      robs = fed_funds / 4,
      dpop = dlog(population)
    ) |>
    tidyr::drop_na()

  raw <- filter(raw, date %in% history$date)
  data_end <- max(history$date)
  last_q4 <- max(history$date[month(history$date) == 12])

  # 2. Fed forecasts ------------------------------------------------------
  # SEP medians: Q4/Q4 GDP growth and PCE inflation, Q4 unemployment and
  # year-end fed funds rate for each projection year. Longer-run values
  # apply from the year after the last projection year.
  sep_ids <- c(gdp = "GDPC1MD", inflation = "PCECTPIMD", unemployment = "UNRATEMD", fed_funds = "FEDTARMD")
  sep <- fetch(sep_ids, as_of) |>
    tidyr::drop_na() |>
    mutate(year = year(date), .keep = "unused")
  projection_end <- make_date(max(sep$year), 12, 1)
  longer_run <- purrr::map(sep_ids, \(id) {
    last(na.omit(fredr(paste0(id, "LR"), realtime_start = as_of, realtime_end = as_of)$value))
  })
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
  if (anyNA(anchors)) stop("The SEP as of ", as_of, " does not cover ", year(last_q4) + 1, ".", call. = FALSE)

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
    dy = growth_path(raw$gdp, anchors$gdp),
    dc = dy,
    dinve = dy,
    labobs = last(history$labobs) + 100 * log((100 - unemployment) / (100 - last(raw$unemployment))),
    pinfobs = growth_path(raw$deflator, anchors$inflation),
    dpop = 100 * diff(log(tail(raw$population, 5)), lag = 4) / 4,
    dw = dy - dpop,
    robs = to_anchors(last(raw$fed_funds), anchors$fed_funds) / 4
  )

  list(
    data = bind_rows(history, forecast),
    forecast_start = min(forecast_dates),
    projection_end = projection_end,
    # The unemployment rate (history, then the path to the SEP anchors), and
    # the Fed's NAIRU estimate as known on `as_of`: the SEP's longer-run
    # unemployment median. Not SW observables, so kept out of `data`.
    unemployment = tibble(date = c(history$date, forecast_dates), unemployment = c(raw$unemployment, unemployment)),
    nairu = longer_run$unemployment,
    # The Fed's estimate of potential output growth as known on `as_of`: the
    # SEP's longer-run GDP growth median, per cent a year.
    potential_growth = longer_run$gdp
  )
}

# Dashboard units: headline growth, hours per person relative to their level
# once unemployment reaches its longer-run rate (the end of the data).
headline <- function(data) {
  data |>
    mutate(labobs = labobs - last(labobs)) |>
    select(-dpop)
}

# 4. Current data ---------------------------------------------------------
current <- build_us_data(Sys.Date())
data_end <- max(current$data$date)

model_data <- current$data |>
  mutate(
    across(c(dy, dc, dinve), \(x) x - dpop),
    labobs = labobs - mean(labobs[date < current$forecast_start])
  ) |>
  select(-dpop)

timeline <- tibble(
  data_start = min(current$data$date),
  data_end = data_end,
  forecast_start = current$forecast_start,
  forecast_end = current$projection_end
)

# Loss-function gaps for one vintage, in the dashboard's display units (see
# inputs/variable_metadata.xlsx), named `<display name>_gap_loss` as
# ezdyn::get_policy_scenario() expects. Each is the variable minus its target
# at the time:
#   - Inflation: quarterly GDP deflator inflation less the 2% a year target
#     (0.5% a quarter)
#   - Nominal Interest Rate Changes: the quarterly change in the annualised
#     fed funds rate (`robs` x 4, the display scale), targeting no change
#   - Unemployment Rate: the unemployment rate less the Fed's NAIRU estimate
#     at the time (the SEP longer-run median), in percentage points. Used by
#     Gali Unemployment (2010)
#   - Output Growth: quarterly GDP growth less the Fed's estimate of potential
#     growth at the time (the SEP longer-run median), as a quarterly log
#     change x 100 like `dy`
#   - Output: SW's model-consistent output `y` has no data counterpart, so,
#     as in the dashboard baseline, it stays at its steady state, which is
#     also its target
# The annualised rate itself (`Nominal Interest Rate`) is kept for the ELB
# constraint. `levels` is a vintage's `vintage_levels` rows, which must start
# before `vintage` so the first rate change is defined.
# Quarterly log growth x 100 (the units of `dy`) at `annual` per cent a year.
quarterly_growth <- function(annual) 100 * log1p(annual / 100) / 4

loss_gaps <- function(levels, vintage) {
  levels |>
    transmute(
      date,
      `Inflation_gap_loss` = pinfobs - inflation_target / 4,
      `Nominal Interest Rate Changes_gap_loss` = 4 * (robs - lag(robs)),
      `Unemployment Rate_gap_loss` = unemployment - nairu,
      `Output Growth_gap_loss` = dy - quarterly_growth(potential_growth),
      `Output_gap_loss` = 0,
      `Nominal Interest Rate` = 4 * robs
    ) |>
    filter(date >= vintage) |>
    mutate(vintage, .before = 1)
}

# 5. Vintages -------------------------------------------------------------
# Each vintage runs from its own quarter to the current `data_end`, covering
# every loss horizon Optimal Policy allows.
vintage_dates <- seq(current$forecast_start, by = "-1 quarter", length.out = vintage_quarters + 1)[-1]
# Each vintage's `baseline`, plus the unemployment rate and the NAIRU estimate
# its losses are measured from.
vintage_data <- purrr::map(vintage_dates, \(vintage) {
  message("Building vintage ", vintage)
  us <- build_us_data(vintage + months(1) - 1, data_end)
  levels <- headline(us$data) |>
    left_join(us$unemployment, by = "date") |>
    mutate(nairu = us$nairu, potential_growth = us$potential_growth)
  list(vintage = vintage, levels = levels)
})
vintage_levels <- purrr::map(vintage_data, \(v) {
  v$levels |> filter(date >= v$vintage) |> mutate(vintage = v$vintage, .before = 1)
}) |>
  bind_rows()
vintages <- purrr::map(vintage_data, \(v) loss_gaps(v$levels, v$vintage)) |>
  bind_rows()
if (anyNA(vintages)) stop("The loss-function vintages are incomplete.", call. = FALSE)

# The current loss-function targets, in display units: the Optimal Policy
# tab's default targets, so the current quarter's gaps match the vintages'.
targets <- tibble(
  variable = c("Inflation", "Unemployment Rate", "Output Growth", "Nominal Interest Rate Changes"),
  target = c(inflation_target / 4, current$nairu, quarterly_growth(current$potential_growth), 0)
)

# 6. Write ----------------------------------------------------------------
writexl::write_xlsx(
  list(
    baseline = headline(current$data) |> left_join(current$unemployment, by = "date"),
    model_data = model_data, timeline = timeline, targets = targets,
    vintages = vintages, vintage_levels = vintage_levels
  ),
  output_path
)
message("Wrote ", output_path, ": ", paste(names(timeline), vapply(timeline, format, ""), sep = " = ", collapse = ", "))
