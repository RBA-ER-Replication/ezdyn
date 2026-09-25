# DSGEModDash: ezdyn Dashboard (USA)

A multi-model [ezdyn](../../functions/ezdyn) dashboard combining the
Smets-Wouters (2007) estimated model with 7 curated small-scale New Keynesian
models from Johannes Pfeifer's
[DSGE_mod](https://github.com/JohannesPfeifer/DSGE_mod) collection of Dynare
replication files. Every model here has an active monetary policy shock, so
the dashboard can be used for cross-model impulse-response policy exercises.

## Launch the dashboard

Once you have run the first-time setup (below), open `app.R` in RStudio and run it, or from R:

```r
setwd("dashboard/DSGEModDash")
source("app.R")
```

## First-time setup

Run these steps from a clone of this repository, with MATLAB (+ Dynare) and R
available on your PATH.

### 1. Clone DSGE_mod into this folder

```powershell
cd dashboard/DSGEModDash
git clone https://github.com/JohannesPfeifer/DSGE_mod.git DSGE_mod
```

The folder must be named exactly `DSGE_mod` (matching what a fresh `git
clone` of that repo produces) - `build_new_models.m` looks for it at
`dashboard/DSGEModDash/DSGE_mod`. This folder is a separate git repository
(it has its own `.git`) and is intentionally excluded from this repo's git
tracking (see `.gitignore`).

### 2. Build the Dynare model objects (MATLAB)

From the repo root:

```powershell
matlab.exe -batch "run('dashboard/DSGEModDash/build_new_models.m')"
```

This runs all 8 models (Smets-Wouters 2007 + the 7 curated NK models)
through Dynare and exports their `M_`/`oo_` objects as JSON into
`dashboard/DSGEModDash/inputs/`, plus a CSV export of SW's real historical
US estimation sample.

### 3. Build the variable/shock/parameter metadata (R)

From `dashboard/DSGEModDash`:

```powershell
Rscript build_model_metadata.R
```

This reads the JSON files produced in step 2 and writes one metadata
workbook per model into `dashboard/DSGEModDash/inputs/`.

### 4. Build the US baseline from FRED (R)

Get a free [FRED API key](https://fred.stlouisfed.org/docs/api/api_key.html)
and add it to your user-level `~/.Renviron` (open it with
`usethis::edit_r_environ()`), then restart R:

```
FRED_API_KEY=<your key>
```

Then, from `dashboard/DSGEModDash`:

```powershell
Rscript build_us_baseline.R
```

This writes `inputs/us_baseline.xlsx` (see [US baseline data](#us-baseline-data)
below). It takes a couple of minutes, mostly fetching the real-time vintages.
Re-run it after each data release or SEP to refresh the baseline.

## US baseline data

`build_us_baseline.R` builds the dashboard's US data from
[FRED](https://fred.stlouisfed.org/): history for Smets-Wouters' seven
observables, extended with the medians from the Fed's latest Summary of
Economic Projections (SEP). `inputs/us_baseline.xlsx` has four sheets:

| Sheet | Contents | Used by |
|---|---|---|
| `baseline` | History and forecast in dashboard units: headline growth rates, hours relative to their long-run level | The dashboard baseline |
| `model_data` | The same data in Smets-Wouters' own definitions: per-person growth rates, demeaned hours | Re-running SW over current data and forecasts (e.g. for shock decompositions) |
| `timeline` | `data_start`, `data_end`, `forecast_start`, `forecast_end` | The dashboard timeline |
| `vintages` | `baseline` as known in each of the last 12 quarters | Optimal Policy's timeless commitment |

Quarters are dated the first day of their last month (Q3 = `YYYY-09-01`).
Growth rates are quarterly log changes x 100, not annualised.

### History

| Observable | `baseline` | `model_data` | FRED series |
|---|---|---|---|
| `dy` Output growth | Real GDP growth | Real GDP growth per person | `GDPC1`, `CNP16OV` |
| `dc` Consumption growth | Real consumption growth | Real consumption growth per person | `PCEC`, `GDPDEF`, `CNP16OV` |
| `dinve` Investment growth | Real fixed investment growth | Real fixed investment growth per person | `FPI`, `GDPDEF`, `CNP16OV` |
| `labobs` Hours worked | Log hours per person, zero once unemployment is at its longer-run rate | Log hours per person, demeaned over the history | `PRS85006023`, `CE16OV`, `CNP16OV` |
| `pinfobs` Inflation | GDP deflator inflation | Same | `GDPDEF` |
| `dw` Real wage growth | Growth in hourly compensation deflated by the GDP deflator | Same | `COMPNFB`, `GDPDEF` |
| `robs` Interest rate | Effective fed funds rate / 4 | Same | `FEDFUNDS` |

- Nominal consumption and investment are deflated by the GDP deflator,
  following Smets and Wouters (2007).
- Hours per person are nonfarm business average weekly hours x civilian
  employment / civilian population aged 16+.
- Monthly series are averaged to quarterly.
- The history starts in the first quarter with all seven observables (1954Q3,
  when the fed funds rate starts) and ends in the last quarter with all of
  them.
- Headline and per-person growth differ only by population growth. Impulse
  responses apply equally to both: shocks in the dashboard's models don't move
  population, so they shift both growth rates by the same amount.
- The hours level is a choice of reference point, not a detrending. The
  `baseline` puts zero where unemployment is at the SEP longer-run rate, so
  hours read as a deviation from steady state in the model's units. The
  `model_data` demeaning follows Smets and Wouters (2007). Hours per person
  have fallen since about 2000 as the population has aged, so on that measure
  current hours sit about 7% below their sample mean.

### Forecast

The forecast uses four SEP medians:

| SEP projection | FRED series | Drives |
|---|---|---|
| Real GDP growth, Q4 to Q4 | `GDPC1MD`, `GDPC1MDLR` | `dy`, and through it `dc`, `dinve`, `dw` |
| PCE inflation, Q4 to Q4 | `PCECTPIMD`, `PCECTPIMDLR` | `pinfobs` |
| Unemployment rate, Q4 average | `UNRATEMD`, `UNRATEMDLR` | `labobs` |
| Fed funds rate, end of year | `FEDTARMD`, `FEDTARMDLR` | `robs` |

Each SEP value is an anchor in Q4 of its year, and each quarterly path runs
linearly from the last observation to those anchors:

- GDP and inflation are interpolated in log levels, so growth is constant
  within each year and matches the Fed's Q4/Q4 projection exactly. In the
  current year, the unpublished quarters make up whatever the published
  quarters leave of the projection.
- The unemployment and fed funds paths run straight to each year's Q4 value.
- After the last projection year, the SEP longer-run values take over: GDP
  growth and inflation run at their longer-run rates from the following year,
  and unemployment and the fed funds rate reach theirs by Q4 of that year. All
  then stay there. The data run 48 quarters past the last published quarter,
  long enough for the Optimal Policy horizon.

For series the SEP doesn't cover:

- **Inflation**: GDP deflator inflation follows the PCE inflation projection.
- **Consumption and investment**: real growth equals real GDP growth.
- **Real wages**: grow with real GDP per person (balanced growth, as in the
  model's steady state).
- **Population**: grows at its latest year-ended rate. It only affects
  `model_data`'s per-person growth and real wages.
- **Hours per person**: move with the employment rate (1 - unemployment rate),
  holding participation and average weekly hours at their latest levels.
- **Fed funds rate**: the Q4 average effective rate equals the SEP year-end
  midpoint, ignoring the small gap between the effective rate and the target
  midpoint.

### Dates

- `data_start`: first quarter of history.
- `forecast_start`: first quarter after the last quarter of data.
- `forecast_end`: Q4 of the last SEP projection year, the Alternative Paths
  forecast window.
- `data_end`: end of the longer-run extension, the Optimal Policy horizon.

### Vintages

Timeless commitment assumes the Fed committed to its policy in some past
quarter (the tab's "Commitment start"). It re-solves each quarter since then
using the baseline as it looked at the time. The `vintages` sheet holds one
such baseline for each of the last 12 quarters (`vintage_quarters` in the
script). Each is built from FRED's real-time data (ALFRED) as known at the end
of that quarter, so it includes data to the quarter before and that
quarter's SEP. It uses the same definitions and assumptions as above and runs
to the current `data_end`. The earliest vintage is the earliest Commitment
start the tab offers.

Smets-Wouters' monetary policy shock is always a surprise. Future policy moves
can't change today's expectations, so commitment, timeless commitment and
discretion give the same optimal path for SW. They only differ in a model with
anticipated policy shocks.

## Files

- `build_new_models.m` - builds all 8 models' Dynare JSON objects (MATLAB).
- `build_model_metadata.R` - builds all 8 models' variable/shock/parameter
  metadata workbooks (R).
- `build_us_baseline.R` - builds the US baseline, timeline and real-time
  vintages from FRED (R).
- `usa_dashboard.R` - dashboard composition (paths, timeline, model list,
  `configure_dashboard()` call, `usa_dashboard()` function).
- `app.R` - deploy entrypoint; sources `usa_dashboard.R` and returns the app.
- `DSGE_mod/` - cloned separately (step 1 above), not tracked in this repo.
- `inputs/` - generated by steps 2-4 above, not tracked in this repo (see
  `.gitignore`) - regenerate locally rather than committing these.
