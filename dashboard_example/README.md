# ezdyn example dashboard (USA)

An example [ezdyn](../README.md) dashboard with eleven model entries: the
Smets-Wouters (2007) model with anticipated monetary policy
shocks, seven small New Keynesian models from Johannes Pfeifer's
[DSGE_mod](https://github.com/JohannesPfeifer/DSGE_mod) collection, two
empirical models of US monetary policy shocks (Romer-Romer and
Gertler-Karadi, from McKay and Wolf 2023), and the RBA's DINGO model of the
Australian economy, for comparison. Gali NK+Wage (Ch.6) uses the
anticipated-shock version. It shows the models' impulse
responses, alternative policy-rate paths, optimal policy and historical shock
decompositions.

> **Illustrative only.** The models and data in this example are purely for
> illustrating what ezdyn can do. They have not had close quality control, so
> don't rely on their results.

The models are already built and a dummy baseline is included, so the
dashboard runs straight after cloning the repository: you don't need MATLAB,
Dynare or any data.

## Quick start

You need R (4.1 or later).

1. In R, set the working directory to this folder,
   `functions/ezdyn/dashboard_example/` (see [Working directory](#working-directory)).
2. Install ezdyn from this repository.

3. Launch the dashboard:

   ```r
    source("usa_dashboard.R")
   ```

    This script constructs and launches the Shiny app.

It opens with the dummy baseline. To use real US data, or your own, see
[Baselines](#baselines).

## Working directory

- **The dashboard and the R scripts** run from this folder,
  `functions/ezdyn/dashboard_example/` (the folder containing `usa_dashboard.R`). Set it
  with `setwd("<path to repository>/functions/ezdyn/dashboard_example")` or, in
  RStudio, *Session > Set Working Directory*, then run a script by its path from
  here, e.g. `Rscript build_baselines/build_dummy_baseline.R`. Launch the
  dashboard with `source("usa_dashboard.R")` from this folder.
- **The MATLAB scripts**, only needed to rebuild the models, run from their own
  folder (see [create_models/README.md](create_models/README.md#rebuilding-the-models)).

## Baselines

A baseline is the history and forecast that the dashboard's Alternative Paths
and Optimal Policy tabs move the policy rate away from. The dashboard offers:

| Baseline | File | In git | Offered |
|---|---|---|---|
| Dummy data (illustrative) | `inputs/dummy_baseline.xlsx` | Yes | Always |
| FRED data + Fed SEP | `inputs/us_baseline.xlsx` | No: you build it | When it exists, as the default |

Users can also upload a baseline in the dashboard (the upload button in
Alternative Paths and Optimal Policy).

### Format

A workbook whose first sheet has a `date` column and one column per variable,
named by its display name and in display units:

| Column | Units |
|---|---|
| `Output Growth`, `Consumption Growth`, `Investment Growth`, `Real Wage Growth` | Per cent, quarterly (log change x 100) |
| `Inflation` | Per cent, quarterly |
| `Hours Worked` | Per cent deviation from steady state (100 x log hours per person) |
| `Nominal Interest Rate` | Per cent, annualised |
| `Unemployment Rate` | Per cent |

Quarters are dated the first day of their last month (Q3 = `YYYY-09-01`), or
written as `YYYYQq`. The dashboard adds derived variables, such as year-ended
inflation, itself. A baseline must cover every quarter from `data_start` to
`data_end` in the [timeline](#updating-the-timeline-and-targets). Only variables
in the baseline can be plotted as alternative or optimal policy paths.

### Option 1: your own baseline

Save it in `inputs/` and add it to `baselines` in `usa_dashboard.R`:

```r
baselines$`My baseline` <- ezdyn::import_baseline("inputs/my_baseline.xlsx", models = models)
```

### Option 2: dummy data

`inputs/dummy_baseline.xlsx` is made-up data on the dates the FRED data would
have: history from 1954Q3 to the last quarter with published GDP data, then a
forecast 48 quarters beyond it. To rebuild it for today's date (no data or
internet needed):

```
Rscript build_baselines/build_dummy_baseline.R
```

It prints the matching timeline for `usa_dashboard.R`.

### Option 3: FRED data and the Fed's projections

US data from [FRED](https://fred.stlouisfed.org/), extended with the medians
of the Fed's latest Summary of Economic Projections (SEP). You need a free
[FRED API key](https://fred.stlouisfed.org/docs/api/api_key.html): add the
line `FRED_API_KEY=<your key>` to your user-level `~/.Renviron` (open it with
`usethis::edit_r_environ()`) and restart R. Then:

```
Rscript build_baselines/fetch_us_fred.R
Rscript build_baselines/build_us_baseline.R
```

`fetch_us_fred.R` downloads the data, which takes a couple of minutes, to
`inputs/us_fred_raw.rds`. `build_us_baseline.R` builds, without calling FRED:

- `inputs/us_baseline.xlsx`: the baseline;
- `inputs/us_vintages.xlsx`: past baselines for Optimal Policy's timeless
  commitment (optional: without it, Commitment is ordinary commitment from the
  forecast start);

and prints the matching timeline and loss-function targets for
`usa_dashboard.R`. None of these files are in git. Re-run both scripts after
each data release or SEP. See [FRED baseline: data and
assumptions](#fred-baseline-data-and-assumptions) for how the data are built.

## Updating the timeline and targets

`timeline` and `targets`, near the top of `usa_dashboard.R`, set the dashboard's
dates and Optimal Policy's default loss-function targets:

- `data_start`: the first quarter of history shown;
- `forecast_start`: the first forecast quarter;
- `forecast_end`: the end of the Alternative Paths window (10 quarters, one
  slider per quarter);
- `data_end`: the end of the baseline, Optimal Policy's horizon.

When you rebuild a baseline in a new quarter, paste in the timeline (and, for
the FRED baseline, the targets) it prints, and rebuild the dummy baseline too,
so every baseline covers the new dates. If one doesn't, the dashboard stops
with an error naming it.

## R packages

- **The dashboard**: ezdyn (see [Quick start](#quick-start)), plus `tibble`,
  `purrr`, `readxl` and `openxlsx2`.
- **`build_dummy_baseline.R`**: `writexl`.
- **The FRED scripts**: `fredr`, `dplyr`, `lubridate`, `purrr`, `tidyr` and
  `writexl`.
- **The model scripts**: see [create_models/README.md](create_models/README.md).

## Models

| Dashboard name | Type |
|---|---|
| Smets Wouters (2007) | Estimated DSGE model, with anticipated monetary policy shocks |
| Gali NK+Wage (Ch.6, anticipated) | Calibrated Chapter 6 model with 60 policy-news shocks; original calibration and metadata |
| Gali NK (Ch.3), Gali SOE (Ch.8), Gali Unemployment (2010), Ireland (2004), Ascari-Sbordone (2014), Born-Pfeifer (2018) | Calibrated New Keynesian models from DSGE_mod |
| Romer-Romer (2004), Gertler-Karadi (2015) | Empirical responses to monetary policy shocks (McKay and Wolf 2023) |
| DINGO | The RBA's estimated DSGE model of the Australian economy, with anticipated monetary policy shocks; no historical estimates |

`model_inputs` in `usa_dashboard.R` lists each model's files in `inputs/`. See
[create_models/README.md](create_models/README.md) for the models' sources,
definitions and assumptions.

## Rebuilding the models

You don't need to: the built models are in `inputs/`. To reproduce or change
them (the DSGE models need MATLAB and Dynare), see
[create_models/README.md](create_models/README.md#rebuilding-the-models).

## Files

- `app.R` - the dashboard's entry point; runs `usa_dashboard()`.
- `usa_dashboard.R` - the dashboard: models, timeline, targets, baselines and
  tab settings.
- `help_text.R` - the startup message and the help guides shown by the info
  buttons.
- `build_baselines/` - `build_dummy_baseline.R`, `fetch_us_fred.R` and
  `build_us_baseline.R` (see [Baselines](#baselines)).
- `create_models/` - the scripts that build the models, and their
  documentation.
- `inputs/` - the built models and metadata, the dummy baseline and, once
  built, the FRED files.

## FRED baseline: data and assumptions

`build_us_baseline.R` builds the US data from the FRED data saved by
`fetch_us_fred.R`: history for Smets-Wouters' seven observables and the
unemployment rate, extended with the medians from the Fed's latest SEP.

| Workbook | Sheet | Contents | Used by |
|---|---|---|---|
| `us_baseline.xlsx` | `baseline` | History and forecast in display names and units: headline growth rates, hours relative to their long-run level, the annualised nominal rate, and "Unemployment Rate" | The dashboard baseline ("Unemployment Rate" is Gali Unemployment (2010)'s) |
| | `model_data` | The same data in Smets-Wouters' own definitions and Dynare names: per-person growth rates, demeaned hours, the quarterly rate `robs` | Re-running SW over current data and forecasts (e.g. for shock decompositions) |
| `us_vintages.xlsx` (optional) | `vintages` | Loss-function gaps (each variable less its target at the time) in each of the last 12 quarters' baselines, in display units | Optimal Policy's timeless commitment |
| | `vintage_levels` | `baseline` as known in each of the last 12 quarters, plus that quarter's targets (`<display name> target`) | The source of `vintages` |

The printed targets are the current loss-function targets in display units:
inflation 0.5% a quarter (2% a year), unemployment and output growth
(quarterly and year-ended) at the latest SEP longer-run rates, and no rate
change.

Growth rates are quarterly log changes x 100, not annualised. The script
builds everything in the dashboard's display names and units, so the
dashboard reads the sheets without conversion: "Nominal Interest Rate" is the
fed funds rate itself (annualised). Only `model_data` converts, to Smets and
Wouters' own definitions and Dynare names, including the quarterly rate
`robs` (fed funds rate / 4).

### History

| Display name (SW code) | `baseline` | `model_data` | FRED series |
|---|---|---|---|
| Output Growth (`dy`) | Real GDP growth | Real GDP growth per person | `GDPC1`, `CNP16OV` |
| Consumption Growth (`dc`) | Real consumption growth | Real consumption growth per person | `PCEC`, `GDPDEF`, `CNP16OV` |
| Investment Growth (`dinve`) | Real fixed investment growth | Real fixed investment growth per person | `FPI`, `GDPDEF`, `CNP16OV` |
| Hours Worked (`labobs`) | Log hours per person, zero once unemployment is at its longer-run rate | Log hours per person, demeaned over the history | `PRS85006023`, `CE16OV`, `CNP16OV` |
| Inflation (`pinfobs`) | GDP deflator inflation | Same | `GDPDEF` |
| Real Wage Growth (`dw`) | Growth in hourly compensation deflated by the GDP deflator | Same | `COMPNFB`, `GDPDEF` |
| Nominal Interest Rate (`robs`) | Effective fed funds rate | Effective fed funds rate / 4 | `FEDFUNDS` |
| Unemployment Rate | Unemployment rate (not in `model_data`) | | `UNRATE` |

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
| Real GDP growth, Q4 to Q4 | `GDPC1MD`, `GDPC1MDLR` | Output Growth, and through it Consumption Growth, Investment Growth and Real Wage Growth |
| PCE inflation, Q4 to Q4 | `PCECTPIMD`, `PCECTPIMDLR` | Inflation |
| Unemployment rate, Q4 average | `UNRATEMD`, `UNRATEMDLR` | Unemployment Rate, and through it Hours Worked |
| Fed funds rate, end of year | `FEDTARMD`, `FEDTARMDLR` | Nominal Interest Rate |

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
- `forecast_end`: the end of the Alternative Paths window, 10 quarters
  (`alt_paths_quarters`) from `forecast_start`.
- `data_end`: end of the longer-run extension, the Optimal Policy horizon.

### Vintages

Timeless commitment assumes the Fed committed to its policy in some past
quarter (the tab's "Commitment start"). It re-solves each quarter since then
using the baseline as it looked at the time. The vintages are a separate,
optional dependency in `us_vintages.xlsx`: without them, the tab's Commitment
is ordinary commitment from the forecast start. They're only used alongside the
FRED baseline. Its `vintage_levels` sheet holds one such baseline for each of
the last 12 quarters (`vintage_quarters` in `fetch_us_fred.R`). Each is built
from FRED's real-time data (ALFRED) as known at the end of that quarter, so it
includes data to the quarter before and that quarter's SEP. It uses the same
definitions and assumptions as above and runs to the current `data_end`. The
earliest vintage is the earliest Commitment start the tab offers.

The loss function only needs each loss variable's gap from its target, so
the `vintages` sheet (what the dashboard reads) holds those gaps, measured
from the targets as they stood at the time. ezdyn reads one
`<loss variable>_gap_loss` column per loss variable, named by its display
name and in dashboard units, as `vintage` and `date` rows:

| Column | Gap | Target |
|---|---|---|
| `Inflation_gap_loss` | GDP deflator inflation (quarterly) less the target | 2% a year (0.5% a quarter), the Fed's target |
| `Nominal Interest Rate Changes_gap_loss` | Quarterly change in the annualised fed funds rate | No change |
| `Unemployment Rate_gap_loss` | Unemployment rate less the target, in percentage points | The Fed's NAIRU estimate at the time: the SEP's longer-run unemployment median (`UNRATEMDLR`) in that quarter |
| `Output Growth_gap_loss` | Quarterly real GDP growth less the target | The Fed's estimate of potential growth at the time: the SEP's longer-run GDP growth median (`GDPC1MDLR`) in that quarter, converted to a quarterly log change |
| `Output Growth (year-ended)_gap_loss` | Real GDP growth over the last four quarters (the sum of quarterly growth) less the target | The same potential growth estimate, as a log change over the year |

It also has `Nominal Interest Rate` (annualised), the instrument's level, for
the ELB. `vintage_levels` holds each vintage's `baseline` columns and its
targets, in display units: `Inflation target`, `Unemployment Rate target`
(that NAIRU estimate), and `Output Growth target` and `Output Growth
(year-ended) target` (that potential growth estimate, as quarterly and
year-ended log changes). Each gap in `vintages` is the level less its target.

Timeless commitment needs a vintage column for every loss variable selected,
so the tab offers only these five as loss variables (Inflation, Output Growth
and Nominal Interest Rate Changes by default). "Unemployment Rate" is offered
only while Gali Unemployment (2010), the only model with it, is the only
model selected.

The tab measures the current quarter's gaps from its target inputs, which
default to `targets` in `usa_dashboard.R`: the same targets the vintages use,
as they stand today. So by default the current unemployment gap is from the
Fed's latest NAIRU estimate, and each vintage's from the estimate of its time.

Commitment, timeless commitment and discretion only give different optimal
paths in a model with anticipated policy shocks, with the Anticipation
Parameter above 0. In this dashboard that is Smets-Wouters (see
[create_models/README.md](create_models/README.md#smets-wouters-with-anticipated-monetary-policy-shocks)).
The other models' policy shocks are always surprises, so the tab shows one
optimal path for each of them.
