# DSGEModDash: ezdyn Dashboard (USA)

A multi-model [ezdyn](../../functions/ezdyn) dashboard combining the
Smets-Wouters (2007) estimated model with 7 curated small-scale New Keynesian
models from Johannes Pfeifer's
[DSGE_mod](https://github.com/JohannesPfeifer/DSGE_mod) collection of Dynare
replication files, plus two semi-structural models: estimated responses to the
Romer-Romer (2004) and Gertler-Karadi (2015) monetary policy shocks, from
McKay and Wolf (2023). Every model here has an active monetary policy shock,
so the dashboard can be used for cross-model impulse-response policy
exercises.

## Launch the dashboard

Once you have run the first-time setup (below), open `app.R` in RStudio and run it, or from R:

```r
setwd("dashboard/DSGEModDash")
source("app.R")
```

The dashboard opens with a disclaimer: it is a beta, and the code that loads
the DSGE_mod models is experimental, so their results are indicative only.
Buttons under the title download the ezdyn whitepaper (`ezdyn.pdf` from
GitHub) and open a new GitHub issue to report a problem.

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

### 5. Build the semi-structural models (R)

From `dashboard/DSGEModDash`:

```powershell
Rscript build_semi_structural_models.R
```

This downloads McKay and Wolf's (2023) published estimates and writes
`inputs/semi_structural_irfs.csv` and `inputs/semi_structural_metadata.xlsx`
(see [Semi-structural models](#semi-structural-models) below). No API key is
needed.

## US baseline data

`build_us_baseline.R` builds the dashboard's US data from
[FRED](https://fred.stlouisfed.org/): history for Smets-Wouters' seven
observables, extended with the medians from the Fed's latest Summary of
Economic Projections (SEP). `inputs/us_baseline.xlsx` has six sheets:

| Sheet | Contents | Used by |
|---|---|---|
| `baseline` | History and forecast in dashboard units: headline growth rates, hours relative to their long-run level, and the unemployment rate (`unemployment`) | The dashboard baseline; the unemployment rate is its "Unemployment Rate" (Gali Unemployment (2010)) |
| `model_data` | The same data in Smets-Wouters' own definitions: per-person growth rates, demeaned hours | Re-running SW over current data and forecasts (e.g. for shock decompositions) |
| `timeline` | `data_start`, `data_end`, `forecast_start`, `forecast_end` | The dashboard timeline |
| `targets` | The current loss-function targets in dashboard units: inflation 0.5% a quarter (2% a year), unemployment and output growth at the latest SEP longer-run rates, no rate change | The Optimal Policy tab's default targets |
| `vintages` | Loss-function gaps (each variable less its target at the time) in each of the last 12 quarters' baselines, in dashboard units | Optimal Policy's timeless commitment |
| `vintage_levels` | `baseline` as known in each of the last 12 quarters, plus the unemployment rate and the Fed's NAIRU estimate | The source of `vintages` |

Quarters are dated the first day of their last month (Q3 = `YYYY-09-01`).
Growth rates are quarterly log changes x 100, not annualised. The sheets store
the nominal rate `robs` as Smets and Wouters define it, quarterly (fed funds
rate / 4); the dashboard shows it annualised (see
[Common variables](#common-variables)).

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
- `forecast_end`: Q4 of the last SEP projection year. `usa_dashboard.R`
  shortens the forecast to its first 10 quarters (`alt_paths_quarters`): the
  Alternative Paths window, with one slider per quarter.
- `data_end`: end of the longer-run extension, the Optimal Policy horizon.

### Vintages

Timeless commitment assumes the Fed committed to its policy in some past
quarter (the tab's "Commitment start"). It re-solves each quarter since then
using the baseline as it looked at the time. The `vintage_levels` sheet holds
one such baseline for each of the last 12 quarters (`vintage_quarters` in the
script). Each is built from FRED's real-time data (ALFRED) as known at the end
of that quarter, so it includes data to the quarter before and that
quarter's SEP. It uses the same definitions and assumptions as above and runs
to the current `data_end`. The earliest vintage is the earliest Commitment
start the tab offers.

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
| `Output Growth_gap_loss` | Quarterly real GDP growth (`dy`) less the target | The Fed's estimate of potential growth at the time: the SEP's longer-run GDP growth median (`GDPC1MDLR`) in that quarter, converted to a quarterly log change |
| `Output_gap_loss` | Zero: SW's "Output" has no data and stays at steady state, as in the baseline | Steady state |

It also has `Nominal Interest Rate` (annualised), the instrument's level, for
the ELB. `vintage_levels` adds the unemployment rate (`unemployment`), that
NAIRU estimate (`nairu`) and that potential growth estimate
(`potential_growth`, per cent a year) to each vintage's `baseline` columns.

Commitment needs a vintage column for every loss variable selected, so the
tab offers only these five as loss variables. "Unemployment Rate" is offered only while Gali
Unemployment (2010), the only model with it, is the only model selected. Its
baseline is the US unemployment rate (the `baseline` sheet's `unemployment`,
history then the path to the SEP), not the model's steady state.

The tab measures the current quarter's gaps from its target inputs, which
default to the `targets` sheet: the same targets the vintages use, as they
stand today. So by default the current unemployment gap is from the Fed's
latest NAIRU estimate, and each vintage's from the estimate of its time.

Smets-Wouters' monetary policy shock is always a surprise. Future policy moves
can't change today's expectations, so commitment, timeless commitment and
discretion give the same optimal path for SW. They only differ in a model with
anticipated policy shocks. None of the dashboard's models has them, so the tab
shows one optimal path for each selected model.

## Semi-structural models

"Romer-Romer (2004)" and "Gertler-Karadi (2015)" are empirical models: the
estimated responses of the US economy to two monetary policy shock series,
from McKay and Wolf (2023), "What Can Time-Series Regressions Tell Us About
Policy Counterfactuals?", *Econometrica* 91(5). `build_semi_structural_models.R`
downloads their published estimates from the paper's replication files
([ckwolf92/policy_cnfctls](https://github.com/ckwolf92/policy_cnfctls), MIT
licence, pinned to commit `4e4e81f`).

### Estimates

- **Shocks**: the Romer and Romer (2004) narrative shocks, extended to 2007 by
  Wieland and Yang (2020); and the Gertler and Karadi (2015) surprise in
  current-month fed funds futures around FOMC announcements (`mp1_tc` from
  Ramey 2016, available from 1988Q4).
- **Model**: a quarterly VAR over 1969Q1-2007Q4 with 4 lags, a constant and a
  linear trend, estimated with a flat prior. The variables, in order, are the
  GK shock, the HP-filtered output gap, GDP deflator inflation, log commodity
  prices, the RR shock and the fed funds rate. Each shock's effects are
  identified by that ordering, with the shock set to zero where its series is
  unavailable.
- **Responses**: the posterior medians over 100 quarters.

The two shocks move rates differently. An RR shock is a short-lived jump in
the fed funds rate that reverses within about a year. A GK shock raises rates
gradually over about 18 months and keeps them elevated for about three years. Output and
inflation fall after an RR shock. After a GK shock they rise slightly at first
(a "price puzzle" in the estimates) and only fall after about two years.

### Variables

| Code | Display name | From the estimates |
|---|---|---|
| `ygap` | Output Gap | Output gap response (%) |
| `output` | Output | Same as the output gap (the shock doesn't move potential output) |
| `dy` | Output Growth | Quarterly change in the output gap |
| `pinfobs` | Inflation | Annualised inflation / 4 |
| `pinfobs_ye` | Inflation (year-ended) | Sum of the last four quarters of `pinfobs` |
| `robs` | Nominal Interest Rate | Fed funds rate (stored as fed funds rate / 4, shown x 4) |
| `robs_q` | Nominal Interest Rate (quarterly) | Fed funds rate / 4 |
| `robs_change` | Nominal Interest Rate Changes | Quarterly change in the annualised fed funds rate (not in Alternative Paths) |

Codes, names and units match Smets-Wouters', so the models use the
dashboard's US baseline and Optimal Policy settings unchanged. Each shock is
scaled so the fed funds rate peaks 100bp (annualised) above baseline.

### In the dashboard

- **Impulse Response Library**: responses to the "Monetary policy shock",
  comparable with the other models.
- **Optimal Policy**: Gertler-Karadi (2015) is selected by default alongside
  SW, so its optimal path is shown next to SW's. While it is selected, the
  tab doesn't offer response or loss variables it lacks (e.g. real wage
  growth). Because output and inflation rise at first after a GK rate rise,
  its optimal path can move rates the opposite way to SW's in the short run.
  Beyond the first few years its path is unreliable: with a 40-quarter horizon
  and no ELB, it keeps cutting, to about -23% by quarter 40, because cuts that
  late only take effect after the loss horizon ends. Turn on the ELB or shorten
  the horizons when comparing with it.
- **Alternative Paths**: selectable, but not selected by default. Treat their
  results with caution. The tab hits the rate path with a fresh surprise shock
  each quarter, and both estimated rate responses peak after impact, so
  holding the rate on a path takes shocks that alternate in sign and grow, and
  output and inflation oscillate. McKay and Wolf instead treat the shocks as
  news about the whole rate path and combine them, which the dashboard doesn't
  do.
- **Not available**: Historical Shock Decomposition (there are no smoothed
  shocks), and hours or wage variables.

## Common variables

Every DSGE model (Smets-Wouters and the 7 Pfeifer models) has these eight
variables under the same name and units, so they can be compared across
models in the Impulse Response Library and Alternative Paths:

| Display name | Units |
|---|---|
| Inflation | Per cent (quarterly) |
| Inflation (year-ended) | Per cent (year-ended) |
| Nominal Interest Rate | Per cent (annualized) |
| Nominal Interest Rate (quarterly) | Per cent (quarterly) |
| Nominal Interest Rate Changes | Percentage points (annualized) |
| Hours Worked | Per cent deviation from steady state |
| Output | Per cent deviation from steady state |
| Output Growth | Per cent (quarterly) |

Each comes from the model variable below. In every model:

- "Inflation (year-ended)" is the sum of the last four quarters of
  "Inflation" (the `year_ended_sum` transform).
- "Nominal Interest Rate" is annualised: the model's quarterly rate (below)
  with a `scale_formula` of 4. In the US baseline it is the effective fed
  funds rate. "Nominal Interest Rate (quarterly)" is a derived row that scales
  it back by 1/4.
- "Nominal Interest Rate Changes" is the quarterly change in the annualised
  rate (the `diff` transform), e.g. for interest-rate smoothing in the
  Optimal Policy loss function. It isn't offered in Alternative Paths.
- "Output Growth" is the model's own output growth variable in SW (`dy`) and
  Ireland (`ghat`). Every other model gets a derived row, the quarterly
  change in "Output" (the `diff` transform): the deviation of output growth
  from trend, as SW's `dy` is (`dy = y - y(-1)` plus a constant trend). In
  the US baseline it is real GDP growth.
- Every other interest rate is annualised the same way (x 4): the Gali
  models', Ascari-Sbordone's and Born-Pfeifer's real and natural rates, and
  SW's model-consistent nominal rate `r`.

Because the policy instrument is annualised, the Alternative Paths sliders,
Custom IRF targets and Optimal Policy rate settings (loss targets, bounds and
the 0.1% ELB) are all in annual per cent.

The semi-structural models also have "Inflation (year-ended)", "Nominal
Interest Rate (quarterly)", "Nominal Interest Rate Changes" and "Output
Growth" (see their Variables table above).

| Model | Inflation | Nominal Interest Rate | Hours Worked | Output |
|---|---|---|---|---|
| Smets-Wouters (2007) | `pinfobs` | `robs` | `labobs` | `y` |
| Gali NK (Ch.3) | `pi` | `i` | `n` | `y` |
| Gali NK+Wage (Ch.6) | `pi_p` | `i` | `n` | `y` |
| Gali SOE (Ch.8) | `pi` | `i` | `n` | `y` |
| Gali Unemployment (2010) | `pi_p` | `ihat` | `nhat` | `y_gap` |
| Ireland (2004) | `pihat` | `rhat` | `yhat` | `yhat` (Alternative Paths only) |
| Ascari-Sbordone (2014) | `pi` | `i` x `i_bar` | `N` | `y` |
| Born-Pfeifer (2018) | `pi_p` | `i` | `n` | `y` |

Where a model's own label differs (e.g. "Price Inflation", "Employment"),
`build_model_metadata.R` adds the common name as a derived alias row
(`derived_from`, no transform) and the model's own label stays available.

### Definitions and assumptions

- **Inflation is price inflation.** Gali Ch.6, Gali (2010) and Born-Pfeifer
  have both price and wage inflation. "Inflation" is price inflation (`pi_p`),
  like SW's GDP deflator inflation. In the small open economy (Gali Ch.8) it
  is the model's own inflation variable, CPI inflation. Domestic inflation is
  still available as "Domestic Inflation".
- **Year-ended inflation is exact.** Every model's quarterly inflation is a
  log change in the price level, so the four-quarter sum is exactly the log
  change over the year. It is not compounded quarterly rates. In the US
  baseline, the first three quarters of history (1954Q3 to 1955Q1) sum fewer
  than four quarters, because the history starts in 1954Q3.
- **Gali Ch.8 hours.** The model labels `n` "employment", but it is the same
  labour input as Ch.3's hours: both models produce with
  `y = a + (1 - alpha) * n`.
- **Gali (2010) hours are employment.** The model has no hours-per-worker
  margin: employment is the only labour input in production. So hours worked
  move one-for-one with employment. This is the model's own assumption, not
  an approximation added here.
- **Gali (2010)'s unemployment rate is a level.** The model's `urhat` is the
  unemployment rate's deviation from steady state, in percentage points. Its
  units are "Per cent (of the labour force)", like an interest rate: the
  model moves it in percentage points around a baseline in per cent, the US
  unemployment rate.
- **Ireland (2004) hours equal detrended output.** Technology is
  `Y_t = Z_t * h_t` (Ireland 2004, eq. 6), so hours equal output relative to
  technology, `Y_t / Z_t`. That is the model's `yhat`, for every shock.
- **Ireland (2004) output is in Alternative Paths only.** `yhat` is output
  relative to technology `Z_t`, a random walk that only technology shocks
  move. So `yhat` is the response of output itself to every shock except a
  technology shock, including the monetary policy shock Alternative Paths
  uses. After a technology shock it misses the permanent shift in `Z_t`. Its
  metadata therefore has `irf = FALSE`. When Ireland (2004) is selected, the
  Impulse Response Library doesn't offer "Output".
- **Gali (2010)'s `y_gap` is output.** Despite its name, the mod file labels
  it "output" and defines it with the production function
  `y_gap = a + (1 - alfa) * nhat`. It was previously mislabelled "Output Gap".
- **Ascari-Sbordone (2014)'s interest rate is rescaled.** This model is solved
  in logs of levels, and `i` is the log of the net nominal rate. So its raw
  response is a proportional change in the rate: a rise from 1% to 1.25%
  shows as a 25% rise. Its `scale_formula` multiplies by the steady-state net
  rate `i_bar`, giving the change in the quarterly rate itself, and then by 4
  to annualise (`4 * i_bar`). That puts it in the same units as every other
  model's rate. Before this, its Alternative Paths responses were about 100
  times too small.
- **Smets-Wouters "Output" has no data.** SW observes only output growth, so
  "Output" is its model-consistent `y` (previously "Output (model-consistent)").
  Its baseline stays at steady state (zero), so Alternative Paths shows it as
  a deviation from baseline.

No model needed output to be approximated by the output gap: each has its own
output variable.

### Which tabs offer a variable

Three logical columns in each model's variable metadata control where a
variable appears:

| Column | Tab |
|---|---|
| `irf` | Impulse Response Library |
| `alt_paths` | Alternative Paths |
| `hsd` | Historical Shock Decomposition |

A blank or missing `irf` or `alt_paths` means the variable is offered. Both
tabs list only variables offered by every selected model. SW's
model-consistent series have `alt_paths = FALSE`. So in Alternative Paths, SW
offers its seven observables, "Inflation (year-ended)", "Nominal Interest
Rate (quarterly)" and "Output".

### Unit symbols

The build scripts set each variable's plot axis symbols from its `units`:

| Units | `unit_symbol_irf` | `unit_symbol_baseline` |
|---|---|---|
| Per cent (quarterly, annualized or year-ended) | ppt | % |
| Per cent deviation from steady state | % dev | % dev |
| Percentage points (deviation from steady state, or a change) | ppt | ppt |

Impulse responses use `unit_symbol_irf`, because they are changes from
baseline: a rate moves in percentage points. Alternative Paths use
`unit_symbol_baseline`, because they plot levels: a rate is a per cent.
`build_model_metadata.R` stops if a variable has units without a symbol.

## Files

- `build_new_models.m` - builds all 8 models' Dynare JSON objects (MATLAB).
- `build_model_metadata.R` - builds all 8 models' variable/shock/parameter
  metadata workbooks (R).
- `build_us_baseline.R` - builds the US baseline, timeline and real-time
  vintages from FRED (R).
- `build_semi_structural_models.R` - builds the Romer-Romer and
  Gertler-Karadi semi-structural models from McKay and Wolf's (2023) estimates
  (R).
- `usa_dashboard.R` - dashboard composition (paths, timeline, model list,
  `configure_dashboard()` call, `usa_dashboard()` function).
- `app.R` - deploy entrypoint; sources `usa_dashboard.R` and returns the app.
- `DSGE_mod/` - cloned separately (step 1 above), not tracked in this repo.
- `inputs/` - generated by steps 2-5 above, not tracked in this repo (see
  `.gitignore`) - regenerate locally rather than committing these.
