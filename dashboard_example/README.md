# ezdyn Dashboard (USA)

A multi-model [ezdyn](../README.md) dashboard combining the Smets-Wouters
(2007) estimated model, extended with anticipated monetary policy shocks, with
7 curated small-scale New Keynesian models from Johannes Pfeifer's
[DSGE_mod](https://github.com/JohannesPfeifer/DSGE_mod) collection of Dynare
replication files, plus two semi-structural models: estimated responses to the
Romer-Romer (2004) and Gertler-Karadi (2015) monetary policy shocks, from
McKay and Wolf (2023). Every model here has an active monetary policy shock,
so the dashboard can be used for cross-model impulse-response policy
exercises.

## Quick start

The model objects are already built and stored in `inputs/`, so you don't
need MATLAB, Dynare or DSGE_mod. You only need to build the US baseline
(`inputs/us_baseline.xlsx`), which isn't stored in this repository.

You need R with ezdyn installed (see [Installation](../README.md#installation))
and a free [FRED API key](https://fred.stlouisfed.org/docs/api/api_key.html).

1. Add your FRED API key to your user-level `~/.Renviron` (open it with
   `usethis::edit_r_environ()`), then restart R:

   ```
   FRED_API_KEY=<your key>
   ```

2. Fetch the US data from FRED and build the baseline. From the repo root:

   ```powershell
   Rscript dashboard_example/fetch_us_fred.R
   Rscript dashboard_example/build_us_baseline.R
   ```

   `fetch_us_fred.R` downloads everything from FRED (taking a couple of
   minutes, mostly for the real-time vintages) and saves it, unprocessed, to
   `inputs/us_fred_raw.rds`. `build_us_baseline.R` turns that into
   `inputs/us_baseline.xlsx` without calling FRED, so it can be re-run freely
   after changing the processing. Re-run both after each data release or SEP
   to refresh the baseline. Both files are gitignored. See
   [US baseline data](#us-baseline-data) for what it builds.

3. Launch the dashboard. From the repo root, in R:

   ```r
   shiny::runApp("dashboard_example")
   ```

   Or open `app.R` in RStudio and click **Run App**.

The dashboard opens with a short introduction, which warns that the code that
loads the DSGE_mod models is experimental, so their results are indicative
only. Links under the title open the ezdyn whitepaper (`ezdyn.pdf` on GitHub)
and a new GitHub issue to report a problem.

To rebuild the model objects yourself, for example to reproduce them or change
a model, see [Rebuilding the model objects](#rebuilding-the-model-objects).

## Models

| Dashboard name | Source | Built by |
|---|---|---|
| Smets Wouters (2007) | This repository: [`Smets_Wouters_2007_anticipated/`](Smets_Wouters_2007_anticipated/), a version of DSGE_mod's `Smets_Wouters_2007/Smets_Wouters_2007_45.mod` with anticipated monetary policy shocks (see [Smets-Wouters with anticipated monetary policy shocks](#smets-wouters-with-anticipated-monetary-policy-shocks)) | `Smets_Wouters_2007_anticipated/build_sw_anticipated.m` |
| Gali NK (Ch.3) | DSGE_mod: `Gali_2015/Gali_2015_chapter_3.mod` | `build_new_models.m` |
| Gali NK+Wage (Ch.6) | DSGE_mod: `Gali_2015/Gali_2015_chapter_6.mod` | `build_new_models.m` |
| Gali SOE (Ch.8) | DSGE_mod: `Gali_2015/Gali_2015_chapter_8.mod` | `build_new_models.m` |
| Gali Unemployment (2010) | DSGE_mod: `Gali_2010/Gali_2010.mod` | `build_new_models.m` |
| Ireland (2004) | DSGE_mod: `Ireland_2004/Ireland_2004.mod` | `build_new_models.m` |
| Ascari-Sbordone (2014) | DSGE_mod: `Ascari_Sbordone_2014/Ascari_Sbordone_2014.mod` | `build_new_models.m` |
| Born-Pfeifer (2018) | DSGE_mod: `Born_Pfeifer_2018/Monetary_Policy_IRFs/Born_Pfeifer_2018_MP.mod` | `build_new_models.m` |
| Romer-Romer (2004), Gertler-Karadi (2015) | McKay and Wolf's (2023) published estimates (see [Semi-structural models](#semi-structural-models)) | `build_semi_structural_models.R` |

`build_new_models.m` runs the 7 DSGE_mod models from a clone of DSGE_mod. The
Smets-Wouters model is not pulled from DSGE_mod: its anticipated-shocks
version, with the data and posterior mode it needs, is kept in this
repository. `build_model_metadata.R` builds every DSGE model's
variable/shock/parameter metadata workbook.

## US baseline data

`build_us_baseline.R` builds the dashboard's US data from
[FRED](https://fred.stlouisfed.org/) data saved by `fetch_us_fred.R`:
history for Smets-Wouters' seven observables and the unemployment rate,
extended with the medians from the Fed's latest Summary of Economic
Projections (SEP). `inputs/us_baseline.xlsx` has six sheets:

| Sheet | Contents | Used by |
|---|---|---|
| `baseline` | History and forecast in display names and units: headline growth rates, hours relative to their long-run level, the annualised nominal rate, and "Unemployment Rate" | The dashboard baseline ("Unemployment Rate" is Gali Unemployment (2010)'s) |
| `model_data` | The same data in Smets-Wouters' own definitions and Dynare names: per-person growth rates, demeaned hours, the quarterly rate `robs` | Re-running SW over current data and forecasts (e.g. for shock decompositions) |
| `timeline` | `data_start`, `data_end`, `forecast_start`, `forecast_end` | The dashboard timeline |
| `targets` | The current loss-function targets in display units: inflation 0.5% a quarter (2% a year), unemployment and output growth at the latest SEP longer-run rates, no rate change | The Optimal Policy tab's default targets |
| `vintages` | Loss-function gaps (each variable less its target at the time) in each of the last 12 quarters' baselines, in display units | Optimal Policy's timeless commitment |
| `vintage_levels` | `baseline` as known in each of the last 12 quarters, plus that quarter's targets (`<display name> target`) | The source of `vintages` |

Quarters are dated the first day of their last month (Q3 = `YYYY-09-01`).
Growth rates are quarterly log changes x 100, not annualised. The script
builds everything in the dashboard's display names and units, so the
dashboard reads the sheets without conversion: "Nominal Interest Rate" is the
fed funds rate itself (annualised, see [Common variables](#common-variables)).
Only `model_data` converts, to Smets and Wouters' own definitions and Dynare
names, including the quarterly rate `robs` (fed funds rate / 4).

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
- `forecast_end`: Q4 of the last SEP projection year. `usa_dashboard.R`
  shortens the forecast to its first 10 quarters (`alt_paths_quarters`): the
  Alternative Paths window, with one slider per quarter.
- `data_end`: end of the longer-run extension, the Optimal Policy horizon.

### Vintages

Timeless commitment assumes the Fed committed to its policy in some past
quarter (the tab's "Commitment start"). It re-solves each quarter since then
using the baseline as it looked at the time. The `vintage_levels` sheet holds
one such baseline for each of the last 12 quarters (`vintage_quarters` in
`fetch_us_fred.R`). Each is built from FRED's real-time data (ALFRED) as known
at the end of that quarter, so it includes data to the quarter before and that
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
| `Output Growth_gap_loss` | Quarterly real GDP growth less the target | The Fed's estimate of potential growth at the time: the SEP's longer-run GDP growth median (`GDPC1MDLR`) in that quarter, converted to a quarterly log change |
| `Output_gap_loss` | Zero: SW's "Output" has no data and stays at steady state, as in the baseline | Steady state |

It also has `Nominal Interest Rate` (annualised), the instrument's level, for
the ELB. `vintage_levels` holds each vintage's `baseline` columns and its
targets, in display units: `Inflation target`, `Unemployment Rate target`
(that NAIRU estimate) and `Output Growth target` (that potential growth
estimate, as a quarterly log change). Each gap in `vintages` is the level
less its target.

Commitment needs a vintage column for every loss variable selected, so the
tab offers only these five as loss variables. "Unemployment Rate" is offered
only while Gali Unemployment (2010), the only model with it, is the only
model selected. Its baseline is the US unemployment rate (the `baseline`
sheet's "Unemployment Rate", history then the path to the SEP), not the
model's steady state.

The tab measures the current quarter's gaps from its target inputs, which
default to the `targets` sheet: the same targets the vintages use, as they
stand today. So by default the current unemployment gap is from the Fed's
latest NAIRU estimate, and each vintage's from the estimate of its time.

Commitment, timeless commitment and discretion only give different optimal
paths in a model with anticipated policy shocks, with the Anticipation
Parameter above 0. In this dashboard that is Smets-Wouters (see
[Smets-Wouters with anticipated monetary policy shocks](#smets-wouters-with-anticipated-monetary-policy-shocks)).
The other models' policy shocks are always surprises: future policy moves
can't change today's expectations, so the tab shows one optimal path for each
of them.

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

## Smets-Wouters with anticipated monetary policy shocks

The dashboard's Smets-Wouters model is DSGE_mod's replication of Smets and
Wouters (2007), `Smets_Wouters_2007_45.mod`, with 60 anticipated monetary
policy shocks added: `em_1` to `em_60`. It is kept in
[`Smets_Wouters_2007_anticipated/`](Smets_Wouters_2007_anticipated/) rather
than pulled from DSGE_mod.

### Timing

A shock announced i quarters ahead is `em_<i+1>`. It hits the monetary policy
shock process `ms` i quarters after it is announced:

| Shock | Announced |
|---|---|
| `em_1` | 0 quarters ahead: a surprise, the same as the original `em` |
| `em_2` | 1 quarter ahead |
| ... | ... |
| `em_60` | 59 quarters ahead |

This is ezdyn's naming convention for anticipated shocks: ezdyn looks for
`<shock>_1`, `<shock>_2`, ... to build partially anticipated policy paths (see
`ezdyn::get_anticipated_shocks()`).

The shocks enter `ms` in the same place as `em`, its innovation. Announced
shocks wait in news states `ms_news_1` to `ms_news_59`, where `ms_news_j` holds
the shocks due in j quarters:

```
ms         = crhoms*ms(-1) + em + em_1 + ms_news_1(-1)
ms_news_j  = ms_news_{j+1}(-1) + em_{j+1}     (j = 1, ..., 58)
ms_news_59 = em_60
```

### Estimates are unchanged

The anticipated shocks have zero variance and aren't estimated. So the
likelihood at the posterior mode, the smoothed shocks and variables, and the
responses to every other shock are the same as in the original model. The mod
file also drops the original's prior plot of the Phillips curve slope, its
LaTeX output and its `shock_decomposition` call, none of which the dashboard
uses.

### Why 60

Optimal Policy needs one anticipated shock per quarter of its optimisation
horizon, plus one per quarter of timeless-commitment history back to the
commitment start. With the current baseline that is up to 48 + 12 = 60
quarters. When anticipation is on, the tab skips a model with too few and
says so. To change the number, edit `n_anticipated` at the top of
`Smets_Wouters_2007_45_anticipated.mod` and rebuild (see
[Rebuilding the model objects](#rebuilding-the-model-objects)).

### In the dashboard

- **Optimal Policy**: with the Anticipation Parameter above 0, Smets-Wouters
  uses its anticipated shocks, so commitment, timeless commitment and
  discretion give different paths. The other models only have surprise
  policy shocks, so the tab shows one optimal path for each of them.
- **Alternative Paths**: "Use anticipated shocks when supported" uses `em_1`
  to `em_10`, one per quarter of the window.
- **Impulse Response Library**: Custom IRF's cognitive discounting uses
  them. The shocks have no metadata rows, so the shock lists don't offer
  them.
- **Historical Shock Decomposition**: they play no part. The estimation and
  smoother don't use them (zero variance), so their smoothed values are zero,
  and `build_sw_anticipated.m` stops if they aren't. The decomposition is the
  same as the original model's, with zero contributions from the anticipated
  shocks.

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

## Rebuilding the model objects

You only need this to reproduce or change the model objects and metadata in
`inputs/`; they are already built (see [Quick start](#quick-start)). Run these
steps from a clone of this repository, with MATLAB (+ Dynare) and R available
on your PATH. The MATLAB scripts use the Dynare on your MATLAB path, or else
the folder in `dynare_matlab_dir` at the top of each script.

### 1. Smets-Wouters with anticipated shocks (MATLAB)

From the repo root:

```powershell
matlab -batch "run('dashboard_example/Smets_Wouters_2007_anticipated/build_sw_anticipated.m')"
```

This runs `Smets_Wouters_2007_45_anticipated.mod` through Dynare and writes
`inputs/SW_2007_45.json`, plus a CSV export of SW's historical US estimation
sample (`inputs/usmodel_data.csv`). Everything it needs is in
`Smets_Wouters_2007_anticipated/`, so it doesn't need DSGE_mod.

### 2. The DSGE_mod models (MATLAB)

Clone DSGE_mod into `dashboard_example/`:

```powershell
cd dashboard_example
git clone https://github.com/JohannesPfeifer/DSGE_mod.git DSGE_mod
```

The folder must be named exactly `DSGE_mod` (matching what a fresh `git
clone` of that repo produces): `build_new_models.m` looks for it at
`dashboard_example/DSGE_mod`. This folder is a separate git repository (it has
its own `.git`) and is excluded from this repo's git tracking (see
`.gitignore`).

Then, from the repo root:

```powershell
matlab -batch "run('dashboard_example/build_new_models.m')"
```

This runs the 7 curated NK models through Dynare and exports their `M_`/`oo_`
objects as JSON into `inputs/`. It doesn't build Smets-Wouters (step 1 does).

### 3. Build the variable/shock/parameter metadata (R)

From `dashboard_example/`:

```powershell
Rscript build_model_metadata.R
```

This reads the JSON files from steps 1 and 2 and writes one metadata workbook
per model into `inputs/` (Smets-Wouters' is `variable_metadata.xlsx`).

### 4. Build the semi-structural models (R)

From `dashboard_example/`:

```powershell
Rscript build_semi_structural_models.R
```

This downloads McKay and Wolf's (2023) published estimates and writes
`inputs/semi_structural_irfs.csv` and `inputs/semi_structural_metadata.xlsx`
(see [Semi-structural models](#semi-structural-models)). No API key is
needed.

The US baseline is built as in the [Quick start](#quick-start).

## Files

- `app.R` - deploy entrypoint; sources `usa_dashboard.R` and returns the app.
- `usa_dashboard.R` - dashboard composition (paths, timeline, model list,
  `configure_dashboard()` call, `usa_dashboard()` function).
- `fetch_us_fred.R` - fetches the US data and real-time vintages from FRED
  and saves them to `inputs/us_fred_raw.rds` (R; needs a FRED API key).
- `build_us_baseline.R` - builds the US baseline, timeline, targets and
  vintages from `inputs/us_fred_raw.rds` (R; no FRED calls).
- `Smets_Wouters_2007_anticipated/` - Smets-Wouters with anticipated monetary
  policy shocks:
  - `Smets_Wouters_2007_45_anticipated.mod` - the Dynare model (GPL v3, like
    the DSGE_mod original it is based on).
  - `usmodel_data.mat`, `usmodel_mode.mat` - the estimation data and
    posterior mode, copied unchanged from DSGE_mod's `Smets_Wouters_2007/`.
  - `build_sw_anticipated.m` - builds `inputs/SW_2007_45.json` (MATLAB).
- `build_new_models.m` - builds the 7 DSGE_mod models' Dynare JSON objects
  (MATLAB).
- `build_model_metadata.R` - builds every DSGE model's variable/shock/parameter
  metadata workbook (R).
- `build_semi_structural_models.R` - builds the Romer-Romer and
  Gertler-Karadi semi-structural models from McKay and Wolf's (2023) estimates
  (R).
- `inputs/` - the built model objects, metadata workbooks and semi-structural
  responses, stored in this repo. `us_fred_raw.rds` and `us_baseline.xlsx` are
  the exceptions: you build them yourself ([Quick start](#quick-start)) and
  they aren't tracked.
- `DSGE_mod/` - cloned separately ([Rebuilding the model
  objects](#2-the-dsge_mod-models-matlab)), not tracked.
