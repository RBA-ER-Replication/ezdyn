# The dashboard's models

The models behind the [example dashboard](../README.md): how they were built,
how each is defined, and how to rebuild them. You don't need any of this to run
the dashboard, as the built models are already in `../inputs/`.

> **Illustrative only.** These models, and the data they're used with, are
> purely for illustrating what ezdyn can do. They have not had close quality
> control. The code that loads the DSGE_mod models in particular is
> experimental.

The dashboard combines the Smets-Wouters (2007) estimated model, extended with
anticipated monetary policy shocks, with 7 curated small-scale New Keynesian
models from Johannes Pfeifer's
[DSGE_mod](https://github.com/JohannesPfeifer/DSGE_mod) collection of Dynare
replication files, plus two semi-structural models: estimated responses to the
Romer-Romer (2004) and Gertler-Karadi (2015) monetary policy shocks, from
McKay and Wolf (2023). Every model has an active monetary policy shock, so the
dashboard can be used for cross-model impulse-response policy exercises.

## Models

| Dashboard name | Source | Built by |
|---|---|---|
| Smets Wouters (2007) | This repository: [`Smets_Wouters_2007_anticipated/`](Smets_Wouters_2007_anticipated/), a version of DSGE_mod's `Smets_Wouters_2007/Smets_Wouters_2007_45.mod` with anticipated monetary policy shocks (see [Smets-Wouters with anticipated monetary policy shocks](#smets-wouters-with-anticipated-monetary-policy-shocks)) | `Smets_Wouters_2007_anticipated/build_sw_anticipated.m` |
| Gali NK (Ch.3) | DSGE_mod: `Gali_2015/Gali_2015_chapter_3.mod` | `build_new_models.m` |
| Gali NK+Wage (Ch.6, anticipated) | Bundled news-augmented version of the same Chapter 6 model | `Gali_2015_chapter_6_anticipated/build_gali_anticipated.m` |
| Gali SOE (Ch.8) | DSGE_mod: `Gali_2015/Gali_2015_chapter_8.mod` | `build_new_models.m` |
| Gali Unemployment (2010) | DSGE_mod: `Gali_2010/Gali_2010.mod` | `build_new_models.m` |
| Ireland (2004) | DSGE_mod: `Ireland_2004/Ireland_2004.mod` | `build_new_models.m` |
| Ascari-Sbordone (2014) | DSGE_mod: `Ascari_Sbordone_2014/Ascari_Sbordone_2014.mod` | `build_new_models.m` |
| Born-Pfeifer (2018) | DSGE_mod: `Born_Pfeifer_2018/Monetary_Policy_IRFs/Born_Pfeifer_2018_MP.mod` | `build_new_models.m` |
| Romer-Romer (2004), Gertler-Karadi (2015) | McKay and Wolf's (2023) published estimates (see [Semi-structural models](#semi-structural-models)) | `build_semi_structural_models.R` |
| DINGO | The RBA's DINGO export, with historical estimates removed (see [DINGO](#dingo)) | `build_dingo_model.R` |

`build_new_models.m` runs the 7 DSGE_mod models from a clone of DSGE_mod. The
Smets-Wouters model is not pulled from DSGE_mod: its anticipated-shocks
version, with the data and posterior mode it needs, is kept in this
repository. `build_model_metadata.R` builds every DSGE model's
variable/shock/parameter metadata workbook.

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
| `dy_ye` | Output Growth (year-ended) | Sum of the last four quarters of `dy` |
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
- **Optimal Policy**: Romer-Romer (2004) and Gertler-Karadi (2015) are
  selectable. While either is selected, the tab
  doesn't offer response or loss variables it lacks (e.g. real wage growth).
  Because output and inflation rise at first after a GK rate rise, GK's
  optimal path can move rates the opposite way to SW's in the short run.
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
[Rebuilding the models](#rebuilding-the-models)).

### In the dashboard

- **Optimal Policy**: SW is selectable; Gali NK+Wage (Ch.6, anticipated) and
  DINGO are selected by default. With the
  Anticipation Parameter above 0 (0.8 by default), Smets-Wouters uses its
  anticipated shocks, so commitment, timeless commitment and discretion give
  different paths. The anticipated Gali model supports these strategies too.
  Models with only surprise policy shocks show one optimal path each. The tab plots year-ended
  inflation and output growth and the nominal interest rate by default.
- **Alternative Paths**: "Use anticipated shocks when supported" uses `em_1`
  to `em_10`, one per quarter of the window, with the cognitive discounting
  parameter at 0.8 by default. It plots the same three variables by default.
- **Impulse Response Library**: Custom IRF's cognitive discounting uses
  them. The shocks have no metadata rows, so the shock lists don't offer
  them.
- **Historical Shock Decomposition**: they play no part. The estimation and
  smoother don't use them (zero variance), so their smoothed values are zero,
  and `build_sw_anticipated.m` stops if they aren't. The decomposition is the
  same as the original model's, with zero contributions from the anticipated
  shocks. They have no metadata rows, so every shock grouping puts them in
  "Other", with a zero contribution.
- **Shock groupings**: SW's metadata offers two: "Demand/Supply/Monetary",
  and "All Shocks", which shows each shock on its own. "All Shocks" would put
  any measurement errors (`is_measurement_error`) together in one
  "Measurement errors" group, but SW has none: it is estimated with seven
  structural shocks for its seven observables.

## Gali NK+Wage with anticipated monetary policy shocks

`Gali_2015_chapter_6_anticipated/` contains a bundled version of Johannes
Pfeifer's `Gali_2015/Gali_2015_chapter_6.mod`, retaining its source header.
It preserves the baseline calibration (both price and wage stickiness are
0.75) and omits the upstream flexible-price/wage experiments and plotting.
The anticipated version replaces the original NK+Wage entry in both dashboard
scripts. The original JSON is retained unchanged as a regression reference.

The policy shock process is augmented in the same way as Smets-Wouters:

```text
nu         = rho_nu*nu(-1) + eps_nu + eps_nu_1 + nu_news_1(-1)
nu_news_j  = nu_news_{j+1}(-1) + eps_nu_{j+1}  (j = 1, ..., 58)
nu_news_59 = eps_nu_60
```

`eps_nu_1` is immediate; `eps_nu_60` arrives 59 quarters after announcement.
The innovation subsequently inherits `rho_nu = 0.5`. All 60 added shocks
have zero variance. They support Custom IRFs, Alternative Paths and Optimal
Policy; they do not introduce estimation or historical decomposition data.
The existing Chapter 6 metadata is reused, including the annualised interest
rate scale of 4, quarterly inflation alias and derived growth variables.

From this model's folder in MATLAB, with Dynare on the path, run:

```matlab
build_gali_anticipated()
```

This rebuilds `../../inputs/Gali2015Ch6_anticipated.json` from the bundled
mod file without downloading anything. Tested with MATLAB R2024a and Dynare
7.2; the builder uses the local Windows Dynare 7.2 installation as a fallback.
To regenerate the bundled mod from a local DSGE_mod checkout, pass its
`Gali_2015_chapter_6.mod` path as the optional argument. The builder validates
the expected source equation before modifying it. For ezdyn 0.3.23, it
exports the legacy `kstate` lag index from Dynare 7.2's `state_var` ordering;
this does not change the solved decision rule.

From the repository root, run the VS Code task **Test anticipated Gali NK
wage model**, or:

```text
Rscript functions/ezdyn/dashboard_example/create_models/Gali_2015_chapter_6_anticipated/test-gali-anticipated.R
```

The checks cover calibration, original-shock equivalence, all 60 news
timings, equilibrium equations, displayed-unit targets, 60-quarter policy
matrices, three-model alternative paths and commitment/discretion scenarios,
plot/export data, and the dashboard with DINGO (including that DINGO is not
offered for historical shock decompositions).

## DINGO

`inputs/DINGO.json` is the RBA's DINGO model export with every historical
smoothing and filtering field removed (`Smoother`, `SmoothedVariables`,
`UpdatedVariables`, `SmoothedShocks` and the initial-value series). It therefore
has no historical shock decomposition. Its solved decision rules, calibration
and estimated parameters are unchanged, and it keeps DINGO's anticipated
monetary policy shocks.

`inputs/DINGO_metadata.xlsx` is DINGO's metadata with shared concepts renamed
to this dashboard's names and units: for example, Cash Rate becomes Nominal
Interest Rate, GDP becomes Output, and trimmed-mean inflation becomes
Inflation. Scale factors are unchanged. DINGO's inflation is still
trimmed-mean inflation, which the dashboard notes beneath its responses.

Rebuild both from the monorepo's DINGO inputs, which are not modified, by
running `Rscript create_models/build_dingo_model.R` from `dashboard_example/`.
The script checks that every retained model field reads back exactly.

## Common variables

Every DSGE model (Smets-Wouters and the 7 Pfeifer models) has these nine
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
| Output Growth (year-ended) | Per cent (year-ended) |

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
- "Output Growth (year-ended)" is output growth over the last four quarters:
  the sum of the last four quarters of SW's and Ireland's own output growth
  (the `year_ended_sum` transform), and in every other model the change in
  "Output" over four quarters (the `year_ended_change` transform), which is
  the same thing (ezdyn can't derive a variable from a derived one).
- Every other interest rate is annualised the same way (x 4): the Gali
  models', Ascari-Sbordone's and Born-Pfeifer's real and natural rates, and
  SW's model-consistent nominal rate `r`.

Because the policy instrument is annualised, the Alternative Paths sliders,
Custom IRF targets and Optimal Policy rate settings (loss targets, bounds and
the 0.1% ELB) are all in annual per cent.

The semi-structural models also have "Inflation (year-ended)", "Nominal
Interest Rate (quarterly)", "Nominal Interest Rate Changes", "Output Growth"
and "Output Growth (year-ended)" (see their Variables table above).

| Model | Inflation | Nominal Interest Rate | Hours Worked | Output |
|---|---|---|---|---|
| Smets-Wouters (2007) | `pinfobs` | `robs` | `labobs` | `y` |
| Gali NK (Ch.3) | `pi` | `i` | `n` | `y` |
| Gali NK+Wage (Ch.6, anticipated) | `pi_p` | `i` | `n` | `y` |
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
  With no US data it isn't in the baseline, so it's only in the Impulse
  Response Library, not Alternative Paths or Optimal Policy.

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
tabs list only variables offered by every selected model. Alternative Paths
and Optimal Policy also only offer variables that are in the baseline, i.e.
that have US data or are derived from it. So in Alternative Paths, SW offers
its seven observables and the variables derived from them (e.g. "Inflation
(year-ended)"); the other models offer the variables they share with that
data.

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

## Rebuilding the models

You only need this to reproduce or change the models and metadata in
`../inputs/`; they are already built. You need MATLAB with Dynare for steps 1
and 2, and R for steps 3 and 4. The MATLAB scripts use the Dynare on your
MATLAB path, or else the folder in `dynare_matlab_dir` at the top of each
script.

The MATLAB scripts run from their own folder; the R scripts run with the
working directory set to `dashboard_example/` (the folder containing
`app.R`). Every script writes to `dashboard_example/inputs/`.

### 1. Smets-Wouters with anticipated shocks (MATLAB)

From `create_models/Smets_Wouters_2007_anticipated/`, in MATLAB:

```matlab
build_sw_anticipated
```

(or `matlab -batch "build_sw_anticipated"` from that folder). This runs
`Smets_Wouters_2007_45_anticipated.mod` through Dynare and writes
`inputs/SW_2007_45.json`, plus a CSV export of SW's historical US estimation
sample (`inputs/usmodel_data.csv`). Everything it needs is in
`Smets_Wouters_2007_anticipated/`, so it doesn't need DSGE_mod.

### 2. The DSGE_mod models (MATLAB)

Clone DSGE_mod into `create_models/`. From `create_models/`:

```powershell
git clone https://github.com/JohannesPfeifer/DSGE_mod.git DSGE_mod
```

The folder must be named exactly `DSGE_mod`: `build_new_models.m` looks for
it at `create_models/DSGE_mod`. It is a separate git repository (it has its
own `.git`) and is excluded from this repository (see `../.gitignore`).

Then, from `create_models/`, in MATLAB:

```matlab
build_new_models
```

(or `matlab -batch "build_new_models"` from that folder). This runs the 7
curated NK models through Dynare and exports their `M_`/`oo_` objects as JSON
into `inputs/`. It doesn't build Smets-Wouters (step 1 does).

### 3. Build the variable/shock/parameter metadata (R)

From `dashboard_example/`:

```powershell
Rscript create_models/build_model_metadata.R
```

This reads the JSON files from steps 1 and 2 and writes one metadata workbook
per model into `inputs/` (Smets-Wouters' is `variable_metadata.xlsx`). It
needs `dplyr`, `tibble`, `jsonlite` and `writexl`.

### 4. Build the semi-structural models (R)

From `dashboard_example/`:

```powershell
Rscript create_models/build_semi_structural_models.R
```

This downloads McKay and Wolf's (2023) published estimates (about 46MB, from
`raw.githubusercontent.com`) and writes one workbook of impulse responses per
model, `inputs/RomerRomer2004_irfs.xlsx` and
`inputs/GertlerKaradi2015_irfs.xlsx`, plus their shared
`inputs/semi_structural_metadata.xlsx` (see [Semi-structural
models](#semi-structural-models)). No API key is needed. It needs `dplyr`,
`tibble`, `purrr`, `tidyr`, `R.matlab` and `writexl`.

## Files in this folder

- `build_new_models.m` - builds the 7 DSGE_mod models' Dynare JSON objects
  (MATLAB).
- `Smets_Wouters_2007_anticipated/` - Smets-Wouters with anticipated monetary
  policy shocks:
  - `Smets_Wouters_2007_45_anticipated.mod` - the Dynare model (GPL v3, like
    the DSGE_mod original it is based on).
  - `usmodel_data.mat`, `usmodel_mode.mat` - the estimation data and
    posterior mode, copied unchanged from DSGE_mod's `Smets_Wouters_2007/`.
  - `build_sw_anticipated.m` - builds `inputs/SW_2007_45.json` (MATLAB).
- `build_model_metadata.R` - builds every DSGE model's variable/shock/parameter
  metadata workbook (R).
- `build_semi_structural_models.R` - builds the Romer-Romer and
  Gertler-Karadi semi-structural models from McKay and Wolf's (2023) estimates
  (R).
- `DSGE_mod/` - cloned separately ([step 2](#2-the-dsge_mod-models-matlab)),
  not tracked.
