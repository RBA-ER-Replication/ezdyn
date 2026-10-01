# ezdyn

`ezdyn` is an R package for loading linear dynamic models, calculating impulse
responses, historical shock decompositions, and alternative policy paths, then
plotting the results consistently across one or more models.

It works with Dynare model objects exported by ezDynare and with custom IRF
datasets supplied in a simple long format.

## Getting started
To install and load in models, see [Installation and loading a model](#installation-and-loading-a-model).

You can use ezdyn to: 
1. Building a dashboard
2. Run tasks in code

## Build a dashboard

Simply provide a list of models and a spreadsheet containing forecast (and the dates).

```r
baseline <- import_baseline("baseline.xlsx", source_model = model, models = models)

config <- configure_dashboard(
  name     = "ezdyn Dashboard",
  models   = models,
  timeline = list(
    data_start     = as.Date("2010-03-01"),
    data_end       = as.Date("2026-06-01"),
    forecast_start = as.Date("2026-09-01"),
    forecast_end   = as.Date("2029-06-01")
  ),
  baselines        = list("Baseline Forecast" = baseline),
  default_baseline = "Baseline Forecast"
)

shiny::runApp(ezdyn_dashboard(config))
```

A full example is available in the `example-dashboard` folder.


## Any task can be completed in 2 lines of code
For each functionality:
1. Run the relevant function which gets your results in a nice table.
2. Call `plot_pretty` to automatically plots your results.

Functionalities include: 
- Impulse responses and impulse response matching
- Alternative policy paths
- Optimal policy exercises
- Historical shock decompositions
- Parameter, variable and shock tables


## Impulse responses

`get_irfs_target()` solves for the shock sequence
needed to hit a target path for one or more variables, then returns the
response of every endogenous variable.  

For example: "what shock sequence delivers a
25bp cash-rate cut, held over 8 periods, and what does that imply for inflation and output?"

```r
irfs <- get_irfs_target(
  models, horizon = 16,
  target = list("Cash Rate" = rep(-0.25, 8)), 
  shock_timing = list("Monetary policy shock" = 1:8), # Shocks in period 1-8
  shock_nickname = "Lower cash rate path"
)

plot_pretty(irfs, display_names = c("Cash Rate", "Inflation", "Output"))$graph
```

`get_irfs_target_gabaix()` is the equivalent for partially anticipated
(cognitive-discounting) shocks, taking an additional discount parameter
`lambda`.

`get_irf` reports raw IRFs (not matched to a particular endogenous variable value).

## Alternative policy paths

Alternative path analysis starts from a shared baseline forecast, built once
with `import_baseline()`, and one or more candidate policy-rate paths.

`get_alt_paths()` solves for the shocks needed to hit each candidate path in
every model and returns a nice data frame of the results.

```r
baseline <- import_baseline("baseline.xlsx", source_model = model_a, models = models)

# One quarterly date column plus one cash-rate path column per scenario.
alt_paths <- get_alt_paths(
  alt_paths = readxl::read_excel("alternative_paths.xlsx"),
  models = models,
  baseline = baseline,
  forecast_start = as.Date("2026-03-01"),
  forecast_end = as.Date("2027-12-01"),
  data_start = as.Date("2020-03-01")
)

plot_pretty(alt_paths, display_names = c("Cash rate", "Inflation", "Output"))$graph
```

`use_cd`/`lambda` toggle the partial-anticipation treatment of the policy
shock, for models that support it; a model that cannot generate anticipated
shocks falls back to an ordinary targeted IRF instead of failing.

## Optimal policy

`get_policy_scenario()` solves for the policy path that minimises a given quadratic
loss function, given a baseline and a model's policy impulse responses.

```r
strategy <- list(
  output_name = "Optimal (Model A, commitment)",
  output_variables = c("Cash Rate", "Inflation", "Unemployment"),
  loss_variables = c("Inflation", "Unemployment", "Cash Rate"),
  loss_weights = c(1, 1, 0.5),
  discount_factor = 0.99,
  T_loss = 20,
  T_instrument = 20,
  commit = "commit"
)

optimal <- get_policy_scenario(
  strategy, baseline = baseline, model = model_a,
  forecast_start = "2026-09-01", forecast_end = "2031-06-01"
)

plot_pretty(optimal, display_names = c("Cash rate", "Inflation", "Unemployment"))$graph
```

## Historical shock decomposition

`ez_hd()` converts a Dynare historical shock decomposition -- or a supplied
decomposition, for non-Dynare models -- into a tidy, labelled table.
`plot_pretty_hd()` plots it, optionally aggregating shocks into named groups
via `shock_group`.

```r
hd_a <- ez_hd(model_a$M_, model_a$oo_)

plot_pretty_hd(hd_a, display_names = c("Inflation", "Output"), shock_group="Demand/Supply/Foreign")$graph
```


## Parameter, variable, and shock tables

`get_param_table()`, `get_variable_table()`, and `get_shock_table()` turn a
model's parameters, variables, and shocks into labelled tables from the
metadata workbook. `mode` selects `"katex"` (static, for reports) or `"dt"`
(interactive, for a dashboard); `show_codes = TRUE` adds the model's own raw
code alongside the rendered label.

```r
get_param_table(model_a$M_, type = "Preferences", mode = "katex", dp = 3, as_of = "August 2026")
get_variable_table(model_a$M_, mode = "dt", show_codes = TRUE)
get_shock_table(model_a$M_, mode = "dt")
```

## Installation and loading a model
### Installation

Install `ezdyn` by cloning this repository and running:

```r
pak::pkg_install(".")
```

### Loading a model

A model object consists of: 
1. A model, either: 
- IRFs
- A Dynare model object. You can use ezDynare to export a dynare model object from MATLAB. 
2. **A metadata file**, which translates the model's variables into consistent human readable names and units. 
- See [test_metadata_dynare.xlsx](tests/testthat/fixtures/dynare/test_metadata_dynare.xlsx) for a working example

To load models: 
```r
library(ezdyn)
# Load a model from Dynare
model_a <- read_dynare("model_a.json", path_meta = "variable_metadata.xlsx", model_name = "Model A")

# Load a model from a dataframe of IRFs
model_b <- custom_moo(model_b_irfs, meta = "variable_metadata.xlsx", model_name = "Model B")

models <- list("Model A" = model_a, "Model B" = model_b)
```

## Further information

Full public documentation coming soon.

Use `?read_dynare`, `?custom_moo`, `?get_irf_target`, `?get_alt_paths`, and
`?ez_hd` for full function documentation and input requirements.


## Known issues

- `get_ir_matrix(..., var_names = ...)` errors with `'dims' cannot be of
  length 0` when `var_names` names exactly **one** variable (affects any
  caller that requests a single variable, e.g.
  `build_policy_irfs(model, variable, ...)`). Work around it by requesting
  two or more variables (e.g. `c("r_obs", "dr")` instead of just `"dr"`).
  Not yet fixed.
