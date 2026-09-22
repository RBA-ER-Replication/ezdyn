# ezdyn

`ezdyn` is an R package for loading linear dynamic models, calculating impulse
responses, historical shock decompositions, optimal policy, and alternative policy paths, then
plotting the results consistently across one or more models.

It works with Dynare model objects exported by ezDynare and with custom IRF
datasets supplied in a simple long format.

## The ezdyn ecosystem

- **ezdyn**: R package for loading models, including Dynare models, and
  performing IRF analysis, historical shock decomposition, and alternative policy
  exercises across multiple models.
- **ezdyn dashboard**: an interactive dashboard for working with a configured
  set of models through the `ezdyn` functions.
- **ezDynare**: MATLAB helpers for running Dynare programmatically and
  exporting model objects for use in R.

## Installation

Install `ezdyn` from a local source checkout:

```r
pak::pkg_install(".")
```

The default plotting backend uses the RBA-styled `ggrba` package. `ggrba` is an
optional dependency; when it is not installed, plotting functions automatically
fall back to the portable `ggplot2` backend. Force a specific backend with
`plotter = "ggrba"` or `plotter = "ggplot"` (an explicit `plotter = "ggrba"`
still errors if the package is not installed). The dashboard exposes the same
choice as a "Plot style" control under each tab's Advanced options.

## Model inputs

Most workflows need a model pair and a metadata workbook:

- `M_` holds the model configuration and metadata.
- `oo_` holds numerical results such as Dynare decision rules or supplied IRFs.
- Metadata supplies display names, units, scaling rules, and optional shock
  descriptions.

Load a model exported from Dynare:

```r
library(ezdyn)

model_a <- read_dynare(
  path = "model_a.json",
  path_meta = "variable_metadata.xlsx",
  model_name = "Model A"
)

```

Or load custom IRFs in long format with `t`, `resp_var`, `shock`, and `value`
columns:

```r
model_b <- custom_moo(
  irf = model_b_irfs,
  meta = "variable_metadata.xlsx",
  model_name = "Model B"
)

```

The examples below use `model_a` and `model_b` as full MOO pairs. Replace the
example variable and shock names with the names or metadata descriptions in
your model.

## IRFs and IRF matching

`get_irf()` returns ordinary impulse responses. `get_irf_target()` solves for
the sequence of shocks needed to hit a target path, then returns the response
of every endogenous variable. The same code works for each model, so results
can be combined and plotted together.

To compare several models at once, pass a named list of models to
`get_irfs_target()` (or `get_irf()`'s multi-model counterpart, `get_irfs()`)
instead of calling the single-model function once per model and `rbind()`-ing
the results by hand. `target` and `shock_timing` are resolved independently
for each model using that model's own metadata, so the same call works
whether every model shares a display name/shock description or each has its
own code for the same shock:

```r
models <- list("Model A" = model_a, "Model B" = model_b)

target <- list("Cash rate" = rep(-0.25, 8))
shock_timing <- list("Monetary policy shock" = 1:8)

irfs <- get_irfs_target(
  models,
  horizon = 16,
  target = target,
  shock_timing = shock_timing,
  shock_nickname = "Lower cash-rate path"
)

plot_pretty(
  irfs,
  display_names = c("Cash rate", "Inflation", "Output")
)$graph
```

For an ordinary IRF, replace `get_irfs_target()` with `get_irfs()` and supply
the shock names and horizon:

```r
irfs <- get_irfs(
  models,
  shock_names = "Monetary policy shock",
  horizon = 16
)

plot_pretty(irfs, display_names = c("Inflation", "Output"))$graph
```

`get_irfs_target_gabaix()` is the equivalent multi-model wrapper for
`get_irf_target_gabaix()`, described below.

## Alternative policy paths


Alternative paths start with a shared baseline and one or more paths for the
policy instrument. `import_baseline()` normalises the baseline into the common
display-name and unit space; `get_alt_paths()` calculates results for every
named model; `plot_pretty()` plots the combined output.

```r
models <- list(
  `Model A` = model_a,
  `Model B` = model_b
)

baseline <- import_baseline(
  input = "baseline.xlsx",
  source_model = model_a,
  models = models
)

# The workbook contains one quarterly Date column and one numeric cash-rate
# path column for each alternative scenario.
alternative_paths <- readxl::read_excel("alternative_paths.xlsx")

paths <- get_alt_paths(
  alt_paths = alternative_paths,
  models = models,
  baseline = baseline,
  forecast_start = as.Date("2026-03-01"),
  forecast_end = as.Date("2027-12-01"),
  data_start = as.Date("2020-03-01")
)

plot_pretty(
  paths,
  display_names = c("Cash rate", "Inflation", "Output")
)$graph
```

## Historical shock decompositions

`ez_hd()` converts a Dynare historical shock decomposition, or supplied
decomposition data, into a tidy table with metadata labels and units.
`plot_pretty_hd()` then produces an (optionally interactive) contribution chart.

```r
hd_a <- ez_hd(model_a$M_, model_a$oo_)

plot_pretty_hd(
  hd_a,
  display_names = c("Inflation", "Output")
)$graph
```

To use a decomposition produced elsewhere, supply a long data frame with `t`,
`shock`, `variable`, and `value` columns:

```r
custom_decomposition <- data.frame(
  t = 1:8,
  shock = "Demand shock",
  variable = "output",
  value = 0
)

hd_b <- ez_hd(model_b$M_, hd_override = custom_decomposition)

plot_pretty_hd(
  hd_b,
  display_names = c("Inflation", "Output")
)$graph
```
## Parameter, variable, and shock tables

`get_param_table()` turns the parameter values in a model object into a
table. When the metadata workbook includes a `parameters` sheet, the table
can use parameter labels, descriptions, sectors, types, and notes. Use `type`
to show one parameter group, or omit it to show all parameters. `mode`
selects the output format: `"katex"` (the default) returns a static, styled
HTML table suited to reports; `"dt"` returns an interactive, sortable/
searchable table suited to a live dashboard. `show_codes = TRUE` adds the
model's own raw parameter code alongside the rendered symbol, for users who
need to match a displayed parameter back to the underlying model code.

```r
get_param_table(
  model_a$M_,
  type = "Preferences",
  mode = "katex",
  dp = 3,
  as_of = "August 2026"
)
```

`param_table_katex()` is a deprecated alias for
`get_param_table(mode = "katex")`, kept only so existing callers keep
working unchanged.

`get_variable_table()` and `get_shock_table()` are analogous functions for a
model's variables and shocks: they list the display names/descriptions a
model's metadata defines (the same information used to label plots and IRF
selectors), with the same `mode`/`show_codes` arguments as
`get_param_table()`. Shocks without a defined description fall back to
showing their model code.

```r
get_variable_table(model_a$M_, mode = "dt", show_codes = TRUE)
get_shock_table(model_a$M_, mode = "dt")
```

The ezdyn dashboard's Variable Dictionary tab lets users pick one or more
configured models and browse all three tables (in `"dt"` mode) side by side.

## Known issues

- `get_ir_matrix(..., var_names = ...)` (and anything that calls it with a
  single requested variable, e.g. `build_policy_irfs(model, variable, ...)`)
  errors with `'dims' cannot be of length 0` when `var_names` names exactly
  **one** variable. Subsetting the IRF array down to one row collapses a
  dimension that `get_ir_matrix()`'s internal `irf_for_shock()` still expects,
  in `array(0, dim = dim(base_irf[, , shock_name]), ...)`. Work around it by
  requesting two or more variables (e.g. `c("r_obs", "dr")` instead of just
  `"dr"`). Not yet fixed.

## Planned enhancements (TODO)

- **Alternative Paths model selection.** The Alternative Paths tab always
  calls `get_alt_paths(models = config$models, ...)` with every registered
  model (`dash_alt_paths_tab.R`), with no UI control to narrow this down. For
  dashboards with several heterogeneous models (e.g. DSGEModDash's SW +
  curated Pfeifer NK models), this means every alt-path run computes
  responses for models that may not even share the requested response
  variable/instrument, which is unnecessary and slower than it needs to be.
  Add a model-picker input (defaulting to all models, or perhaps to only
  those exposing the current instrument) so users can select which
  registered models are "applicable" to a given Alternative Paths exercise.

## Further information

Full public documentation coming soon.

Use `?read_dynare`, `?custom_moo`, `?get_irf_target`, `?get_alt_paths`, and
`?ez_hd` for full function documentation and input requirements.
