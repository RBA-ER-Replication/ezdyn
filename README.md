# ezdyn

`ezdyn` is an R package for loading linear dynamic models, calculating impulse
responses, historical shock decompositions, and alternative policy paths, then
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
optional dependency, so install it separately when needed. Use the portable
`ggplot2` backend with `plotter = "ggplot"`.

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

```r
target <- list("Cash rate" = rep(-0.25, 8))
shock_timing <- list("Monetary policy shock" = 1:8)

irfs_a <- get_irf_target(
  model_a$M_, model_a$oo_,
  horizon = 16,
  target = target,
  shock_timing = shock_timing,
  pretty = TRUE,
  shock_nickname = "Lower cash-rate path"
)

irfs_b <- get_irf_target(
  model_b$M_, model_b$oo_,
  horizon = 16,
  target = target,
  shock_timing = shock_timing,
  pretty = TRUE,
  shock_nickname = "Lower cash-rate path"
)

plot_pretty(
  rbind(irfs_a, irfs_b),
  display_names = c("Cash rate", "Inflation", "Output")
)$graph
```

For an ordinary IRF, replace `get_irf_target()` with `get_irf()` and supply the
shock names and horizon:

```r
irf <- get_irf(
  model_a$M_, model_a$oo_,
  shock_names = "Monetary policy shock",
  horizon = 16,
  pretty = TRUE
)

plot_pretty(irf, display_names = c("Inflation", "Output"))$graph
```

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
## Parameter tables

`param_table_katex()` turns the parameter values in a model object into a
formatted HTML table. When the metadata workbook includes a `parameters` sheet,
the table can use parameter labels, descriptions, sectors, types, and notes.
Use `type` to show one parameter group, or omit it to show all parameters.

```r
param_table_katex(
  model_a$M_,
  type = "Preferences",
  dp = 3,
  as_of = "August 2026"
)
```

## Further information

Full public documentation (and release of dashboard code) coming soon.

Use `?read_dynare`, `?custom_moo`, `?get_irf_target`, `?get_alt_paths`, and
`?ez_hd` for full function documentation and input requirements.