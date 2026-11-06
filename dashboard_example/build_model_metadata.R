# Build variable/shock/parameter metadata workbooks for all 8 models in this
# dashboard: Smets-Wouters (2007) plus 7 curated Pfeifer DSGE_mod NK models
# (see build_new_models.m / README.md for how the underlying Dynare JSON
# model objects are built - this script reads inputs/SW_2007_45.json for SW's
# parameter values, so build_new_models.m must be run first).
#
# Display names are shared ACROSS models wherever the underlying concept and
# units genuinely match (e.g. "Nominal Interest Rate", "Inflation", "Hours
# Worked", "Output Growth") - this is what lets the dashboard's Impulse
# Response Library overlay the same variable's response across multiple
# models. Units are aligned to a shared convention ("Per cent (quarterly)"
# for inflation-type flow variables, "Per cent deviation from steady state"
# for gap/level variables) - each of the 7 new models' own RAW
# (non-annualized) quarterly variable is used rather than its "_ann"
# counterpart, so no rescaling is needed to make units match SW's own
# convention. The exception is interest rates: every model shows its nominal,
# real and natural rates annualised (scale factor 4 on the quarterly rate,
# "Per cent (annualized)"). Only a handful of other display names (e.g. "Money Growth", only ever
# available as "_ann" in the source files) are annualized, but those are not
# shared with any other model's display name, so there is no unit conflict
# (ezdyn's `validate_display_names()` only errors when the SAME display name
# is used with DIFFERENT units across models).
#
# Every model provides the same six common variables: "Inflation",
# "Inflation (year-ended)", "Nominal Interest Rate", "Nominal Interest Rate
# (quarterly)", "Hours Worked" and "Output". Where a model's own variable has
# a different name (e.g. "Price Inflation"), a derived alias row
# (`derived_from`, no transform) adds the common name alongside it.
# "Inflation (year-ended)" is the four-quarter sum of each model's quarterly
# inflation. "Nominal Interest Rate" is each model's quarterly nominal rate
# annualised (x 4), and "Nominal Interest Rate (quarterly)" is the quarterly
# rate itself. See README.md ("Common variables") for how each model gets
# each variable.
#
# The `alt_paths`, `irf` and `hsd` columns choose which dashboard tabs offer
# a variable (Alternative Paths, the Impulse Response Library, Historical
# Shock Decomposition).
#
# Run from this directory: Rscript build_model_metadata.R

library(dplyr)
library(tibble)
library(jsonlite)
library(writexl)

# Fill in the metadata columns ezdyn reads, keeping any value a row already
# sets (e.g. a derived row's `derived_from`). None of the 7 Pfeifer models
# has smoothed historical data, so `hsd` defaults to FALSE.
common_cols <- function(df) {
  defaults <- list(
    scale_formula = NA_character_,
    derived_from = NA_character_,
    derived_transform = NA_character_,
    default = TRUE,
    alt_paths = TRUE,
    irf = TRUE,
    hsd = FALSE
  )
  for (col in names(defaults)) {
    value <- if (col %in% names(df)) df[[col]] else NA
    df[[col]] <- coalesce(value, defaults[[col]])
  }
  df
}

# Insert `rows` directly after the row for model variable `after`. The
# dashboard's selectors follow metadata row order, so this lists each derived
# variable next to its source.
insert_after <- function(df, after, rows) {
  at <- match(after, df$dynare_name)
  bind_rows(df[seq_len(at), ], rows, df[-seq_len(at), ])
}

# Show quarterly interest rates `rates` annualised: scale them by 4, on top
# of any scaling of their own (`own_scale`).
annualise <- function(df, rates, own_scale = NULL) {
  at <- df$dynare_name %in% rates
  if (!("scale_formula" %in% names(df))) df$scale_formula <- NA_character_
  df$units[at] <- "Per cent (annualized)"
  df$scale_formula[at] <- paste(c("4", own_scale), collapse = " * ")
  df
}

# Show the model's quarterly nominal rate `rate` annualised, as "Nominal
# Interest Rate". A derived "Nominal Interest Rate (quarterly)" row scales it
# back by 1/4 (applied after the rate's own scale factor).
annualise_rate <- function(df, rate, own_scale = NULL) {
  df |>
    annualise(rate, own_scale) |>
    insert_after(rate, tibble(
      dynare_name = paste0(rate, "_q"),
      display_name = "Nominal Interest Rate (quarterly)",
      units = "Per cent (quarterly)",
      scale_formula = "0.25",
      derived_from = rate
    ))
}

# A common display name for an existing model variable `source`.
alias_row <- function(dynare_name, display_name, units, source) {
  tibble(dynare_name = dynare_name, display_name = display_name, units = units, derived_from = source)
}

# "Inflation (year-ended)": the sum of the last four quarters of the model's
# quarterly inflation variable `inflation`. Every model's inflation is a log
# change in the price level, so the sum is exactly the log change over the
# year. `inflation` must be a model variable, not an alias row (ezdyn doesn't
# chain derived variables).
year_ended_inflation_row <- function(inflation) {
  tibble(
    dynare_name = paste0(inflation, "_ye"),
    display_name = "Inflation (year-ended)",
    units = "Per cent (year-ended)",
    derived_from = inflation,
    derived_transform = "year_ended_sum"
  )
}

# 0. Smets_Wouters_2007_45 ----------------------------------------------------
# Descriptions are taken directly from the `long_name` annotations in
# DSGE_mod/Smets_Wouters_2007/Smets_Wouters_2007_45.mod. Parameter values are
# read back from the model JSON produced by build_new_models.m, so they
# always match the actual calibration used by the exported MOO object (not
# hand-transcribed).
sw_model_json <- fromJSON(readLines("inputs/SW_2007_45.json"), simplifyVector = TRUE)
sw_param_names <- unlist(sw_model_json$M_$param_names)
sw_param_values <- as.numeric(sw_model_json$M_$params)

# Seven observables (already in the model's display units - per cent, either
# quarterly growth/level or deviation from steady state) plus their
# model-consistent (pre-measurement-equation) counterparts for finer-grained
# IRF exploration. `alt_paths`/`hsd` are TRUE only for the observables, to
# keep the Alternative Paths and Historical Shock Decomposition pickers free
# of near-duplicate entries; the model-consistent series stay available in
# the Impulse Response Library. The exception is output: SW has no output
# level observable (only growth), so `y` is labelled plain "Output" and
# offered in Alternative Paths as the common output variable.
sw_variables <- tribble(
  ~dynare_name, ~display_name,                          ~units,                                    ~alt_paths, ~hsd,
  "dy",         "Output Growth",                        "Per cent (quarterly)",                    TRUE,       TRUE,
  "dc",         "Consumption Growth",                   "Per cent (quarterly)",                     TRUE,       TRUE,
  "dinve",      "Investment Growth",                    "Per cent (quarterly)",                     TRUE,       TRUE,
  "dw",         "Real Wage Growth",                     "Per cent (quarterly)",                     TRUE,       TRUE,
  "pinfobs",    "Inflation",                            "Per cent (quarterly)",                     TRUE,       TRUE,
  "robs",       "Nominal Interest Rate",                "Per cent (quarterly)",                     TRUE,       TRUE,
  "labobs",     "Hours Worked",                         "Per cent deviation from steady state",      TRUE,       TRUE,
  "y",          "Output",                               "Per cent deviation from steady state",      TRUE,       FALSE,
  "c",          "Consumption (model-consistent)",       "Per cent deviation from steady state",      FALSE,      FALSE,
  "inve",       "Investment (model-consistent)",        "Per cent deviation from steady state",      FALSE,      FALSE,
  "lab",        "Hours Worked (model-consistent)",       "Per cent deviation from steady state",      FALSE,      FALSE,
  "pinf",       "Inflation (model-consistent)",         "Per cent deviation from steady state",      FALSE,      FALSE,
  "w",          "Real Wage (model-consistent)",         "Per cent deviation from steady state",      FALSE,      FALSE,
  "r",          "Nominal Interest Rate (model-consistent)", "Per cent deviation from steady state",  FALSE,      FALSE,
  "mc",         "Marginal Cost",                        "Per cent deviation from steady state",      FALSE,      FALSE
) |>
  annualise_rate("robs") |>
  annualise("r") |>
  insert_after("pinfobs", year_ended_inflation_row("pinfobs")) |>
  common_cols()

# `Demand/Supply/Monetary` is a shock-grouping column (any column besides
# `shock`/`description`/`is_measurement_error` is picked up automatically by
# `ez_hd.available_shock_groupings()` and offered as a "Shock grouping:"
# choice in the dashboard's Historical Shock Decomposition tab).
sw_shocks <- tribble(
  ~shock,   ~description,                            ~`Demand/Supply/Monetary`,
  "ea",     "Productivity shock",                     "Supply",
  "eb",     "Risk premium shock",                     "Demand",
  "eg",     "Government spending shock",               "Demand",
  "eqs",    "Investment-specific technology shock",    "Supply",
  "em",     "Monetary policy shock",                   "Monetary",
  "epinf",  "Price markup shock",                      "Supply",
  "ew",     "Wage markup shock",                       "Supply"
)

# Parameters sheet (documentation only - ezdyn ignores this sheet)
sw_parameter_descriptions <- tribble(
  ~dynare_name, ~description,
  "curvw",      "Curvature of the Kimball aggregator, wages",
  "cgy",        "Feedback of technology shock on exogenous spending",
  "curvp",      "Curvature of the Kimball aggregator, prices",
  "constelab",  "Steady-state hours worked",
  "constepinf", "Steady-state quarterly inflation rate (per cent)",
  "constebeta", "Time preference rate, 100*(beta^-1 - 1)",
  "cmaw",       "Coefficient on MA term, wage markup shock",
  "cmap",       "Coefficient on MA term, price markup shock",
  "calfa",      "Capital share in production",
  "czcap",      "Capacity utilisation adjustment cost",
  "csadjcost",  "Investment adjustment cost",
  "ctou",       "Capital depreciation rate",
  "csigma",     "Risk aversion (inverse elasticity of intertemporal substitution)",
  "chabb",      "External habit persistence",
  "ccs",        "Unused parameter",
  "cinvs",      "Unused parameter",
  "cfc",        "Fixed-cost share / gross price markup",
  "cindw",      "Wage indexation to past inflation",
  "cprobw",     "Calvo probability, wages",
  "cindp",      "Price indexation to past inflation",
  "cprobp",     "Calvo probability, prices",
  "csigl",      "Inverse Frisch elasticity of labour supply",
  "clandaw",    "Steady-state gross wage markup",
  "crdpi",      "Unused parameter",
  "crpi",       "Taylor rule inflation feedback",
  "crdy",       "Taylor rule output-growth feedback",
  "cry",        "Taylor rule output-gap feedback",
  "crr",        "Taylor rule interest-rate smoothing (persistence)",
  "crhoa",      "Persistence, productivity shock",
  "crhoas",     "Unused parameter",
  "crhob",      "Persistence, risk premium shock",
  "crhog",      "Persistence, spending shock",
  "crhols",     "Unused parameter",
  "crhoqs",     "Persistence, investment-specific technology shock",
  "crhoms",     "Persistence, monetary policy shock",
  "crhopinf",   "Persistence, price markup shock",
  "crhow",      "Persistence, wage markup shock",
  "ctrend",     "Net growth rate, per cent",
  "cg",         "Steady-state exogenous spending share of output"
)

sw_parameters <- tibble(dynare_name = sw_param_names, value = sw_param_values) |>
  left_join(sw_parameter_descriptions, by = "dynare_name") |>
  mutate(description = coalesce(description, "")) |>
  select(dynare_name, description, value)

write_xlsx(
  list(variables = sw_variables, shocks = sw_shocks, parameters = sw_parameters),
  path = "inputs/variable_metadata.xlsx"
)
cat("build_model_metadata: wrote inputs/variable_metadata.xlsx\n")

# 1. Gali_2015_chapter_3 - flagship baseline 3-equation NK model -------------
gali2015ch3_variables <- tribble(
  ~dynare_name,   ~display_name,                    ~units,
  "i",            "Nominal Interest Rate",          "Per cent (quarterly)",
  "pi",           "Inflation",                      "Per cent (quarterly)",
  "r_real",       "Real Interest Rate",             "Per cent (quarterly)",
  "r_nat",        "Natural Interest Rate",          "Per cent (quarterly)",
  "y_gap",        "Output Gap",                     "Per cent deviation from steady state",
  "y",            "Output",                         "Per cent deviation from steady state",
  "n",            "Hours Worked",                   "Per cent deviation from steady state",
  "w_real",       "Real Wage",                      "Per cent deviation from steady state",
  "p",            "Price Level",                    "Per cent deviation from steady state",
  "m_real",       "Real Money Stock",               "Per cent deviation from steady state",
  "m_growth_ann", "Money Growth",                   "Per cent (annualized)",
  "mu",           "Price Markup",                   "Per cent deviation from steady state",
  "a",            "Technology Shock Process",       "Per cent deviation from steady state",
  "z",            "Preference Shock Process",       "Per cent deviation from steady state",
  "nu",           "Monetary Policy Shock Process",  "Per cent deviation from steady state"
) |>
  annualise_rate("i") |>
  annualise(c("r_real", "r_nat")) |>
  insert_after("pi", year_ended_inflation_row("pi")) |>
  common_cols()

gali2015ch3_shocks <- tribble(
  ~shock,     ~description,
  "eps_a",    "Technology shock",
  "eps_nu",   "Monetary policy shock",
  "eps_z",    "Preference shock"
)

# 2. Gali_2015_chapter_6 - NK model with price AND wage rigidities -----------
gali2015ch6_variables <- tribble(
  ~dynare_name,   ~display_name,                    ~units,
  "i",            "Nominal Interest Rate",          "Per cent (quarterly)",
  "pi_p",         "Price Inflation",                "Per cent (quarterly)",
  "pi_w",         "Wage Inflation",                 "Per cent (quarterly)",
  "r_real",       "Real Interest Rate",             "Per cent (quarterly)",
  "r_nat",        "Natural Interest Rate",          "Per cent (quarterly)",
  "y_gap",        "Output Gap",                     "Per cent deviation from steady state",
  "y",            "Output",                         "Per cent deviation from steady state",
  "n",            "Hours Worked",                   "Per cent deviation from steady state",
  "w_real",       "Real Wage",                      "Per cent deviation from steady state",
  "w_gap",        "Real Wage Gap",                  "Per cent deviation from steady state",
  "mu_p",         "Price Markup",                   "Per cent deviation from steady state",
  "m_real",       "Real Money Stock",               "Per cent deviation from steady state",
  "m_growth_ann", "Money Growth",                   "Per cent (annualized)",
  "a",            "Technology Shock Process",       "Per cent deviation from steady state",
  "z",            "Preference Shock Process",       "Per cent deviation from steady state",
  "nu",           "Monetary Policy Shock Process",  "Per cent deviation from steady state"
) |>
  annualise_rate("i") |>
  annualise(c("r_real", "r_nat")) |>
  # Price inflation is the model's inflation rate (wage inflation is separate).
  insert_after("pi_p", alias_row("inflation", "Inflation", "Per cent (quarterly)", "pi_p")) |>
  insert_after("inflation", year_ended_inflation_row("pi_p")) |>
  common_cols()

gali2015ch6_shocks <- tribble(
  ~shock,     ~description,
  "eps_a",    "Technology shock",
  "eps_nu",   "Monetary policy shock",
  "eps_z",    "Preference shock"
)

# 3. Gali_2015_chapter_8 - small open economy NK model -----------------------
gali2015ch8_variables <- tribble(
  ~dynare_name,   ~display_name,                    ~units,
  "i",            "Nominal Interest Rate",          "Per cent (quarterly)",
  "pi",           "Inflation",                      "Per cent (quarterly)",
  "pi_h",         "Domestic Inflation",             "Per cent (quarterly)",
  "r_real",       "Real Interest Rate",             "Per cent (quarterly)",
  "r_nat",        "Natural Interest Rate",          "Per cent (quarterly)",
  "y_gap",        "Output Gap",                     "Per cent deviation from steady state",
  "y",            "Output",                         "Per cent deviation from steady state",
  "n",            "Employment",                     "Per cent deviation from steady state",
  "s",            "Terms of Trade",                 "Per cent deviation from steady state",
  "s_gap",        "Terms of Trade Gap",             "Per cent deviation from steady state",
  "nx",           "Net Exports",                    "Per cent deviation from steady state",
  "er",           "Nominal Exchange Rate",          "Per cent deviation from steady state",
  "d_er",         "Nominal Exchange Rate Growth",   "Per cent deviation from steady state",
  "a",            "Technology Shock Process",       "Per cent deviation from steady state",
  "z",            "Preference Shock Process",       "Per cent deviation from steady state",
  "nu",           "Monetary Policy Shock Process",  "Per cent deviation from steady state"
) |>
  annualise_rate("i") |>
  annualise(c("r_real", "r_nat")) |>
  insert_after("pi", year_ended_inflation_row("pi")) |>
  # `n` is the same hours input as Ch.3's "Hours Worked" (both use the
  # production function y = a + (1 - alpha) * n).
  insert_after("n", alias_row("hours", "Hours Worked", "Per cent deviation from steady state", "n")) |>
  common_cols()

gali2015ch8_shocks <- tribble(
  ~shock,       ~description,
  "eps_nu",     "Monetary policy shock",
  "eps_a",      "Technology shock",
  "eps_z",      "Preference shock",
  "p_star",     "World price level shock"
)

# 4. Gali_2010 - sticky wage model with unemployment (Handbook ch.10) --------
# Despite its name, the mod file's `y_gap` is output, not an output gap: its
# long name is "output" and it is defined by the production function
# y_gap = a + (1 - alfa) * nhat.
gali2010_variables <- tribble(
  ~dynare_name,   ~display_name,                    ~units,
  "ihat",         "Nominal Interest Rate",          "Per cent (quarterly)",
  "rhat",         "Real Interest Rate",             "Per cent (quarterly)",
  "pi_p",         "Price Inflation",                "Per cent (quarterly)",
  "pi_w",         "Wage Inflation",                 "Per cent (quarterly)",
  "y_gap",        "Output",                         "Per cent deviation from steady state",
  "nhat",         "Employment",                     "Per cent deviation from steady state",
  "fhat",         "Labor Force",                    "Per cent deviation from steady state",
  "urhat",        "Unemployment Rate",              "Percentage points deviation from steady state",
  "hatw_real",    "Real Wage",                      "Per cent deviation from steady state",
  "mu_hat",       "Price Markup",                   "Per cent deviation from steady state",
  "a",            "Technology Shock Process",       "Per cent deviation from steady state",
  "nu",           "Monetary Policy Shock Process",  "Per cent deviation from steady state"
) |>
  annualise_rate("ihat") |>
  annualise("rhat") |>
  insert_after("pi_p", alias_row("inflation", "Inflation", "Per cent (quarterly)", "pi_p")) |>
  insert_after("inflation", year_ended_inflation_row("pi_p")) |>
  # The model has no hours-per-worker margin (employment is the only labour
  # input in production), so hours worked move one-for-one with employment.
  insert_after("nhat", alias_row("hours", "Hours Worked", "Per cent deviation from steady state", "nhat")) |>
  common_cols()

gali2010_shocks <- tribble(
  ~shock,     ~description,
  "eps_a",    "Technology shock",
  "eps_nu",   "Monetary policy shock"
)

# 5. Ireland_2004 - estimated NK model (post-1980 calibration) ---------------
# Ireland's technology is Y_t = Z_t * h_t, with Z_t a random walk, and `yhat`
# is output relative to that stochastic trend (Y_t / Z_t). So:
# - Hours Worked equal `yhat` exactly, for every shock.
# - `yhat` is also the response of output itself to every shock except a
#   technology shock (which moves Z_t permanently, and `yhat` then misses
#   that trend shift). So "Output" is offered in Alternative Paths only
#   (`irf = FALSE`), where only the monetary policy shock is used.
ireland2004_variables <- tribble(
  ~dynare_name,   ~display_name,                    ~units,
  "rhat",         "Nominal Interest Rate",          "Per cent (quarterly)",
  "pihat",        "Inflation",                      "Per cent (quarterly)",
  "ghat",         "Output Growth",                  "Per cent (quarterly)",
  "x",            "Output Gap",                     "Per cent deviation from steady state",
  "yhat",         "Output",                         "Per cent deviation from steady state",
  "a",            "Preference Shock Process",       "Per cent deviation from steady state",
  "e",            "Cost-Push Shock Process",        "Per cent deviation from steady state",
  "z",            "Technology Shock Process",       "Per cent deviation from steady state"
) |>
  annualise_rate("rhat") |>
  insert_after("pihat", year_ended_inflation_row("pihat")) |>
  insert_after("yhat", alias_row("hours", "Hours Worked", "Per cent deviation from steady state", "yhat")) |>
  common_cols() |>
  mutate(irf = dynare_name != "yhat")

ireland2004_shocks <- tribble(
  ~shock,     ~description,
  "eps_a",    "Preference shock",
  "eps_e",    "Cost-push shock",
  "eps_z",    "Technology shock",
  "eps_r",    "Monetary policy shock"
)

# 6. Ascari_Sbordone_2014 - trend inflation NK model -------------------------
ascari_variables <- tribble(
  ~dynare_name,       ~display_name,                    ~units,
  "i",                "Nominal Interest Rate",          "Per cent (quarterly)",
  "pi",               "Inflation",                      "Per cent (quarterly)",
  "real_interest",    "Real Interest Rate",             "Per cent (quarterly)",
  "y",                "Output",                         "Per cent deviation from steady state",
  "N",                "Hours Worked",                   "Per cent deviation from steady state",
  "w",                "Real Wage",                      "Per cent deviation from steady state",
  "s",                "Price Dispersion",               "Per cent deviation from steady state",
  "Average_markup",   "Average Price Markup",           "Per cent deviation from steady state",
  "Marginal_markup",  "Marginal Price Markup",          "Per cent deviation from steady state",
  "A",                "Technology Shock Process",       "Per cent deviation from steady state",
  "zeta",             "Preference Shock Process",       "Per cent deviation from steady state",
  "v",                "Monetary Policy Shock Process",  "Per cent deviation from steady state"
) |>
  # This model is written in logs of levels, and `i` is the log of the NET
  # nominal rate (the Euler equation uses 1 + exp(i)), so its response is a
  # proportional change in the rate. Multiplying by the steady-state net rate
  # `i_bar` converts it to a change in the quarterly rate itself - the same
  # units as `pi` (log gross inflation) and every other model's nominal rate -
  # before annualising.
  annualise_rate("i", own_scale = "i_bar") |>
  annualise("real_interest") |>
  insert_after("pi", year_ended_inflation_row("pi")) |>
  common_cols()

ascari_shocks <- tribble(
  ~shock,     ~description,
  "e_v",      "Monetary policy shock",
  "e_a",      "Technology shock",
  "e_zeta",   "Preference shock"
)

# 7. Born_Pfeifer_2018_MP - Calvo (default) vs Rotemberg wage NK model -------
bp2018mp_variables <- tribble(
  ~dynare_name,   ~display_name,                    ~units,
  "i",            "Nominal Interest Rate",          "Per cent (quarterly)",
  "pi_p",         "Price Inflation",                "Per cent (quarterly)",
  "pi_w",         "Wage Inflation",                 "Per cent (quarterly)",
  "r_real",       "Real Interest Rate",             "Per cent (quarterly)",
  "r_nat",        "Natural Interest Rate",          "Per cent (quarterly)",
  "y_gap",        "Output Gap",                     "Per cent deviation from steady state",
  "y",            "Output",                         "Per cent deviation from steady state",
  "n",            "Hours Worked",                   "Per cent deviation from steady state",
  "w_real",       "Real Wage",                      "Per cent deviation from steady state",
  "w_gap",        "Real Wage Gap",                  "Per cent deviation from steady state",
  "mu_p",         "Price Markup",                   "Per cent deviation from steady state",
  "m_real",       "Real Money Stock",               "Per cent deviation from steady state",
  "m_growth_ann", "Money Growth",                   "Per cent (annualized)",
  "a",            "Technology Shock Process",       "Per cent deviation from steady state",
  "z",            "Preference Shock Process",       "Per cent deviation from steady state",
  "nu",           "Monetary Policy Shock Process",  "Per cent deviation from steady state"
) |>
  annualise_rate("i") |>
  annualise(c("r_real", "r_nat")) |>
  insert_after("pi_p", alias_row("inflation", "Inflation", "Per cent (quarterly)", "pi_p")) |>
  insert_after("inflation", year_ended_inflation_row("pi_p")) |>
  common_cols()

bp2018mp_shocks <- tribble(
  ~shock,     ~description,
  "eps_a",    "Technology shock",
  "eps_nu",   "Monetary policy shock",
  "eps_z",    "Preference shock"
)

# Write the remaining 7 workbooks --------------------------------------------
models <- list(
  Gali2015Ch3 = list(variables = gali2015ch3_variables, shocks = gali2015ch3_shocks),
  Gali2015Ch6 = list(variables = gali2015ch6_variables, shocks = gali2015ch6_shocks),
  Gali2015Ch8 = list(variables = gali2015ch8_variables, shocks = gali2015ch8_shocks),
  Gali2010 = list(variables = gali2010_variables, shocks = gali2010_shocks),
  Ireland2004 = list(variables = ireland2004_variables, shocks = ireland2004_shocks),
  AscariSbordone2014 = list(variables = ascari_variables, shocks = ascari_shocks),
  BornPfeifer2018MP = list(variables = bp2018mp_variables, shocks = bp2018mp_shocks)
)

for (model_name in names(models)) {
  out_path <- file.path("inputs", paste0(model_name, "_metadata.xlsx"))
  write_xlsx(
    list(variables = models[[model_name]]$variables, shocks = models[[model_name]]$shocks),
    path = out_path
  )
  cat("build_model_metadata: wrote", out_path, "\n")
}
