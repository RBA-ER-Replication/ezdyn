# Build the dashboard's semi-structural models: impulse responses to the
# Romer-Romer (2004) and Gertler-Karadi (2015) monetary policy shocks, as
# estimated by McKay & Wolf (2023), plus their metadata.
#
# McKay & Wolf estimate a quarterly VAR over 1969Q1-2007Q4 (4 lags, constant
# and linear trend, flat prior), ordering the shock series recursively:
# [GK shock, output gap, inflation, commodity prices, RR shock, fed funds
# rate]. We use their published posterior-median responses from the
# replication files (https://github.com/ckwolf92/policy_cnfctls, MIT licence).
# See README.md for details.
#
# Outputs (inputs/):
#   semi_structural_irfs.csv      - IRFs in long format, one set per model
#   semi_structural_metadata.xlsx - variable and shock metadata for custom_moo()
#
# Run from this directory: Rscript build_semi_structural_models.R

library(dplyr)
library(tibble)

mw_url <- paste0(
  "https://raw.githubusercontent.com/ckwolf92/policy_cnfctls/",
  "4e4e81fad4f5f3885bee643d187f645f406a4778/empirics/var_inputs/_results/IS_mp_base.mat"
)

# McKay & Wolf save each shock's responses as IS_<n>: IS_1 is the GK shock
# (ordered first), IS_2 the RR shock (ordered fifth).
shocks <- c(`Romer-Romer (2004)` = "IS.2", `Gertler-Karadi (2015)` = "IS.1")

mat_path <- tempfile(fileext = ".mat")
options(timeout = max(600, getOption("timeout"))) # 46 MB, mostly posterior draws
download.file(mw_url, mat_path, mode = "wb", quiet = TRUE)
mw <- R.matlab::readMat(mat_path)
unlink(mat_path)

# Posterior-median responses (100 quarters x [output gap, annualised GDP
# deflator inflation, fed funds rate], all per cent), converted to the
# dashboard's quarterly units and scaled so the fed funds rate peaks 100bp
# (annualised) above baseline. Output growth is the change in the output gap,
# assuming the shock doesn't move potential output.
irfs <- purrr::imap(shocks, \(field, model) {
  median <- mw[[field]][["IRF.med", 1, 1]]
  scale <- 1 / max(median[, 3])
  tibble(
    t = seq_len(nrow(median)),
    ygap = scale * median[, 1],
    dy = diff(c(0, ygap)),
    pinfobs = scale * median[, 2] / 4,
    robs = scale * median[, 3] / 4
  ) |>
    tidyr::pivot_longer(-t, names_to = "resp_var") |>
    mutate(model = model, shock = "em", .before = 1)
}) |>
  bind_rows()

# Variable codes, names and units match Smets-Wouters' (and the Gali models'
# "Output Gap"), so the models share the dashboard baseline and Optimal
# Policy settings. As in build_model_metadata.R, derived rows add the common
# "Output" (the output gap, as potential is unaffected), "Inflation
# (year-ended)" (four-quarter sum of quarterly inflation) and "Nominal Interest
# Rate (quarterly)" variables. "Nominal Interest Rate" is shown annualised (x 4
# on the quarterly `robs`). The output gap has no real-data baseline, so it is
# left out of Alternative Paths.
variables <- tribble(
  ~dynare_name, ~display_name,                       ~units,                                 ~alt_paths, ~scale_formula, ~derived_from, ~derived_transform,
  "ygap",       "Output Gap",                        "Per cent deviation from steady state", FALSE,      NA,             NA,            NA,
  "output",     "Output",                            "Per cent deviation from steady state", TRUE,       NA,             "ygap",        NA,
  "dy",         "Output Growth",                     "Per cent (quarterly)",                 TRUE,       NA,             NA,            NA,
  "pinfobs",    "Inflation",                         "Per cent (quarterly)",                 TRUE,       NA,             NA,            NA,
  "pinfobs_ye", "Inflation (year-ended)",            "Per cent (year-ended)",                TRUE,       NA,             "pinfobs",     "year_ended_sum",
  "robs",       "Nominal Interest Rate",             "Per cent (annualized)",                TRUE,       "4",            NA,            NA,
  "robs_q",     "Nominal Interest Rate (quarterly)", "Per cent (quarterly)",                 TRUE,       "0.25",         "robs",        NA
) |>
  mutate(
    irf = TRUE,
    hsd = FALSE,
    scale_formula = as.character(scale_formula),
    derived_from = as.character(derived_from),
    derived_transform = as.character(derived_transform),
    default = TRUE
  ) |>
  # Plot axis unit symbols, as in build_model_metadata.R: impulse responses
  # are changes from baseline, so rates move in percentage points, while
  # Alternative Paths plot levels, so rates are per cent.
  mutate(
    unit_symbol_irf = if_else(units == "Per cent deviation from steady state", "% dev", "ppt"),
    unit_symbol_baseline = if_else(units == "Per cent deviation from steady state", "% dev", "%"),
    .after = units
  )

write.csv(irfs, "inputs/semi_structural_irfs.csv", row.names = FALSE)
writexl::write_xlsx(
  list(variables = variables, shocks = tibble(shock = "em", description = "Monetary policy shock")),
  "inputs/semi_structural_metadata.xlsx"
)
message("Wrote inputs/semi_structural_irfs.csv and inputs/semi_structural_metadata.xlsx")
