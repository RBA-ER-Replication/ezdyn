dashboard_test_model <- function(
    label = "Fixture",
    anticipated_shock = FALSE,
    shock_name = "eps_r",
    shock_description = "Monetary policy shock",
    include_gdp_growth = FALSE,
    include_derived_ye = FALSE,
    include_param_table = FALSE) {
  shocks <- c(shock_name, if (anticipated_shock) paste0(shock_name, "_1"))
  responses <- c("r_obs", "infl_obs", if (include_gdp_growth) "gdp_growth")
  irf <- expand.grid(
    t = 1:4,
    shock = shocks,
    resp_var = responses,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  irf$value <- ifelse(
    irf$resp_var == "r_obs",
    1 / irf$t,
    0.25 / irf$t
  )
  varmeta <- data.frame(
    dynare_name = responses,
    display_name = c("Cash Rate", "Inflation", if (include_gdp_growth) "GDP Growth"),
    units = c("Per cent", "Percentage points", if (include_gdp_growth) "Percentage points"),
    hsd = TRUE,
    default = TRUE,
    scale_formula = NA_character_,
    unit_symbol_irf = "ppt",
    unit_symbol_baseline = "%",
    derived_from = NA_character_,
    derived_transform = NA_character_,
    stringsAsFactors = FALSE
  )
  varmeta$scale_factor <- rep(list(1), length(responses))
  if (include_derived_ye) {
    derived_rows <- data.frame(
      dynare_name = c("infl_obs_ye", "gdp_growth_ye"),
      display_name = c("Inflation (YE)", "GDP Growth (YE)"),
      units = "Percentage points",
      hsd = FALSE,
      default = TRUE,
      scale_formula = NA_character_,
      unit_symbol_irf = "ppt",
      unit_symbol_baseline = "%",
      derived_from = c("infl_obs", "gdp_growth"),
      derived_transform = "year_ended_sum",
      stringsAsFactors = FALSE
    )
    derived_rows$scale_factor <- rep(list(1), nrow(derived_rows))
    varmeta <- rbind(varmeta, derived_rows)
  }
  model <- list(
    M_ = list(
      model_name = label,
      endo.vars = responses,
      exo.vars = shocks,
      varmeta = varmeta,
      shock_meta = data.frame(
        shock = shocks,
        description = c(shock_description, rep(NA_character_, length(shocks) - 1)),
        stringsAsFactors = FALSE
      ),
      param_table = if (include_param_table) {
        data.frame(
          Sector = c("Preferences", "Preferences"),
          Type = c("Calibrated", "Calibrated"),
          LaTeX_Symbol = c("$\\beta$", "$\\sigma$"),
          Dyn_Symbol = c("beta", "sigma"),
          Description = c("Discount factor", "Risk aversion"),
          Value = c(0.99, 2),
          stringsAsFactors = FALSE
        )
      }
    ),
    oo_ = list(
      irf = irf,
      # Mock smoothed shocks/variables so this fixture genuinely qualifies as
      # having historical shock decomposition data (see hsd_data_available()).
      # Not a full decision-rule solve - only used by tests that check
      # model-selection/config behaviour, never by tests that actually
      # invoke get_hd()/ez_hd() end-to-end on this fixture.
      SmoothedShocks = stats::setNames(as.list(rep(0, length(shocks))), shocks),
      SmoothedVariables = stats::setNames(as.list(rep(0, length(responses))), responses)
    )
  )
  class(model$M_) <- "custom_moo"
  class(model$oo_) <- "custom_moo"
  class(model) <- "custom_moo"
  model
}

dashboard_test_timeline <- function() {
  list(
    data_start = as.Date("2023-12-01"),
    data_end = as.Date("2024-06-01"),
    forecast_start = as.Date("2024-03-01"),
    forecast_end = as.Date("2024-06-01")
  )
}

dashboard_test_baseline <- function(include_gdp_growth = FALSE, include_derived_ye = FALSE) {
  baseline <- data.frame(
    date = seq(as.Date("2023-12-01"), as.Date("2024-06-01"), by = "quarter"),
    check.names = FALSE
  )
  baseline[["Cash Rate"]] <- c(4.1, 4.2, 4.3)
  if (include_gdp_growth) {
    baseline[["Inflation"]] <- c(0.2, 0.3, 0.4)
    baseline[["GDP Growth"]] <- c(0.5, 0.6, 0.7)
  }
  if (include_derived_ye) {
    baseline[["Inflation (YE)"]] <- c(0.8, 1.1, 1.4)
    baseline[["GDP Growth (YE)"]] <- c(2.0, 2.3, 2.6)
  }
  class(baseline) <- c("ezdyn_baseline", class(baseline))
  baseline
}

dashboard_test_scenarios <- function() {
  data.frame(
    scenario = rep("Test Scenario", 2),
    t = 1:2,
    shock = rep("Test shock", 2),
    dynare_name = rep("r_obs", 2),
    display_name = rep("Cash Rate", 2),
    display_unit = rep("Per cent", 2),
    value = c(0.1, 0.2),
    model_name = rep("Fixture", 2),
    footnote = rep("Fixture scenario.", 2),
    stringsAsFactors = FALSE
  )
}

dashboard_scenario_test_model <- function(label = "Fixture", anticipated = TRUE) {
  model <- dashboard_test_model(
    label = label,
    anticipated_shock = FALSE,
    shock_name = "eps_r",
    shock_description = "Monetary policy shock"
  )
  shocks <- c("eps_r", "eps_psi", "eps_xi_c", "eps_a", "eps_a_n", "eps_g", "eps_p_star_z", "eps_infl_star")
  if (anticipated) {
    shocks <- c(shocks, paste0("eps_r_", 1:4), paste0("eps_psi_", 1:4))
  }
  responses <- c("r_obs", "q_obs", "c", "a", "a_n", "g", "p_star_z", "infl_star_obs")
  irf <- expand.grid(
    t = 1:40,
    shock = shocks,
    resp_var = responses,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  irf$value <- 0
  irf$value[irf$resp_var == "r_obs"] <- 1 / irf$t[irf$resp_var == "r_obs"]
  irf$value[irf$resp_var == "q_obs"] <- 10 / irf$t[irf$resp_var == "q_obs"]
  irf$value[irf$resp_var == "c"] <- 0.01 / irf$t[irf$resp_var == "c"]
  irf$value[irf$resp_var == "a"] <- 0.01 / irf$t[irf$resp_var == "a"]
  irf$value[irf$resp_var == "a_n"] <- 0.01 / irf$t[irf$resp_var == "a_n"]
  irf$value[irf$resp_var == "g"] <- 1 / irf$t[irf$resp_var == "g"]
  irf$value[irf$resp_var == "p_star_z"] <- 1 / irf$t[irf$resp_var == "p_star_z"]
  irf$value[irf$resp_var == "infl_star_obs"] <- 1 / irf$t[irf$resp_var == "infl_star_obs"]
  if (anticipated) {
    for (period in 1:4) {
      monetary_rows <- irf$shock == paste0("eps_r_", period) & irf$resp_var == "r_obs"
      exchange_rate_rows <- irf$shock == paste0("eps_psi_", period) & irf$resp_var == "q_obs"
      irf$value[monetary_rows] <- as.numeric(irf$t[monetary_rows] == period)
      irf$value[exchange_rate_rows] <- as.numeric(irf$t[exchange_rate_rows] == period)
    }
  }
  model$M_$endo.vars <- responses
  model$M_$exo.vars <- shocks
  model$M_$varmeta <- data.frame(
    dynare_name = responses,
    display_name = c(
      "Cash Rate", "Real TWI", "Consumption", "Aggregate Productivity",
      "Non-Tradable Productivity", "Public demand", "Commodity Price (Foreign)",
      "Foreign Inflation"
    ),
    units = rep("Per cent", length(responses)),
    unit_symbol_irf = rep("ppt", length(responses)),
    unit_symbol_baseline = rep("%", length(responses)),
    default = TRUE,
    scale_formula = NA_character_,
    stringsAsFactors = FALSE
  )
  model$M_$varmeta$scale_factor <- rep(list(1), nrow(model$M_$varmeta))
  model$M_$shock_meta <- data.frame(
    shock = c("eps_r", "eps_psi", "eps_xi_c", "eps_a", "eps_a_n", "eps_g", "eps_p_star_z", "eps_infl_star"),
    description = c(
      "Monetary policy shock", "Risk premium (exchange rate) shock",
      "Consumption preference shock", "Stationary aggregate TFP productivity",
      "Stationary non-tradeable sector productivity", "Government spending shock",
      "Foreign commodity price shock", "Foreign cost push shock"
    ),
    stringsAsFactors = FALSE
  )
  model$oo_$irf <- irf
  model
}

dashboard_test_config <- function(
    overview = list(),
    hsd = list(),
    irf = list(),
    help = list(),
    optimal_policy = NULL,
    include_gdp_growth = FALSE,
    include_derived_ye = FALSE) {
  configure_dashboard(
    models = list(Fixture = dashboard_test_model(
      include_gdp_growth = include_gdp_growth,
      include_derived_ye = include_derived_ye
    )),
    timeline = dashboard_test_timeline(),
    baselines = list(Staff = dashboard_test_baseline(
      include_gdp_growth = include_gdp_growth,
      include_derived_ye = include_derived_ye
    )),
    default_baseline = "Staff",
    overview = overview,
    hsd = hsd,
    irf = irf,
    help = help,
    optimal_policy = optimal_policy
  )
}
