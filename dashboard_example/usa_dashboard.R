# ezdyn Dashboard (USA): Smets-Wouters (2007), seven New Keynesian models from
# Johannes Pfeifer's DSGE_mod collection, two semi-structural models of US
# monetary policy shocks, and the RBA's DINGO model (an Australian model,
# included for comparison). The models and data are illustrative only: see
# README.md to run it and create_models/README.md for the models.
#
# Run from this folder (dashboard_example/); all paths are relative to it.


# 0. Config to ignore ----
library(tibble)
source("help_text.R", local = TRUE, encoding = "UTF-8")
source("read_vintages.R", local = TRUE, encoding = "UTF-8") 

# 1. Load Models ----
model_inputs <- tribble(
  ~model_name,                  ~colour,          ~model_path,                           ~metadata_path,
  "Smets Wouters (2007)",       "forestgreen",    "inputs/SW_2007_45.json",              "inputs/variable_metadata.xlsx",
  "Gali NK (Ch.3)",             "red",            "inputs/Gali2015Ch3.json",             "inputs/Gali2015Ch3_metadata.xlsx",
  "Gali NK+Wage (Ch.6, anticipated)", "darkorange", "inputs/Gali2015Ch6_anticipated.json", "inputs/Gali2015Ch6_metadata.xlsx",
  "Gali SOE (Ch.8)",            "purple",         "inputs/Gali2015Ch8.json",             "inputs/Gali2015Ch8_metadata.xlsx",
  "Gali Unemployment (2010)",   "firebrick",      "inputs/Gali2010.json",                "inputs/Gali2010_metadata.xlsx",
  "Ireland (2004)",             "darkgoldenrod",  "inputs/Ireland2004.json",             "inputs/Ireland2004_metadata.xlsx",
  "Ascari-Sbordone (2014)",     "darkslategray",  "inputs/AscariSbordone2014.json",      "inputs/AscariSbordone2014_metadata.xlsx",
  "Born-Pfeifer (2018)",        "deeppink",       "inputs/BornPfeifer2018MP.json",       "inputs/BornPfeifer2018MP_metadata.xlsx",
  "Romer-Romer (2004)",         "sienna",         "inputs/RomerRomer2004_irfs.xlsx",     "inputs/semi_structural_metadata.xlsx",
  "Gertler-Karadi (2015)",      "darkcyan",       "inputs/GertlerKaradi2015_irfs.xlsx",  "inputs/semi_structural_metadata.xlsx",
  "DINGO",                      "#C43C39",        "inputs/DINGO.json",                   "inputs/DINGO_metadata.xlsx"
)

# Load each models into a dynare model object and put them all in a list 
models <- purrr::pmap(model_inputs, function(model_name, colour, model_path, metadata_path) {
  if (tools::file_ext(model_path) == "json") {
    ezdyn::read_dynare(path = model_path, path_meta = metadata_path, model_name = model_name, colour = colour)
  } else {
    ezdyn::custom_moo(irf = readxl::read_excel(model_path), model_name = model_name, meta = metadata_path, colour = colour)
  }
})
names(models) <- model_inputs$model_name


# 2. Define Timeline ----
# Update these from the timeline that build_baselines/build_us_baseline.R or
# build_baselines/build_dummy_baseline.R prints. forecast_end closes the
# Alternative Paths window; Optimal Policy runs to data_end.
timeline <- list(
  data_start = as.Date("1954-09-01"),
  forecast_start = as.Date("2026-12-01"),
  forecast_end = as.Date("2029-03-01"),
  data_end = as.Date("2038-09-01")
)

# 3. Load Baselines ----
if (file.exists("inputs/us_baseline.xlsx")) {
  baselines <- list(`FRED data + Fed SEP` = ezdyn::import_baseline("inputs/us_baseline.xlsx", models = models),
                    `Dummy data (illustrative)` = ezdyn::import_baseline("inputs/dummy_baseline.xlsx", models = models))
  # Historical vintages of loss used for timeless commitment option.
  vintages <- us_read_vintages("inputs/us_vintages.xlsx", timeline$forecast_start)
  default_baseline <- "FRED data + Fed SEP"
} else { # If FRED baseline does not exist, only load dummy data baseilne
  baselines <- list(`Dummy data (illustrative)` = ezdyn::import_baseline("inputs/dummy_baseline.xlsx", models = models))
  vintages <- NULL
  default_baseline <- "Dummy data (illustrative)"
}

# 4. Tab-Specific Config ----
  # 4.1. Impulse Response Functions (IRFs) tab
  comparison_models <- c("Gali NK+Wage (Ch.6, anticipated)", "DINGO")
  irf_tab_config <- list(defaults = list(models = comparison_models,
                                         display_names = c("Nominal Interest Rate", "Inflation"), horizon = 20))
  # 4.2. Alt Paths Tab
  alt_paths_tab_config <- list(defaults = list(
    models = comparison_models,
    display_names = c("Inflation (year-ended)", "Output Growth (year-ended)", "Nominal Interest Rate"),
    lambda = 0.8
  ))
  # 4.3. Optimal Policy Tab
  opt_policy_tab_config <- list(
        defaults = list(models = comparison_models),
        instrument_variable = "Nominal Interest Rate",
        instrument_shock = "Monetary policy shock",
        use_cd = TRUE,
        lambda = 0.8,
        forecast_end = timeline$data_end,
        # Without vintages these are NULL: the commitment start defaults to the
        # forecast start, and Commitment is ordinary commitment.
        preloss_start = if (!is.null(vintages)) min(vintages$vintage),
        loss_variable_vintages = vintages,
        # The loss variables with US data in the baseline and past gaps in
        # `vintages`, which timeless commitment needs. "Unemployment Rate" is
        # offered for the Gali Unemployment model.
        loss_variable_choices = c(
          "Inflation", "Output Growth", "Output Growth (year-ended)",
          "Nominal Interest Rate Changes", "Unemployment Rate"
        ),
        constraint_variable_choices = c("Nominal Interest Rate", "Inflation"),
        # Default loss variable targets over the forecast horizon. 
        loss_variable_defaults = c(Inflation = 0.5, `Unemployment Rate` = 4.2, `Output Growth` = 0.5,
                                  `Output Growth (year-ended)` = 2, `Nominal Interest Rate Changes` = 0),
        default_response_variables = c("Inflation (year-ended)", "Output Growth (year-ended)", "Nominal Interest Rate"),
        default_loss_variables = c("Inflation", "Output Growth", "Nominal Interest Rate Changes"),
        # Miscellaneous Optional Configuration for Optimal Policy tab
        help = list(
          cd = usa_help("anticipated_shocks"),
          strategy = helpmodal_strategy,
          loss_variables = helpmodal_loss_variables,
          loss_horizon = helpmodal_loss_horizon,
          instrument_horizon = helpmodal_instrument_horizon,
          constraints = helpmodal_constraints,
          commitment_start = helpmodal_commitment_start
        )
      )

# 4. Define Dashboard Configuration ----

config <- ezdyn::configure_dashboard(
    # General Configuration
    name = "ezdyn Dashboard (USA)",
    models = models,
    timeline = timeline,
    baselines = baselines,
    default_baseline = default_baseline,
    instrument_display_name = "Nominal Interest Rate",
    # Tab-Specific Configuration
    irf = irf_tab_config,
    alt_paths = alt_paths_tab_config,
    optimal_policy = opt_policy_tab_config,
    response_notes = tibble(
      model_name = "DINGO",
      note = "DINGO is an Australian model; its inflation is trimmed-mean inflation."
    ),
    # Miscellaneous Optional Configuration
    header_links = c(
      "Read the whitepaper" = "https://github.com/RBA-ER-Replication/ezdyn/blob/main/ezdyn.pdf?raw=true",
      "Report issue" = "https://github.com/RBA-ER-Replication/ezdyn/issues/new"
    ),
    startup_modal = usa_startup_modal, # Startup message with disclaimers 
    # Custom help menu for explaining anticipation and cognitive discounting.
    help = list(anticipated_shocks = usa_help("anticipated_shocks"))
  )

# Launch the Dashboard ----
usa_dashboard <- ezdyn::ezdyn_dashboard(config)
shiny::runApp(usa_dashboard)
