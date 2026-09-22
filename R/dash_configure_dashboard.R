# Dashboard configuration --------------------------------------------------

# Return a default only when an optional configuration value is NULL.
dashboard_or_default <- function(value, default) {
  if (is.null(value)) default else value
}

# Validate a named registry of MOO pairs returned by ezdyn constructors.
dashboard_validate_models <- function(models) {
  if (!is.list(models) || length(models) == 0) {
    stop("`models` must be a non-empty named list of full MOO pairs.", call. = FALSE)
  }
  labels <- names(models)
  if (is.null(labels) || anyNA(labels) || any(labels == "") || anyDuplicated(labels)) {
    stop("`models` must have unique, non-empty names.", call. = FALSE)
  }

  valid_models <- vapply(models, function(model) {
    inherits(model, "dynare") || inherits(model, "custom_moo")
  }, logical(1))
  if (!all(valid_models)) {
    invalid_labels <- labels[!valid_models]
    stop(
      sprintf(
        "`models` entries must be `dynare` or `custom_moo` MOO pairs returned by ezdyn: %s.",
        paste(sprintf("`%s`", invalid_labels), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(models)
}

# Require one first-day-of-quarter date in March, June, September, or December.
dashboard_is_quarter_date <- function(date) {
  inherits(date, "Date") &&
    length(date) == 1 &&
    !is.na(date) &&
    format(date, "%d") == "01" &&
    format(date, "%m") %in% c("03", "06", "09", "12")
}

# Validate the timeline shared by dashboard modules.
dashboard_validate_timeline <- function(timeline) {
  required_dates <- c("data_start", "data_end", "forecast_start", "forecast_end")
  if (!is.list(timeline) || !all(required_dates %in% names(timeline))) {
    stop(
      sprintf("`timeline` must contain %s.", paste(sprintf("`%s`", required_dates), collapse = ", ")),
      call. = FALSE
    )
  }

  timeline <- timeline[required_dates]
  if (!all(vapply(timeline, dashboard_is_quarter_date, logical(1)))) {
    stop("Timeline dates must be first-day quarterly Date values.", call. = FALSE)
  }
  if (!(timeline$data_start <= timeline$forecast_start &&
        timeline$forecast_start <= timeline$forecast_end &&
        timeline$forecast_end <= timeline$data_end)) {
    stop("Require `data_start <= forecast_start <= forecast_end <= data_end`.", call. = FALSE)
  }

  expected_forecasts <- seq(timeline$forecast_start, timeline$forecast_end, by = "quarter")
  if (!identical(utils::tail(expected_forecasts, 1), timeline$forecast_end)) {
    stop("`forecast_start` and `forecast_end` must define an inclusive quarterly sequence.", call. = FALSE)
  }

  timeline
}

# Check the boundary class only: import_baseline() owns baseline data validation.
dashboard_validate_baseline <- function(baseline, label) {
  if (!inherits(baseline, "ezdyn_baseline")) {
    stop(sprintf("Baseline `%s` must be an `ezdyn_baseline` result from `import_baseline()`.", label), call. = FALSE)
  }

  invisible(baseline)
}

# Validate configured baseline coverage for Alternative Policy Paths.
dashboard_validate_baseline_coverage <- function(baseline, timeline, label) {
  if (!is.data.frame(baseline) || !("date" %in% names(baseline)) ||
      !inherits(baseline$date, "Date")) {
    stop(sprintf("Baseline `%s` must contain a Date `date` column.", label), call. = FALSE)
  }

  required_dates <- seq(timeline$data_start, timeline$forecast_end, by = "quarter")
  if (!all(required_dates %in% baseline$date)) {
    stop(
      sprintf("Baseline `%s` must cover every quarter from `data_start` through `forecast_end`.", label),
      call. = FALSE
    )
  }

  invisible(baseline)
}

# Validate named configured baselines and their default selection.
dashboard_validate_baselines <- function(baselines, default_baseline, timeline) {
  if (!is.list(baselines) || length(baselines) == 0) {
    stop("`baselines` must be a non-empty named list of `ezdyn_baseline` objects.", call. = FALSE)
  }
  labels <- names(baselines)
  if (is.null(labels) || anyNA(labels) || any(labels == "") || anyDuplicated(labels)) {
    stop("`baselines` must have unique, non-empty names.", call. = FALSE)
  }
  if (!is.character(default_baseline) || length(default_baseline) != 1 ||
      is.na(default_baseline) || !(default_baseline %in% labels)) {
    stop("`default_baseline` must name one configured baseline.", call. = FALSE)
  }

  for (label in labels) {
    dashboard_validate_baseline(baselines[[label]], label)
  }

  invisible(baselines)
}

# Validate scalar slider controls supplied in configuration.
dashboard_validate_slider_preference <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value) ||
      !is.finite(value) || value <= 0) {
    stop(sprintf("`%s` must be one positive finite numeric value.", name), call. = FALSE)
  }
  value
}

# Normalize and validate curated pretty IRF configuration.
dashboard_validate_irf_config <- function(irf, models) {
  if (is.null(irf)) {
    irf <- list()
  }
  if (!is.list(irf)) {
    stop("`irf` must be a list.", call. = FALSE)
  }

  scenarios <- irf$scenarios
  if (!is.null(scenarios)) {
    required_columns <- c(
      "scenario", "t", "shock", "dynare_name", "display_name", "display_unit",
      "value", "model_name"
    )
    if (!is.data.frame(scenarios) || !all(required_columns %in% names(scenarios))) {
      stop(
        sprintf("`irf$scenarios` must contain %s.", paste(sprintf("`%s`", required_columns), collapse = ", ")),
        call. = FALSE
      )
    }
    scenario_columns <- c("scenario", "shock", "dynare_name", "display_name", "display_unit", "model_name")
    if (!is.numeric(scenarios$t) || !is.numeric(scenarios$value) ||
        anyNA(scenarios$t) || anyNA(scenarios$value) ||
        !all(vapply(scenarios[scenario_columns], is.character, logical(1))) ||
        any(vapply(scenarios[scenario_columns], function(values) anyNA(values) || any(values == ""), logical(1))) ||
        any(!(scenarios$model_name %in% names(models)))) {
      stop("`irf$scenarios` must have complete scenario labels, numeric `t`/`value`, and configured model names.", call. = FALSE)
    }
    if ("footnote" %in% names(scenarios) && !is.character(scenarios$footnote)) {
      stop("`irf$scenarios$footnote` must be character when supplied.", call. = FALSE)
    }
  }

  defaults <- irf$defaults
  if (is.null(defaults)) {
    defaults <- list()
  }
  if (!is.list(defaults)) {
    stop("`irf$defaults` must be a list.", call. = FALSE)
  }
  defaults$horizon <- dashboard_or_default(defaults$horizon, 40L)
  defaults$models <- dashboard_or_default(defaults$models, names(models))
  defaults$display_names <- dashboard_or_default(defaults$display_names, "Trimmed Mean Inflation (ye)")
  defaults$use_cd <- dashboard_or_default(defaults$use_cd, FALSE)
  defaults$lambda <- dashboard_or_default(defaults$lambda, 0)

  if (!is.numeric(defaults$horizon) || length(defaults$horizon) != 1 ||
      is.na(defaults$horizon) || defaults$horizon < 1 || defaults$horizon %% 1 != 0) {
    stop("`irf$defaults$horizon` must be one positive integer.", call. = FALSE)
  }
  if (!is.character(defaults$models) || !all(defaults$models %in% names(models))) {
    stop("`irf$defaults$models` must contain configured model labels.", call. = FALSE)
  }
  if (!is.character(defaults$display_names) ||
      !is.logical(defaults$use_cd) || length(defaults$use_cd) != 1 || is.na(defaults$use_cd) ||
      !is.numeric(defaults$lambda) || length(defaults$lambda) != 1 ||
      is.na(defaults$lambda) || defaults$lambda < 0 || defaults$lambda > 1) {
    stop("`irf$defaults` contains invalid display, CD, or lambda settings.", call. = FALSE)
  }

  list(scenarios = scenarios, defaults = defaults)
}

# Normalize the optional Overview and HSD configuration blocks.
dashboard_validate_overview_config <- function(overview) {
  if (is.null(overview)) {
    return(list(html_path = NULL, ui = NULL, enabled = FALSE))
  }
  if (!is.list(overview)) {
    stop("`overview` must be a list.", call. = FALSE)
  }

  html_path <- dashboard_or_default(overview$html_path, NULL)
  ui <- dashboard_or_default(overview$ui, NULL)
  if (!is.null(html_path) && (!is.character(html_path) || length(html_path) != 1 || is.na(html_path))) {
    stop("`overview$html_path` must be one file path when supplied.", call. = FALSE)
  }
  if (!is.null(ui) && !is.function(ui)) {
    stop("`overview$ui` must be a function when supplied.", call. = FALSE)
  }
  if (!is.null(html_path) && !is.null(ui)) {
    stop("Supply only one of `overview$html_path` or `overview$ui`.", call. = FALSE)
  }

  list(html_path = html_path, ui = ui, enabled = !is.null(html_path) || !is.null(ui))
}

dashboard_validate_hsd_config <- function(hsd, models) {
  if (is.null(hsd)) {
    hsd <- list()
  }
  if (!is.list(hsd)) {
    stop("`hsd` must be a list.", call. = FALSE)
  }

  hsd$models <- dashboard_or_default(hsd$models, names(models))
  hsd$display_names <- dashboard_or_default(hsd$display_names, character())
  hsd$shock_group <- dashboard_or_default(hsd$shock_group, NULL)
  hsd$preamble_ui <- dashboard_or_default(hsd$preamble_ui, NULL)
  if (!is.character(hsd$models) || length(hsd$models) == 0 ||
      anyNA(hsd$models) || any(hsd$models == "") || anyDuplicated(hsd$models) ||
      !all(hsd$models %in% names(models)) ||
      !is.character(hsd$display_names) ||
      (!is.null(hsd$shock_group) &&
       (!is.character(hsd$shock_group) || length(hsd$shock_group) != 1 || is.na(hsd$shock_group))) ||
      (!is.null(hsd$preamble_ui) && !is.function(hsd$preamble_ui))) {
    stop("`hsd` contains invalid enabled-model, display-name, shock-group, or preamble settings.", call. = FALSE)
  }

  # Only offer models with genuine historical shock decomposition data
  # (`oo_$SmoothedShocks`/`SmoothedVariables`) - calibrated-only and
  # custom_moo() models never have these, so silently drop them here rather
  # than letting users pick a model that will error or produce meaningless
  # output (applies whether `hsd$models` was defaulted or set explicitly).
  hsd$models <- Filter(function(nm) hsd_data_available(models[[nm]]$oo_), hsd$models)
  if (length(hsd$models) == 0) {
    stop(
      "None of the configured `hsd$models` have historical shock decomposition data (`oo_$SmoothedShocks`/`SmoothedVariables`); the Historical Shock Decomposition tab requires at least one estimated/smoothed model.",
      call. = FALSE
    )
  }

  hsd
}

# Validate the optional Alternative Policy Paths module-selection configuration.
# `models` is the eligible allow-list offered by the tab's model selector
# (defaults to every configured model); `defaults$models` is the selector's
# initial selection (defaults to every eligible model).
dashboard_validate_alt_paths_config <- function(alt_paths, models) {
  if (is.null(alt_paths)) {
    alt_paths <- list()
  }
  if (!is.list(alt_paths)) {
    stop("`alt_paths` must be a list.", call. = FALSE)
  }

  alt_paths$models <- dashboard_or_default(alt_paths$models, names(models))
  if (!is.character(alt_paths$models) || length(alt_paths$models) == 0 ||
      anyNA(alt_paths$models) || any(alt_paths$models == "") || anyDuplicated(alt_paths$models) ||
      !all(alt_paths$models %in% names(models))) {
    stop("`alt_paths$models` must contain unique configured model labels.", call. = FALSE)
  }

  defaults <- alt_paths$defaults
  if (is.null(defaults)) {
    defaults <- list()
  }
  if (!is.list(defaults)) {
    stop("`alt_paths$defaults` must be a list.", call. = FALSE)
  }
  defaults$models <- dashboard_or_default(defaults$models, alt_paths$models)
  if (!is.character(defaults$models) || length(defaults$models) == 0 ||
      anyNA(defaults$models) || any(defaults$models == "") || anyDuplicated(defaults$models) ||
      !all(defaults$models %in% alt_paths$models)) {
    stop("`alt_paths$defaults$models` must contain eligible Alt Paths model labels.", call. = FALSE)
  }

  list(models = alt_paths$models, defaults = defaults)
}

# Validate the optional Variable Dictionary module-selection configuration.
# `models` is the eligible allow-list offered by the tab's model selector
# (defaults to every configured model); `defaults$models` is the selector's
# initial selection (defaults to every eligible model). Per-model capability
# (whether a model actually has `param_table`/`varmeta`/`shock_meta`) is
# checked at render time by the tab itself, not here, since eligibility
# differs by section (Parameters/Variables/Shocks).
dashboard_validate_dictionary_config <- function(dictionary, models) {
  if (is.null(dictionary)) {
    dictionary <- list()
  }
  if (!is.list(dictionary)) {
    stop("`dictionary` must be a list.", call. = FALSE)
  }

  dictionary$models <- dashboard_or_default(dictionary$models, names(models))
  if (!is.character(dictionary$models) || length(dictionary$models) == 0 ||
      anyNA(dictionary$models) || any(dictionary$models == "") || anyDuplicated(dictionary$models) ||
      !all(dictionary$models %in% names(models))) {
    stop("`dictionary$models` must contain unique configured model labels.", call. = FALSE)
  }

  defaults <- dictionary$defaults
  if (is.null(defaults)) {
    defaults <- list()
  }
  if (!is.list(defaults)) {
    stop("`dictionary$defaults` must be a list.", call. = FALSE)
  }
  defaults$models <- dashboard_or_default(defaults$models, dictionary$models)
  if (!is.character(defaults$models) || length(defaults$models) == 0 ||
      anyNA(defaults$models) || any(defaults$models == "") || anyDuplicated(defaults$models) ||
      !all(defaults$models %in% dictionary$models)) {
    stop("`dictionary$defaults$models` must contain eligible Variable Dictionary model labels.", call. = FALSE)
  }

  list(models = dictionary$models, defaults = defaults)
}

# Require a non-empty named character vector, e.g. a label -> dynare_name choice list.
dashboard_is_named_character <- function(value) {
  is.character(value) && length(value) > 0 && !is.null(names(value)) &&
    !anyNA(names(value)) && all(names(value) != "")
}

# Validate an optional named list of help-modal hook functions against a set
# of allowed keys. Every allowed key resolves to either `NULL` or a function,
# so callers can rely on `hooks[[key]]` without checking for missing entries.
dashboard_validate_help_hooks <- function(hooks, allowed_keys) {
  if (is.null(hooks)) {
    hooks <- list()
  }
  if (!is.list(hooks) ||
      (length(hooks) > 0 && (is.null(names(hooks)) || anyNA(names(hooks)) || any(names(hooks) == "")))) {
    stop("Help hooks must be a named list of functions.", call. = FALSE)
  }
  if (!all(names(hooks) %in% allowed_keys)) {
    stop(sprintf("Help hooks must be one of: %s.", paste(allowed_keys, collapse = ", ")), call. = FALSE)
  }
  if (!all(vapply(hooks, is.function, logical(1)))) {
    stop("Help hooks must be functions.", call. = FALSE)
  }

  defaults <- stats::setNames(vector("list", length(allowed_keys)), allowed_keys)
  utils::modifyList(defaults, hooks)
}

# Validate the optional top-level Help configuration used by dashboard modules
# for contextual help modals shown outside the Optimal Policy tab (e.g.
# Alternative Policy Paths' anticipated-shock control).
dashboard_validate_help_config <- function(help) {
  dashboard_validate_help_hooks(help, "anticipated_shocks")
}

# Validate an optional named numeric vector (e.g. loss-variable defaults).
dashboard_validate_named_numeric <- function(values, name) {
  if (is.null(values)) {
    return(stats::setNames(numeric(0), character(0)))
  }
  if (!is.numeric(values) || is.null(names(values)) || anyNA(names(values)) || any(names(values) == "")) {
    stop(sprintf("`%s` must be a named numeric vector when supplied.", name), call. = FALSE)
  }

  values
}

# Validate the optional Optimal Policy module configuration. The dashboard owns
# policy-specific baseline preparation; ezdyn owns the resulting calculation.
dashboard_validate_optimal_policy_config <- function(optimal_policy, models, timeline) {
  if (is.null(optimal_policy)) {
    return(list(enabled = FALSE))
  }
  if (!is.list(optimal_policy)) {
    stop("`optimal_policy` must be a list.", call. = FALSE)
  }
  enabled <- dashboard_or_default(optimal_policy$enabled, TRUE)
  if (!is.logical(enabled) || length(enabled) != 1 || is.na(enabled)) {
    stop("`optimal_policy$enabled` must be one logical value.", call. = FALSE)
  }
  if (!enabled) return(list(enabled = FALSE))

  policy_model <- dashboard_or_default(optimal_policy$policy_model, names(models)[[1]])
  comparison_model <- dashboard_or_default(optimal_policy$comparison_model, NULL)
  forecast_end <- dashboard_or_default(optimal_policy$forecast_end, NULL)
  preloss_start <- dashboard_or_default(optimal_policy$preloss_start, timeline$forecast_start)
  instrument_variable <- dashboard_or_default(optimal_policy$instrument_variable, "r_obs")
  instrument_shock <- dashboard_or_default(optimal_policy$instrument_shock, "eps_r")
  use_cd <- dashboard_or_default(optimal_policy$use_cd, FALSE)
  lambda <- dashboard_or_default(optimal_policy$lambda, 0)
  prepare_baseline <- dashboard_or_default(optimal_policy$prepare_baseline, identity)
  extend_baseline <- dashboard_or_default(optimal_policy$extend_baseline, function(baseline, policy_config) baseline)
  loss_variable_vintages <- dashboard_or_default(optimal_policy$loss_variable_vintages, NULL)
  loss_variable_choices <- dashboard_or_default(optimal_policy$loss_variable_choices, NULL)
  constraint_variable_choices <- dashboard_or_default(optimal_policy$constraint_variable_choices, NULL)
  default_response_variables <- dashboard_or_default(optimal_policy$default_response_variables, character(0))
  default_loss_variables <- dashboard_or_default(optimal_policy$default_loss_variables, character(0))
  elb_variable <- dashboard_or_default(optimal_policy$elb_variable, instrument_variable)
  help <- dashboard_validate_help_hooks(
    optimal_policy$help,
    c("cd", "strategy", "loss_variables", "loss_horizon", "instrument_horizon", "constraints", "commitment_start")
  )
  description_ui <- dashboard_or_default(optimal_policy$description_ui, NULL)
  loss_variable_defaults <- dashboard_validate_named_numeric(
    optimal_policy$loss_variable_defaults,
    "optimal_policy$loss_variable_defaults"
  )
  loss_variable_weight_defaults <- dashboard_validate_named_numeric(
    optimal_policy$loss_variable_weight_defaults,
    "optimal_policy$loss_variable_weight_defaults"
  )

  if (!is.character(policy_model) || length(policy_model) != 1 || !(policy_model %in% names(models)) ||
      (!is.null(comparison_model) && (!is.character(comparison_model) || length(comparison_model) != 1 || !(comparison_model %in% names(models)))) ||
      !dashboard_is_quarter_date(forecast_end) || forecast_end < timeline$forecast_start ||
      !dashboard_is_quarter_date(preloss_start) || preloss_start > timeline$forecast_start ||
      !is.character(instrument_variable) || length(instrument_variable) != 1 || instrument_variable == "" ||
      !is.character(instrument_shock) || length(instrument_shock) != 1 || instrument_shock == "" ||
      !is.logical(use_cd) || length(use_cd) != 1 || is.na(use_cd) ||
      !is.numeric(lambda) || length(lambda) != 1 || is.na(lambda) || lambda < 0 || lambda > 1 ||
      !is.function(prepare_baseline) || !is.function(extend_baseline) ||
      (!is.null(loss_variable_vintages) && !is.data.frame(loss_variable_vintages)) ||
      !dashboard_is_named_character(loss_variable_choices) ||
      !dashboard_is_named_character(constraint_variable_choices) ||
      !is.character(default_response_variables) || !is.character(default_loss_variables) ||
      !is.character(elb_variable) || length(elb_variable) != 1 || elb_variable == "" ||
      (!is.null(description_ui) && !is.function(description_ui))) {
    stop("`optimal_policy` contains invalid model, horizon, policy, baseline, vintage, choice-list, or help settings.", call. = FALSE)
  }

  list(
    enabled = TRUE,
    policy_model = policy_model,
    comparison_model = comparison_model,
    forecast_start = timeline$forecast_start,
    forecast_end = forecast_end,
    preloss_start = preloss_start,
    instrument_variable = instrument_variable,
    instrument_shock = instrument_shock,
    use_cd = use_cd,
    lambda = lambda,
    prepare_baseline = prepare_baseline,
    extend_baseline = extend_baseline,
    loss_variable_vintages = loss_variable_vintages,
    loss_variable_choices = loss_variable_choices,
    constraint_variable_choices = constraint_variable_choices,
    default_response_variables = default_response_variables,
    default_loss_variables = default_loss_variables,
    elb_variable = elb_variable,
    help = help,
    description_ui = description_ui,
    loss_variable_defaults = loss_variable_defaults,
    loss_variable_weight_defaults = loss_variable_weight_defaults
  )
}

# Validate an optional modal shown once when a dashboard session starts.
dashboard_validate_startup_modal <- function(startup_modal) {
  if (is.null(startup_modal)) {
    return(NULL)
  }
  if (!inherits(startup_modal, c("shiny.tag", "shiny.tag.list"))) {
    stop("`startup_modal` must be NULL or UI returned by `shiny::modalDialog()`.", call. = FALSE)
  }

  startup_modal
}

#' Create and validate a configuration for an ezdyn dashboard.
#'
#' This constructor only validates supplied objects. It does not read files,
#' load models, build baselines, or start a Shiny app.
#'
#' @param name Dashboard title.
#' @param models Named list of full Dynare or custom-MOO pairs.
#' @param timeline Named list containing quarterly Date values `data_start`,
#'   `data_end`, `forecast_start`, and `forecast_end`.
#' @param baselines Named list of canonical `ezdyn_baseline` objects.
#' @param default_baseline Name of the baseline selected initially.
#' @param instrument_display_name Display name of the policy instrument.
#' @param instrument_shock_name Display name or code of the instrument shock.
#' @param r_slider_range Distance around a baseline level shown by each slider.
#' @param r_slider_step Slider increment.
#' @param cr_button_step Increment used by slider plus/minus controls.
#' @param baseline_upload_source_models Configured model labels available for
#'   translating uploaded source-native baselines.
#' @param default_baseline_upload_source_model Initial source model for uploads.
#' @param allow_display_name_upload Whether users may declare an upload already
#'   uses canonical display-name columns.
#' @param irf Curated Scenario Mode IRF data and defaults. Scenario data must
#'   include one scenario-group label per row in `scenario`; only the
#'   composition root that computes curated scenarios should build this data.
#'   Custom IRF targets always use displayed IRF units.
#' @param overview Overview HTML or UI callback configuration.
#' @param hsd Historical shock-decomposition configuration. `hsd$preamble_ui`
#'   is an optional function returning UI shown above the HSD chart; `NULL`
#'   (the default) shows no preamble.
#' @param alt_paths Alternative Policy Paths model-selection configuration.
#'   `alt_paths$models` is the eligible model allow-list offered by the tab's
#'   model selector (defaults to every configured model); `alt_paths$defaults$models`
#'   is the selector's initial selection (defaults to every eligible model).
#' @param dictionary Variable Dictionary model-selection configuration.
#'   `dictionary$models` is the eligible model allow-list offered by the tab's
#'   model selector (defaults to every configured model); `dictionary$defaults$models`
#'   is the selector's initial selection (defaults to every eligible model).
#' @param optimal_policy Optional policy-module configuration. When enabled,
#'   supplies model roles, a separate policy horizon, explicit instrument codes,
#'   baseline preparation/extension functions, timeless-policy vintages, and
#'   optional composition-supplied UI hooks: `help` (named list of modal-
#'   returning functions keyed by `cd`, `strategy`, `loss_variables`,
#'   `loss_horizon`, `instrument_horizon`, `constraints`, `commitment_start`;
#'   missing keys show a generic fallback modal), `description_ui` (function
#'   returning the tab's introductory UI; a generic placeholder is used when
#'   unset), `loss_variable_defaults` (named numeric target defaults), and
#'   `loss_variable_weight_defaults` (named numeric weight defaults; unlisted
#'   loss variables default to a weight of 1).
#' @param help Optional top-level contextual-help configuration used outside
#'   the Optimal Policy tab. `help$anticipated_shocks` is an optional function
#'   returning UI from [shiny::modalDialog()] for Alternative Policy Paths'
#'   anticipated-shock and cognitive-discounting help buttons; a generic
#'   fallback modal is shown when unset.
#' @param startup_modal Optional UI returned by [shiny::modalDialog()] to show
#'   once when a session starts.
#'
#' @return An object of class `ezdyn_dashboard_config`.
#'
#' @export
configure_dashboard <- function(
    name = "ezdyn Dashboard",
    models,
    timeline,
    baselines,
    default_baseline,
    instrument_display_name = "Cash Rate",
    instrument_shock_name = "Monetary policy shock",
    r_slider_range = 3,
    r_slider_step = 0.005,
    cr_button_step = 0.25,
    baseline_upload_source_models = names(models),
    default_baseline_upload_source_model = NULL,
    allow_display_name_upload = TRUE,
    irf = list(),
    overview = list(),
    hsd = list(),
    alt_paths = list(),
    dictionary = list(),
    optimal_policy = NULL,
    help = list(),
    startup_modal = NULL) {
  if (!is.character(name) || length(name) != 1 || is.na(name) || name == "") {
    stop("`name` must be one non-empty character value.", call. = FALSE)
  }
  if (!is.character(instrument_display_name) || length(instrument_display_name) != 1 ||
      is.na(instrument_display_name) || instrument_display_name == "" ||
      !is.character(instrument_shock_name) || length(instrument_shock_name) != 1 ||
      is.na(instrument_shock_name) || instrument_shock_name == "") {
    stop("Instrument names must be non-empty character values.", call. = FALSE)
  }

  dashboard_validate_models(models)
  timeline <- dashboard_validate_timeline(timeline)
  dashboard_validate_baselines(baselines, default_baseline, timeline)

  if (!is.character(baseline_upload_source_models) ||
      length(baseline_upload_source_models) == 0 ||
      any(!(baseline_upload_source_models %in% names(models)))) {
    stop("`baseline_upload_source_models` must contain configured model labels.", call. = FALSE)
  }
  if (is.null(default_baseline_upload_source_model)) {
    default_baseline_upload_source_model <- baseline_upload_source_models[[1]]
  }
  if (!is.character(default_baseline_upload_source_model) ||
      length(default_baseline_upload_source_model) != 1 ||
      is.na(default_baseline_upload_source_model) ||
      !(default_baseline_upload_source_model %in% baseline_upload_source_models)) {
    stop("`default_baseline_upload_source_model` must name an allowed upload source model.", call. = FALSE)
  }
  if (!is.logical(allow_display_name_upload) || length(allow_display_name_upload) != 1 ||
      is.na(allow_display_name_upload)) {
    stop("`allow_display_name_upload` must be one logical value.", call. = FALSE)
  }

  config <- list(
    name = name,
    models = models,
    timeline = timeline,
    baselines = baselines,
    default_baseline = default_baseline,
    instrument_display_name = instrument_display_name,
    instrument_shock_name = instrument_shock_name,
    slider_preferences = list(
      r_slider_range = dashboard_validate_slider_preference(r_slider_range, "r_slider_range"),
      r_slider_step = dashboard_validate_slider_preference(r_slider_step, "r_slider_step"),
      cr_button_step = dashboard_validate_slider_preference(cr_button_step, "cr_button_step")
    ),
    baseline_upload = list(
      source_models = unique(baseline_upload_source_models),
      default_source_model = default_baseline_upload_source_model,
      allow_display_name_upload = allow_display_name_upload
    ),
    irf = dashboard_validate_irf_config(irf, models),
    overview = dashboard_validate_overview_config(overview),
    hsd = dashboard_validate_hsd_config(hsd, models),
    alt_paths = dashboard_validate_alt_paths_config(alt_paths, models),
    dictionary = dashboard_validate_dictionary_config(dictionary, models),
    optimal_policy = dashboard_validate_optimal_policy_config(optimal_policy, models, timeline),
    help = dashboard_validate_help_config(help),
    startup_modal = dashboard_validate_startup_modal(startup_modal)
  )
  class(config) <- c("ezdyn_dashboard_config", "list")
  config
}
