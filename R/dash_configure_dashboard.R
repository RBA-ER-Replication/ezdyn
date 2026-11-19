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
#
# `hsd_data_start` is an optional extra field (not in `required_dates`): the
# calendar quarter that `t == 1` represents in the Historical Shock
# Decomposition tab (see `ez_hd()`'s `data_start` argument). It defaults to
# `data_start` so existing dashboards keep their current behaviour, but can
# be set separately since it must match a model's true smoothed-estimation-
# sample start, which need not coincide with `data_start` (the baseline/
# Alt Paths display and coverage floor).
dashboard_validate_timeline <- function(timeline) {
  required_dates <- c("data_start", "data_end", "forecast_start", "forecast_end")
  if (!is.list(timeline) || !all(required_dates %in% names(timeline))) {
    stop(
      sprintf("`timeline` must contain %s.", paste(sprintf("`%s`", required_dates), collapse = ", ")),
      call. = FALSE
    )
  }

  timeline$hsd_data_start <- dashboard_or_default(timeline$hsd_data_start, timeline$data_start)
  all_dates <- c(required_dates, "hsd_data_start")
  timeline <- timeline[all_dates]
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
  defaults$shock <- dashboard_or_default(defaults$shock, NULL)
  defaults$target_display_name <- dashboard_or_default(defaults$target_display_name, NULL)

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
  if (!is.null(defaults$shock) &&
      (!is.character(defaults$shock) || length(defaults$shock) != 1 || is.na(defaults$shock))) {
    stop("`irf$defaults$shock` must be one character shock description when supplied.", call. = FALSE)
  }
  if (!is.null(defaults$target_display_name) &&
      (!is.character(defaults$target_display_name) || length(defaults$target_display_name) != 1 ||
       is.na(defaults$target_display_name))) {
    stop("`irf$defaults$target_display_name` must be one character display name when supplied.", call. = FALSE)
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

# Validate the optional Response Notes footnote configuration: reviewed
# caveats shown as a plot footnote in the IRF Library and Alternative Policy
# Paths tabs whenever the named model (and, if given, the named response
# variable) appears in the plotted data. `display_name` is optional - `NA`
# (the default when omitted) applies the note whenever the model appears at
# all, regardless of variable; naming a `display_name` scopes the note to
# one response variable within that model. This lets composition roots (e.g.
# dingo_dashboard.R) attach model-specific or variable-specific caveats ("X is
# a linearised approximation", "Y is derived via Okun's law") without any
# model- or variable-name branching in generic dashboard code.
dashboard_validate_response_notes <- function(response_notes, models) {
  empty <- data.frame(model_name = character(), display_name = character(), note = character(), stringsAsFactors = FALSE)
  if (is.null(response_notes)) {
    return(empty)
  }
  required_columns <- c("model_name", "note")
  if (!is.data.frame(response_notes) || !all(required_columns %in% names(response_notes))) {
    stop("`response_notes` must be a data frame containing `model_name` and `note`.", call. = FALSE)
  }
  if (nrow(response_notes) == 0) {
    return(empty)
  }
  if (!("display_name" %in% names(response_notes))) {
    response_notes$display_name <- NA_character_
  }
  if (anyNA(response_notes$model_name) || any(response_notes$model_name == "") ||
      !all(response_notes$model_name %in% names(models)) ||
      anyNA(response_notes$note) || any(response_notes$note == "")) {
    stop("`response_notes` must have non-empty `model_name` values naming configured models and non-empty `note` values.", call. = FALSE)
  }

  response_notes[c("model_name", "display_name", "note")]
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
  # Initially plotted response variables (`NULL` uses `irf$defaults$display_names`)
  # and the initial cognitive-discounting (anticipation) parameter.
  defaults$lambda <- dashboard_or_default(defaults$lambda, 0.8)
  if ((!is.null(defaults$display_names) && (!is.character(defaults$display_names) || anyNA(defaults$display_names))) ||
      !is.numeric(defaults$lambda) || length(defaults$lambda) != 1 || is.na(defaults$lambda) ||
      defaults$lambda < 0 || defaults$lambda > 1) {
    stop("`alt_paths$defaults` contains invalid display-name or lambda settings.", call. = FALSE)
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

# Label a choice vector: each choice without a name is labelled with its own
# value, so `c("Inflation", "Output")` and `c("Inflation" = "Inflation",
# "Output" = "Output")` are equivalent. Anything but a non-empty character
# vector is returned unchanged, for the caller to reject.
dashboard_label_choices <- function(choices) {
  if (!is.character(choices) || length(choices) == 0 || anyNA(choices) || any(choices == "")) {
    return(choices)
  }
  labels <- names(choices)
  if (is.null(labels)) labels <- choices
  unlabelled <- is.na(labels) | labels == ""
  labels[unlabelled] <- choices[unlabelled]
  stats::setNames(choices, labels)
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

  optimal_policy$models <- dashboard_or_default(optimal_policy$models, names(models))
  if (!is.character(optimal_policy$models) || length(optimal_policy$models) == 0 ||
      anyNA(optimal_policy$models) || any(optimal_policy$models == "") || anyDuplicated(optimal_policy$models) ||
      !all(optimal_policy$models %in% names(models))) {
    stop("`optimal_policy$models` must contain unique configured model labels.", call. = FALSE)
  }
  policy_defaults <- optimal_policy$defaults
  if (is.null(policy_defaults)) {
    policy_defaults <- list()
  }
  if (!is.list(policy_defaults)) {
    stop("`optimal_policy$defaults` must be a list.", call. = FALSE)
  }
  policy_defaults$models <- dashboard_or_default(policy_defaults$models, optimal_policy$models)
  if (!is.character(policy_defaults$models) || length(policy_defaults$models) == 0 ||
      anyNA(policy_defaults$models) || any(policy_defaults$models == "") || anyDuplicated(policy_defaults$models) ||
      !all(policy_defaults$models %in% optimal_policy$models)) {
    stop("`optimal_policy$defaults$models` must contain eligible Optimal Policy model labels.", call. = FALSE)
  }
  baseline_model <- dashboard_or_default(optimal_policy$baseline_model, optimal_policy$models[[1]])
  if (!is.character(baseline_model) || length(baseline_model) != 1 || !(baseline_model %in% optimal_policy$models)) {
    stop("`optimal_policy$baseline_model` must be one eligible Optimal Policy model label.", call. = FALSE)
  }
  forecast_end <- dashboard_or_default(optimal_policy$forecast_end, NULL)
  preloss_start <- dashboard_or_default(optimal_policy$preloss_start, timeline$forecast_start)
  instrument_variable <- dashboard_or_default(optimal_policy$instrument_variable, "r_obs")
  instrument_shock <- dashboard_or_default(optimal_policy$instrument_shock, "eps_r")
  use_cd <- dashboard_or_default(optimal_policy$use_cd, FALSE)
  lambda <- dashboard_or_default(optimal_policy$lambda, 0)
  prepare_baseline <- dashboard_or_default(optimal_policy$prepare_baseline, function(baseline, model, policy_config) baseline)
  extend_baseline <- dashboard_or_default(optimal_policy$extend_baseline, function(baseline, policy_config) baseline)
  loss_variable_vintages <- dashboard_or_default(optimal_policy$loss_variable_vintages, NULL)
  loss_variable_choices <- dashboard_label_choices(dashboard_or_default(optimal_policy$loss_variable_choices, NULL))
  constraint_variable_choices <- dashboard_label_choices(dashboard_or_default(optimal_policy$constraint_variable_choices, NULL))
  if (is.data.frame(loss_variable_vintages) && !all(c("vintage", "date") %in% names(loss_variable_vintages))) {
    stop(
      "`optimal_policy$loss_variable_vintages` needs `vintage` and `date` columns, plus one `<loss variable>_gap_loss` column per loss variable.",
      call. = FALSE
    )
  }
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

  if (!dashboard_is_quarter_date(forecast_end) || forecast_end < timeline$forecast_start ||
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
    models = optimal_policy$models,
    defaults = policy_defaults,
    baseline_model = baseline_model,
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

# Validate optional links shown as buttons under the dashboard title: a named
# character vector of URLs, named by each button's label.
dashboard_validate_header_links <- function(header_links) {
  if (is.null(header_links)) {
    return(NULL)
  }
  if (!dashboard_is_named_character(header_links) || anyNA(header_links) || any(header_links == "")) {
    stop("`header_links` must be NULL or a named character vector of URLs, named by button label.", call. = FALSE)
  }

  header_links
}

# Validate the configured allow-list of plotting backends. Explicit and
# static - not detected from installed packages - so deployments control
# exactly which backends are offered regardless of what happens to be
# installed on the server.
dashboard_validate_plotters <- function(plotters) {
  allowed <- c("ggrba", "ggplot")
  if (!is.character(plotters) || length(plotters) == 0 ||
      anyNA(plotters) || anyDuplicated(plotters) || !all(plotters %in% allowed)) {
    stop(
      sprintf("`plotters` must be a unique, non-empty subset of %s.", paste(sprintf("`%s`", allowed), collapse = ", ")),
      call. = FALSE
    )
  }

  plotters
}

#' Configure an ezdyn dashboard
#'
#' @description
#' `configure_dashboard()` gathers everything an ezdyn dashboard needs - the
#' models, the dates, the baseline forecasts and each tab's settings - into one
#' checked configuration. Pass the result to [ezdyn_dashboard()] to build the
#' Shiny app.
#'
#' It only checks what you give it: it doesn't read files, load models, build
#' baselines or start the app. Load the models with [read_dynare()] or
#' [custom_moo()] and build each baseline with [import_baseline()] first.
#'
#' Only the first four arguments are required. Every other argument has a
#' default that gives a working dashboard: the tab settings come next, then
#' optional extras. The one exception to "the later, the less likely you need
#' it" is the pair of instrument settings, which must match your models'
#' names. The sections below walk through them:
#'
#' 1. **What you must supply**: `models`, `timeline`, `baselines`,
#'    `default_baseline`.
#' 2. **The tabs**: which tabs the dashboard shows, and which argument
#'    controls each.
#' 3. **Two settings to check**: `instrument_display_name` and
#'    `instrument_shock_name`, which must match your models.
#' 4. **The tab settings**: `hsd`, `optimal_policy`, `alt_paths` and `irf`,
#'    each a list of options, with their own sections below.
#' 5. **Optional extras**: everything else, which you can ignore when setting
#'    up a dashboard.
#'
#' A complete, working example is the USA dashboard in the package
#' repository's `dashboard_example/usa_dashboard.R`.
#'
#' @section What you must supply:
#'
#' * **`models`**: a named list of models, each loaded with [read_dynare()]
#'   (a solved Dynare model) or [custom_moo()] (impulse responses from any
#'   other source, e.g. a VAR). The names are the labels the dashboard shows,
#'   e.g. `list("Smets Wouters (2007)" = sw, "My VAR" = var)`. Give each
#'   model a `colour` when you load it, so its lines are easy to tell apart.
#'
#' * **`timeline`**: a list of four dates, each the first day of a quarter's
#'   last month (March, June, September or December, e.g.
#'   `as.Date("2026-09-01")` for 2026Q3):
#'   - `data_start`: the first quarter of history shown;
#'   - `forecast_start`: the first forecast quarter;
#'   - `forecast_end`: the last forecast quarter. Alternative Policy Paths
#'     gives the user one slider per quarter from `forecast_start` to here,
#'     so 8 to 12 quarters is typical;
#'   - `data_end`: the last quarter of the baseline. It can run past
#'     `forecast_end`, e.g. to give Optimal Policy a longer horizon.
#'
#'   They must satisfy `data_start <= forecast_start <= forecast_end <=
#'   data_end`. An optional fifth date, `hsd_data_start`, is only needed if
#'   your models' historical shock decompositions start at a different
#'   quarter from `data_start`.
#'
#' * **`baselines`**: a named list of one or more baseline forecasts, each
#'   built with [import_baseline()], so its columns are display names in the
#'   dashboard's units. Each must cover every quarter from `data_start` to
#'   `forecast_end`. Users choose between them, and can upload more, in the
#'   app.
#'
#' * **`default_baseline`**: the name of the baseline selected when the
#'   dashboard opens.
#'
#' One more requirement: at least one model must have a historical shock
#' decomposition (an estimated Dynare model, with smoothed shocks and
#' variables), because the Historical Shock Decomposition tab is always shown.
#'
#' @section The tabs:
#'
#' | Tab | Shown | Configured by |
#' |---|---|---|
#' | Overview | Only if `overview` supplies content | `overview` |
#' | Alternative Policy Paths | Always | `alt_paths`, the instrument settings |
#' | Impulse Response Library | Always | `irf` |
#' | Historical Shock Decompositions | Always (needs one model with a decomposition) | `hsd` |
#' | Optimal Policy | Only if `optimal_policy` is supplied | `optimal_policy` |
#'
#' Every tab works with its defaults, except Optimal Policy, which needs a
#' few settings of its own (see "Optimal Policy" below).
#'
#' Throughout, variables are referred to by their **display names**, the
#' `display_name` column of each model's metadata (e.g. "Inflation"), not
#' their model codes (e.g. `pinfobs`). Display names are what let one setting
#' apply to several models whose codes differ. A tab only offers a variable
#' that every selected model has.
#'
#' @section Two settings to check:
#'
#' Alternative Policy Paths moves the policy interest rate, and needs to know
#' which variable and shock that is in your models:
#'
#' * `instrument_display_name` (default `"Cash Rate"`): the display name of
#'   the policy rate.
#' * `instrument_shock_name` (default `"Monetary policy shock"`): the
#'   description (or code) of the monetary policy shock.
#'
#' Set them if your models' metadata name these differently, e.g.
#' `instrument_display_name = "Nominal Interest Rate"`. They are the only
#' settings outside the required four that a new dashboard is likely to need.
#' `name` sets the title shown at the top of the dashboard.
#'
#' @section Historical Shock Decompositions (`hsd`):
#'
#' The tab shows how each model attributes history to its shocks. Settings,
#' all optional:
#'
#' * `models`: the models offered. Defaults to every model; models without a
#'   historical shock decomposition are dropped automatically.
#' * `display_names`: the variables selected when the tab opens (default
#'   none).
#' * `shock_group`: the shock grouping selected when the tab opens. A grouping
#'   is any extra column of a model's shock metadata (e.g.
#'   "Demand/Supply/Monetary"), which adds up the contributions of the shocks
#'   in each group. `NULL` (the default) shows each shock separately.
#' * `preamble_ui`: a function returning UI shown above the chart, e.g. a
#'   short explanation.
#'
#' @section Alternative Policy Paths (`alt_paths`):
#'
#' The user sets a path for the policy rate, one slider per forecast quarter,
#' and the tab shows each model's response: the policy shocks that would
#' deliver that path, and their effects on the other variables. Optionally,
#' it uses anticipated (announced) shocks, for models that have them, with a
#' "cognitive discounting" parameter that sets how far ahead people look.
#' Settings, all optional:
#'
#' * `models`: the models offered (default: all).
#' * `defaults$models`: the models selected when the tab opens (default: all
#'   offered models).
#' * `defaults$display_names`: the variables plotted when the tab opens.
#'   Defaults to `irf$defaults$display_names`.
#' * `defaults$lambda`: the initial cognitive discounting parameter, between
#'   0 and 1 (default 0.8).
#'
#' The sliders themselves are set by `instrument_display_name` and, rarely,
#' `r_slider_range`, `r_slider_step` and `cr_button_step` (see "Optional extras" below).
#'
#' ```r
#' alt_paths = list(defaults = list(
#'   models = "Smets Wouters (2007)",
#'   display_names = c("Inflation (year-ended)", "Nominal Interest Rate"),
#'   lambda = 0.8
#' ))
#' ```
#'
#' @section Impulse Response Library (`irf`):
#'
#' The tab shows the models' impulse responses, in up to three modes:
#'
#' * **Custom IRF**: a chosen shock, scaled so that a chosen "target"
#'   variable moves by a chosen amount in the first quarter (e.g. a shock
#'   that raises the policy rate by 1 percentage point). Comparing models this
#'   way puts them on a like-for-like footing.
#' * **Custom IRF (raw shocks)**: a one-unit shock, unscaled.
#' * **Default (Pre-prepared scenarios)**: responses you computed in advance
#'   and supply in `irf$scenarios`. This mode appears only if you do.
#'
#' Settings, all optional, in `irf$defaults`:
#'
#' * `models`: the models selected when the tab opens (default: all).
#' * `display_names`: the variables plotted when the tab opens. The default,
#'   "Trimmed Mean Inflation (ye)", suits RBA models only, so set this to
#'   variables your models have. Alternative Policy Paths uses it too, unless
#'   `alt_paths$defaults$display_names` is set.
#' * `target_display_name`: Custom IRF's initial target variable. Defaults to
#'   the first of `display_names` that can be targeted.
#' * `shock`: the description of the shock selected when the tab opens.
#' * `horizon`: the number of quarters shown (default 40).
#' * `use_cd`, `lambda`: whether Custom IRF starts with cognitive discounting
#'   on (default `FALSE`), and its initial parameter (default 0).
#'
#' `irf$scenarios` is a data frame of pre-computed responses with columns
#' `scenario` (the scenario's label), `t`, `shock`, `dynare_name`,
#' `display_name`, `display_unit`, `value`, `model_name` (a configured model
#' label) and, optionally, `footnote`.
#'
#' ```r
#' irf = list(defaults = list(
#'   display_names = c("Nominal Interest Rate", "Inflation"),
#'   horizon = 20
#' ))
#' ```
#'
#' @section Optimal Policy (`optimal_policy`):
#'
#' The tab finds the policy rate path that minimises a loss function: the
#' weighted sum of squared gaps between chosen variables and their targets.
#' The user picks the loss variables, their weights and targets, the
#' strategy (commitment or discretion), the horizons, and any bounds on
#' variables (including an effective lower bound on the rate). It runs
#' [get_policy_scenario()] for each selected model and strategy, which
#' explains the method: the tab's "loss evaluation horizon" and "optimisation
#' horizon" are its `T_loss` and `T_instrument`, and the tab builds the
#' `<loss variable>_gap_loss` columns itself, from the baseline and the
#' targets the user sets. The tab is only shown if you supply
#' `optimal_policy`, and it then needs:
#'
#' * `forecast_end` (required): the last quarter of the policy horizon, at or
#'   after `timeline$forecast_start`. Often the end of the baseline
#'   (`timeline$data_end`). Every baseline must cover every quarter up to
#'   here, or be extended to it by `extend_baseline` (see "Advanced" below).
#' * `loss_variable_choices` (required): the variables offered for the loss
#'   function, as a character vector of display names, e.g.
#'   `c("Inflation", "Output Growth")`. Each is labelled with its own name; to
#'   relabel one, name it, e.g. `c("Output gap" = "Output")`.
#' * `constraint_variable_choices` (required): the variables that can be
#'   given upper or lower bounds, in the same form.
#' * `instrument_variable` and `instrument_shock`: the policy rate and
#'   monetary policy shock, as display names or codes. Their defaults
#'   (`"r_obs"` and `"eps_r"`) only suit RBA models, so set them, usually to
#'   the same values as `instrument_display_name` and
#'   `instrument_shock_name`.
#'
#' Worth setting for a good first view, all optional:
#'
#' * `defaults$models`: the models selected when the tab opens (default: all
#'   offered). `models` is the list offered (default: all).
#' * `default_response_variables`: the variables plotted when the tab opens.
#' * `default_loss_variables`: the loss variables selected when the tab opens.
#' * `loss_variable_defaults`: initial targets, as a named numeric vector,
#'   e.g. `c("Inflation" = 0.5)`. Unlisted variables start at 0.
#' * `loss_variable_weight_defaults`: initial weights, in the same form.
#'   Unlisted variables start at 1.
#' * `use_cd` and `lambda`: whether the tab offers anticipated policy shocks
#'   (for models that have them), and the initial anticipation parameter.
#'   Without anticipated shocks, commitment and discretion give the same path.
#'
#' Timeless commitment, optional:
#'
#' * `loss_variable_vintages`: past baselines, so that "Commitment" assumes
#'   policy was committed to in some past quarter and re-solved each quarter
#'   since, using the baseline of the time. Without them, Commitment is
#'   ordinary commitment from the forecast start. It's a data frame with
#'   `vintage` and `date` columns and one `<loss variable>_gap_loss` column per
#'   loss variable (e.g. `Inflation_gap_loss`): unlike the baseline, these
#'   must already be gaps, from the target as it stood at the time. The
#'   "ELB" option also needs the policy rate's level, in a column named like
#'   `instrument_variable`: under timeless commitment the ELB is assumed to
#'   have applied in every past vintage too, which can call for "make-up"
#'   policy today. See [get_policy_scenario()] for the full format.
#' * `preloss_start`: the earliest commitment start the user can choose,
#'   usually the first vintage (default `timeline$forecast_start`).
#'
#' Advanced, rarely needed:
#'
#' * `baseline_model`: the model whose metadata names the baseline's
#'   variables (default: the first of `models`).
#' * `prepare_baseline(baseline, model, policy_config)` and
#'   `extend_baseline(baseline, policy_config)`: functions that adjust or
#'   extend the selected baseline before the calculation, e.g. to extend it to
#'   `forecast_end`. Both default to leaving it unchanged.
#' * `elb_variable`: the variable the "ELB" option bounds (default
#'   `instrument_variable`).
#' * `description_ui`: a function returning the tab's introductory UI.
#' * `help`: a named list of functions, each returning a
#'   [shiny::modalDialog()], for the tab's help buttons: `cd`, `strategy`,
#'   `loss_variables`, `loss_horizon`, `instrument_horizon`, `constraints` and
#'   `commitment_start`. Missing ones show a generic message.
#' * `enabled = FALSE`: hides the tab while keeping its settings.
#'
#' ```r
#' optimal_policy = list(
#'   forecast_end = timeline$data_end,
#'   instrument_variable = "Nominal Interest Rate",
#'   instrument_shock = "Monetary policy shock",
#'   loss_variable_choices = c("Inflation", "Output Growth", "Nominal Interest Rate Changes"),
#'   constraint_variable_choices = c("Nominal Interest Rate", "Inflation"),
#'   default_response_variables = c("Inflation", "Output Growth", "Nominal Interest Rate"),
#'   default_loss_variables = c("Inflation", "Output Growth"),
#'   loss_variable_defaults = c("Inflation" = 0.5)
#' )
#' ```
#'
#' @section Optional extras:
#'
#' You can leave all of these at their defaults when setting up a dashboard.
#'
#' * `overview`: an Overview tab, shown first, from either `html_path` (an
#'   HTML file) or `ui` (a function returning Shiny UI).
#' * `startup_modal`: a [shiny::modalDialog()] shown when a session starts,
#'   e.g. a disclaimer.
#' * `response_notes`: footnotes added to charts whenever a given model (and,
#'   optionally, a given variable) is plotted, e.g. to flag a caveat. A data
#'   frame with columns `model_name`, `note` and, optionally, `display_name`.
#' * `header_links`: buttons under the title linking to web pages, as a named
#'   character vector of URLs named by button label.
#' * `r_slider_range`, `r_slider_step`, `cr_button_step`: the Alternative
#'   Policy Paths sliders' range either side of the baseline, step, and the
#'   step of their plus/minus buttons, in the policy rate's units.
#' * `baseline_upload_source_models`, `default_baseline_upload_source_model`,
#'   `allow_display_name_upload`: which models users can choose to translate
#'   an uploaded baseline written in model codes, and whether they can upload
#'   one already in display names.
#' * `help`: `help$anticipated_shocks`, a function returning a
#'   [shiny::modalDialog()] for Alternative Policy Paths' help buttons.
#' * `plotters`: the plotting styles offered, from `"ggplot"` (the default)
#'   and `"ggrba"` (RBA style, which needs the internal `ggrba` package).
#' * `dictionary`: settings for a Variable Dictionary tab, which isn't shown
#'   at present.
#'
#' @param models Named list of models, each from [read_dynare()] or
#'   [custom_moo()]; the names are the labels shown. See "What you must supply" below.
#' @param timeline List of quarterly dates `data_start`, `forecast_start`,
#'   `forecast_end` and `data_end` (each the first day of a quarter's last
#'   month), and optionally `hsd_data_start`. See "What you must supply" below.
#' @param baselines Named list of baselines from [import_baseline()], each
#'   covering `data_start` to `forecast_end`. See "What you must supply" below.
#' @param default_baseline Name of the baseline selected initially.
#' @param hsd Historical Shock Decompositions settings: a list with any of
#'   `models`, `display_names`, `shock_group` and `preamble_ui`. See
#'   "Historical Shock Decompositions" below.
#' @param optimal_policy Optimal Policy settings, or `NULL` (the default) for
#'   no Optimal Policy tab. A list that needs `forecast_end`,
#'   `loss_variable_choices` and `constraint_variable_choices`, and usually
#'   `instrument_variable` and `instrument_shock`. See "Optimal Policy" below.
#' @param alt_paths Alternative Policy Paths settings: a list with any of
#'   `models` and `defaults` (`models`, `display_names`, `lambda`). See
#'   "Alternative Policy Paths" below.
#' @param irf Impulse Response Library settings: a list with any of
#'   `defaults` (`models`, `display_names`, `target_display_name`, `shock`,
#'   `horizon`, `use_cd`, `lambda`) and `scenarios`. See "Impulse Response Library" below.
#' @param overview Overview tab content: a list with `html_path` (an HTML
#'   file) or `ui` (a function returning UI). No Overview tab by default.
#' @param startup_modal A [shiny::modalDialog()] shown once when a session
#'   starts, or `NULL` (the default) for none.
#' @param response_notes Chart footnotes for particular models and variables:
#'   a data frame with columns `model_name`, `note` and, optionally,
#'   `display_name` (`NA` or omitted applies the note whenever the model is
#'   plotted). Shown in the Impulse Response Library and Alternative Policy
#'   Paths tabs. `NULL` (the default) for none.
#' @param name Dashboard title.
#' @param instrument_display_name Display name of the policy rate in your
#'   models. See "Two settings to check" below.
#' @param instrument_shock_name Description (or code) of the monetary policy
#'   shock in your models. See "Two settings to check" below.
#' @param r_slider_range Alternative Policy Paths sliders' range either side
#'   of the baseline, in the policy rate's units.
#' @param r_slider_step Alternative Policy Paths sliders' step.
#' @param cr_button_step Step of the sliders' plus/minus buttons.
#' @param baseline_upload_source_models Models users can choose to translate
#'   an uploaded baseline written in model codes (default: all).
#' @param default_baseline_upload_source_model The model initially chosen for
#'   that (default: the first).
#' @param allow_display_name_upload Whether users can upload a baseline
#'   already in display names and units (default `TRUE`).
#' @param dictionary Variable Dictionary settings: a list with `models` and
#'   `defaults$models`. The tab isn't shown at present.
#' @param help Help for Alternative Policy Paths: a list with
#'   `anticipated_shocks`, a function returning a [shiny::modalDialog()]. A
#'   generic message is shown by default. (Optimal Policy's help is set in
#'   `optimal_policy$help`.)
#' @param header_links Buttons under the title linking to web pages: a named
#'   character vector of URLs, named by button label, e.g.
#'   `c("Read the paper" = "https://...")`. `NULL` (the default) for none.
#' @param plotters Plotting styles offered, from `"ggplot"` (the default)
#'   and `"ggrba"`. The style control is shown only when there are both;
#'   `"ggrba"` needs the internal `ggrba` package.
#'
#' @return An `ezdyn_dashboard_config` object, to pass to
#'   [ezdyn_dashboard()].
#'
#' @seealso [ezdyn_dashboard()] to build the app; [read_dynare()],
#'   [custom_moo()] and [import_baseline()] to prepare its inputs;
#'   [get_policy_scenario()] for Optimal Policy's calculation and vintage
#'   format.
#'
#' @examples
#' \dontrun{
#' # 1. A minimal dashboard: one estimated model and one baseline -----------
#' sw <- read_dynare(
#'   path = "inputs/SW_2007_45.json",
#'   path_meta = "inputs/variable_metadata.xlsx",
#'   model_name = "Smets Wouters (2007)",
#'   colour = "forestgreen"
#' )
#' models <- list("Smets Wouters (2007)" = sw)
#' baseline <- import_baseline("inputs/baseline.xlsx", source_model = sw, models = models)
#' timeline <- list(
#'   data_start = as.Date("2010-03-01"),
#'   forecast_start = as.Date("2026-09-01"),
#'   forecast_end = as.Date("2028-12-01"),
#'   data_end = as.Date("2028-12-01")
#' )
#'
#' config <- configure_dashboard(
#'   models = models,
#'   timeline = timeline,
#'   baselines = list("Baseline forecast" = baseline),
#'   default_baseline = "Baseline forecast",
#'   # Match the models' names for the policy rate and its shock (see "Two settings to check")
#'   instrument_display_name = "Nominal Interest Rate",
#'   instrument_shock_name = "Monetary policy shock"
#' )
#' shiny::runApp(ezdyn_dashboard(config))
#'
#' # 2. Several models, with tab defaults set --------------------------------
#' var <- custom_moo(
#'   irf = read.csv("inputs/var_irfs.csv"),
#'   model_name = "My VAR",
#'   meta = "inputs/var_metadata.xlsx",
#'   colour = "firebrick"
#' )
#' models <- list("Smets Wouters (2007)" = sw, "My VAR" = var)
#' plotted <- c("Inflation (year-ended)", "Output Growth (year-ended)", "Nominal Interest Rate")
#'
#' config <- configure_dashboard(
#'   models = models,
#'   timeline = timeline,
#'   baselines = list("Baseline forecast" = baseline),
#'   default_baseline = "Baseline forecast",
#'   hsd = list(display_names = "Inflation", shock_group = "Demand/Supply/Monetary"),
#'   alt_paths = list(defaults = list(models = "Smets Wouters (2007)", display_names = plotted)),
#'   irf = list(defaults = list(display_names = c("Nominal Interest Rate", "Inflation"), horizon = 20)),
#'   name = "My Dashboard",
#'   instrument_display_name = "Nominal Interest Rate",
#'   instrument_shock_name = "Monetary policy shock"
#' )
#'
#' # 3. Adding the Optimal Policy tab -----------------------------------------
#' # The baseline must run to `optimal_policy$forecast_end`, so here it (and
#' # `data_end`) runs ten years past the Alternative Paths window.
#' timeline$data_end <- as.Date("2038-06-01")
#' # Optional past baselines for timeless commitment (see "Optimal Policy" below), e.g.
#' # as built by the package repository's dashboard_example/build_us_baseline.R
#' vintages <- readxl::read_excel("inputs/us_vintages.xlsx", sheet = "vintages")
#'
#' config <- configure_dashboard(
#'   models = models,
#'   timeline = timeline,
#'   baselines = list("Baseline forecast" = baseline),
#'   default_baseline = "Baseline forecast",
#'   optimal_policy = list(
#'     forecast_end = timeline$data_end,
#'     instrument_variable = "Nominal Interest Rate",
#'     instrument_shock = "Monetary policy shock",
#'     loss_variable_choices = c("Inflation", "Output Growth", "Nominal Interest Rate Changes"),
#'     constraint_variable_choices = c("Nominal Interest Rate", "Inflation"),
#'     default_response_variables = plotted,
#'     default_loss_variables = c("Inflation", "Output Growth"),
#'     # Targets in the variables' units: 2% a year inflation is 0.5 a quarter
#'     loss_variable_defaults = c("Inflation" = 0.5, "Output Growth" = 0.45),
#'     # Offer anticipated policy shocks, for models that have them
#'     use_cd = TRUE,
#'     lambda = 0.8,
#'     # Optional: past baselines, for timeless commitment
#'     loss_variable_vintages = vintages,
#'     preloss_start = min(vintages$vintage)
#'   ),
#'   instrument_display_name = "Nominal Interest Rate",
#'   instrument_shock_name = "Monetary policy shock"
#' )
#'
#' # 4. Optional extras ----------------------------------------------------------
#' config <- configure_dashboard(
#'   models = models,
#'   timeline = timeline,
#'   baselines = list("Baseline forecast" = baseline),
#'   default_baseline = "Baseline forecast",
#'   overview = list(html_path = "overview.html"),
#'   startup_modal = shiny::modalDialog(title = "Welcome", "Results are indicative only."),
#'   response_notes = data.frame(
#'     model_name = "My VAR",
#'     display_name = NA,
#'     note = "Estimated over 1990-2019."
#'   ),
#'   header_links = c("Read the paper" = "https://example.com/paper.pdf"),
#'   instrument_display_name = "Nominal Interest Rate",
#'   instrument_shock_name = "Monetary policy shock"
#' )
#' }
#'
#' @md
#' @export
configure_dashboard <- function(
    models,
    timeline,
    baselines,
    default_baseline,
    hsd = list(),
    optimal_policy = NULL,
    alt_paths = list(),
    irf = list(),
    overview = list(),
    startup_modal = NULL,
    response_notes = NULL,
    name = "ezdyn Dashboard",
    instrument_display_name = "Cash Rate",
    instrument_shock_name = "Monetary policy shock",
    r_slider_range = 3,
    r_slider_step = 0.005,
    cr_button_step = 0.25,
    baseline_upload_source_models = names(models),
    default_baseline_upload_source_model = NULL,
    allow_display_name_upload = TRUE,
    dictionary = list(),
    help = list(),
    header_links = NULL,
    plotters = "ggplot") {
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
    response_notes = dashboard_validate_response_notes(response_notes, models),
    optimal_policy = dashboard_validate_optimal_policy_config(optimal_policy, models, timeline),
    help = dashboard_validate_help_config(help),
    startup_modal = dashboard_validate_startup_modal(startup_modal),
    header_links = dashboard_validate_header_links(header_links),
    plotters = dashboard_validate_plotters(plotters)
  )
  class(config) <- c("ezdyn_dashboard_config", "list")
  config
}
