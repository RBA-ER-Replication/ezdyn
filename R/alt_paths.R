# alt_paths.R
# Alternative Policy Paths in ezdyn. 
# Anna Liang, 2026

# Alternative-path calculations ------------------------------------------

# 1. Input validation and normalization -----------------------------------

# Convert one explicit alternative-path date input to `Date`.
ezdyn_alt_path_date <- function(value, name) {
  date <- suppressWarnings(as.Date(value))

  if (length(date) != 1 || is.na(date)) {
    stop(sprintf("`%s` must be one valid date.", name), call. = FALSE)
  }

  date
}

# Normalize vector or wide-table alternative paths to the forecast quarter sequence.
ezdyn_normalize_alt_paths <- function(alt_paths, forecast_dates) {
  # 1. Convert a single numeric path into the standard wide input shape.
  if (is.numeric(alt_paths) && is.atomic(alt_paths) && is.null(dim(alt_paths))) {
    if (length(alt_paths) != length(forecast_dates)) {
      stop("A numeric alternative path must have one value per forecast quarter.", call. = FALSE)
    }

    alt_paths <- tibble::tibble(
      date = forecast_dates,
      Alternative = unname(alt_paths)
    )
  }

  # 2. Reuse baseline date normalization, then enforce the forecast horizon.
  if (!is.data.frame(alt_paths)) {
    stop("`alt_paths` must be a numeric vector or data frame.", call. = FALSE)
  }

  alt_paths <- ezdyn_parse_baseline_dates(alt_paths)
  path_names <- setdiff(names(alt_paths), "date")

  if (length(path_names) == 0 || anyNA(path_names) || any(path_names == "") ||
      any(grepl("^\\.\\.\\.?[0-9]*$", path_names))) {
    stop("Alternative paths must have explicit scenario names.", call. = FALSE)
  }
  if (!identical(alt_paths$date, forecast_dates)) {
    stop("Alternative-path dates must exactly match the inclusive quarterly forecast range.", call. = FALSE)
  }

  invalid_paths <- purrr::keep(path_names, function(path_name) {
    values <- alt_paths[[path_name]]
    !is.numeric(values) || anyNA(values) || any(!is.finite(values))
  })
  if (length(invalid_paths) > 0) {
    stop(
      sprintf("Alternative path(s) must contain finite numeric values: %s.", paste(sprintf("`%s`", invalid_paths), collapse = ", ")),
      call. = FALSE
    )
  }

  tibble::as_tibble(alt_paths)
}

# Validate all public `get_alt_paths()` inputs and return normalized inputs.
ezdyn_validate_get_alt_paths <- function(alt_paths, models, baseline, use_cd,
                                         lambda, instrument_display_name,
                                         instrument_shock_name, forecast_start,
                                         forecast_end, data_start) {
  # 1. Validate model, timeline, and solver inputs.
  ezdyn_validate_baseline_models(models)
  forecast_start <- ezdyn_alt_path_date(forecast_start, "forecast_start")
  forecast_end <- ezdyn_alt_path_date(forecast_end, "forecast_end")
  data_start <- ezdyn_alt_path_date(data_start, "data_start")

  if (data_start > forecast_start || forecast_start > forecast_end) {
    stop("Require `data_start <= forecast_start <= forecast_end`.", call. = FALSE)
  }
  if (!inherits(baseline, "ezdyn_baseline")) {
    stop("`baseline` must be the canonical result of `import_baseline()`.", call. = FALSE)
  }
  if (!is.logical(use_cd) || length(use_cd) != 1 || is.na(use_cd)) {
    stop("`use_cd` must be one logical value.", call. = FALSE)
  }
  if (!is.numeric(lambda) || length(lambda) != 1 || is.na(lambda) || lambda < 0 || lambda > 1) {
    stop("`lambda` must be one numeric value in [0, 1].", call. = FALSE)
  }
  if (!is.character(instrument_display_name) || length(instrument_display_name) != 1 ||
      is.na(instrument_display_name) || instrument_display_name == "" ||
      !is.character(instrument_shock_name) || length(instrument_shock_name) != 1 ||
      is.na(instrument_shock_name) || instrument_shock_name == "") {
    stop("Instrument names must be non-empty character values.", call. = FALSE)
  }

  # 2. Validate the canonical baseline and derive expected date sequences.
  if (!inherits(baseline$date, "Date") || anyDuplicated(baseline$date) ||
      is.unsorted(baseline$date, strictly = TRUE)) {
    stop("`baseline$date` must contain strictly increasing unique Date values.", call. = FALSE)
  }
  if (!identical(baseline$date, seq(min(baseline$date), max(baseline$date), by = "quarter"))) {
    stop("`baseline$date` must be a complete quarterly sequence.", call. = FALSE)
  }

  forecast_dates <- seq(forecast_start, forecast_end, by = "quarter")
  output_dates <- baseline$date[baseline$date >= data_start & baseline$date <= forecast_end]
  previous_forecast_date <- lubridate::`%m-%`(
    forecast_start,
    lubridate::period(month = 3)
  )
  required_dates <- c(data_start, previous_forecast_date, forecast_dates)

  if (!all(required_dates %in% baseline$date)) {
    stop("`baseline` must include the output range and quarter before `forecast_start`.", call. = FALSE)
  }

  # 3. Return one normalized object for the calculation functions.
  list(
    alt_paths = ezdyn_normalize_alt_paths(alt_paths, forecast_dates),
    models = models,
    baseline = baseline,
    use_cd = use_cd,
    lambda = lambda,
    instrument_display_name = instrument_display_name,
    instrument_shock_name = instrument_shock_name,
    forecast_dates = forecast_dates,
    output_dates = output_dates
  )
}

# 2. Model metadata and baseline preparation -----------------------------

# Convert the canonical wide baseline to long display-name levels only.
ezdyn_alt_path_baseline_levels <- function(baseline, output_dates) {
  baseline |>
    dplyr::filter(date %in% output_dates) |>
    tidyr::pivot_longer(-date, names_to = "display_name", values_to = "baseline_value")
}

# Attach pretty targeted-IRF metadata to shared baseline levels once.
ezdyn_alt_path_shared_baseline <- function(baseline_levels, response_metadata) {
  baseline_levels |>
    dplyr::inner_join(response_metadata, by = "display_name") |>
    dplyr::transmute(
      date,
      dynare_name = NA_character_,
      display_name,
      display_unit,
      unit_symbol_irf,
      unit_symbol_baseline,
      path_name = "Baseline",
      model_name = NA_character_,
      value = baseline_value,
      effective_use_cd = NA
    )
}

# Return pretty-IRF metadata with one record per display name.
ezdyn_alt_path_response_metadata <- function(irf) {
  irf |>
    dplyr::distinct(display_name, .keep_all = TRUE) |>
    dplyr::select(
      display_name,
      dynare_name,
      display_unit,
      unit_symbol_irf,
      unit_symbol_baseline
    )
}

# Keep pretty-IRF responses supported by complete canonical baseline levels.
ezdyn_alt_path_usable_metadata <- function(response_metadata, baseline_levels, model_name) {
  # A missing baseline column means no level path was supplied for that response,
  # so it is outside this calculation's level-path output by construction.
  response_metadata <- response_metadata |>
    dplyr::filter(display_name %in% baseline_levels$display_name)

  baseline_summary <- baseline_levels |>
    dplyr::group_by(display_name) |>
    dplyr::summarise(has_missing_value = any(is.na(baseline_value)), .groups = "drop")

  incomplete_display_names <- baseline_summary |>
    dplyr::filter(has_missing_value) |>
    dplyr::pull(display_name) |>
    intersect(response_metadata$display_name)

  if (length(incomplete_display_names) > 0) {
    warning(
      sprintf("Model `%s` has incomplete baseline values for: %s; omitting those responses.", model_name, paste(sprintf("`%s`", incomplete_display_names), collapse = ", ")),
      call. = FALSE
    )
  }

  response_metadata |>
    dplyr::filter(!(display_name %in% incomplete_display_names))
}

# Determine whether a model supports cognitive discounting for the target shock.
ezdyn_alt_path_effective_cd <- function(model, model_name, use_cd, lambda,
                                        shock_code, forecast_dates) {
  available_shocks <- if (inherits(model$oo_, "custom_moo")) {
    unique(model$oo_$irf$shock)
  } else {
    unique(model$M_$exo.names)
  }

  if (!(shock_code %in% available_shocks)) {
    stop(sprintf("Model `%s` does not provide instrument shock `%s`.", model_name, shock_code), call. = FALSE)
  }

  anticipated_shocks <- paste0(shock_code, "_", seq_along(forecast_dates))
  effective_use_cd <- isTRUE(use_cd) && lambda > 0 && all(anticipated_shocks %in% available_shocks)

  if (isTRUE(use_cd) && lambda > 0 && !effective_use_cd) {
    warning(
      sprintf("Model `%s` does not support anticipated shock(s) for `%s`; using unanticipated target IRFs.", model_name, shock_code),
      call. = FALSE
    )
  }

  effective_use_cd
}

# 3. Alternative-path calculation ----------------------------------------

# Calculate every alternative scenario for one model in display-name level space.
ezdyn_alt_path_model_paths <- function(alt_paths, model, model_name,
                                       baseline_levels, instrument_display_name,
                                       shock_code, effective_use_cd, lambda,
                                       forecast_dates) {
  # 1. Get the instrument's level baseline, which is the only baseline series
  #    needed to construct the targeted path deviations.
  instrument_baseline <- baseline_levels |>
    dplyr::filter(display_name == instrument_display_name, date %in% forecast_dates) |>
    dplyr::arrange(date)

  if (nrow(instrument_baseline) != length(forecast_dates) || anyNA(instrument_baseline$baseline_value)) {
    stop(sprintf("Model `%s` is missing forecast baseline values for instrument `%s`.", model_name, instrument_display_name), call. = FALSE)
  }

  alt_paths |>
    tidyr::pivot_longer(-date, names_to = "path_name", values_to = "target_value") |>
    dplyr::left_join(
      instrument_baseline |>
        dplyr::select(date, baseline_value),
      by = "date"
    ) |>
    dplyr::mutate(deviation = target_value - baseline_value) |>
    dplyr::group_split(path_name) |>
    purrr::map_dfr(function(path) {
      path_name <- unique(path$path_name)
      target <- stats::setNames(list(path$deviation), instrument_display_name)
      shock_timing <- stats::setNames(list(seq_along(forecast_dates)), shock_code)

      # 2. Compute a pretty targeted IRF. It is the authoritative source of
      #    model-specific names, units, symbols, and output `dynare_name`.
      irf <- if (effective_use_cd) {
        get_irf_target_gabaix(
          model$M_, model$oo_, length(forecast_dates), target, shock_timing,
          lambda, pretty = TRUE, shock_nickname = path_name
        )
      } else {
        get_irf_target(
          model$M_, model$oo_, length(forecast_dates), target, shock_timing,
          pretty = TRUE, shock_nickname = path_name
        )
      }

      # 3. Exclude response variables without complete canonical baseline
      #    levels, then join deviations to levels by date and display name only.
      response_metadata <- ezdyn_alt_path_response_metadata(irf)
      usable_metadata <- ezdyn_alt_path_usable_metadata(
        response_metadata,
        baseline_levels,
        model_name
      )
      if (!(instrument_display_name %in% usable_metadata$display_name)) {
        stop(sprintf("Model `%s` is missing forecast baseline values for instrument `%s`.", model_name, instrument_display_name), call. = FALSE)
      }

      irf_levels <- irf |>
        dplyr::mutate(date = forecast_dates[t]) |>
        dplyr::inner_join(
          usable_metadata,
          by = c(
            "dynare_name",
            "display_name",
            "display_unit",
            "unit_symbol_irf",
            "unit_symbol_baseline"
          )
        ) |>
        dplyr::select(date, display_name, irf_value = value)

      baseline_levels |>
        dplyr::inner_join(usable_metadata, by = "display_name") |>
        dplyr::left_join(irf_levels, by = c("date", "display_name")) |>
        dplyr::mutate(
          path_name = path_name,
          model_name = model_name,
          value = dplyr::coalesce(baseline_value + irf_value, baseline_value),
          effective_use_cd = effective_use_cd
        ) |>
        dplyr::select(
          date,
          dynare_name,
          display_name,
          display_unit,
          unit_symbol_irf,
          unit_symbol_baseline,
          path_name,
          model_name,
          value,
          effective_use_cd
        )
    })
}

# Resolve one model's instrument and run its complete alternative-path calculation.
ezdyn_alt_path_model_result <- function(inputs, model, model_name, baseline_levels) {
  # 1. Resolve the named instrument separately for each model.
    instrument_code <- ezdyn_resolve_variable_names(
    inputs$instrument_display_name,
    model$M_
  )
  shock_code <- ezdyn_resolve_shock_names(
    inputs$instrument_shock_name,
    model$M_
  )
  instrument_display_name <- display_name(instrument_code, model$M_)

    if (length(instrument_display_name) != 1 || is.na(instrument_display_name) ||
      !(instrument_display_name %in% baseline_levels$display_name)) {
    stop(
      sprintf("Model `%s` does not have baseline metadata for instrument `%s`.", model_name, inputs$instrument_display_name),
      call. = FALSE
    )
  }

  # 2. Determine whether cognitive discounting is supported for the model.
  # (does the model have anticipated shocks for the instrument shock?)
  effective_use_cd <- ezdyn_alt_path_effective_cd(
    model,
    model_name,
    inputs$use_cd,
    inputs$lambda,
    shock_code,
    inputs$forecast_dates
  )

  # 3. Calculate every supplied policy path for the current model.
  ezdyn_alt_path_model_paths(
    inputs$alt_paths,
    model,
    model_name,
    baseline_levels,
    instrument_display_name,
    shock_code,
    effective_use_cd,
    inputs$lambda,
    inputs$forecast_dates
  )
}

#' Calculate alternative policy paths for multiple models.
#'
#' Joins a canonical [import_baseline()] result to each named alternative policy
#' path, solves targeted IRFs for every requested model, and returns level paths.
#' The returned baseline is shared and has missing `model_name` and `dynare_name`.
#' If a model cannot supply anticipated shocks, cognitive discounting falls back
#' to ordinary targeted IRFs with a warning and `effective_use_cd = FALSE`.
#'
#' @param alt_paths Numeric vector or data frame of named alternative policy paths.
#' @param models Named list of full model MOO pairs.
#' @param baseline Canonical result of [import_baseline()].
#' @param use_cd Whether to use cognitive discounting where supported.
#' @param lambda Cognitive discounting parameter in $[0, 1]$.
#' @param instrument_display_name Instrument variable display or Dynare name.
#' @param instrument_shock_name Instrument shock description or code.
#' @param forecast_start First forecast quarter.
#' @param forecast_end Last forecast quarter.
#' @param data_start First baseline quarter returned.
#'
#' @return Pretty tibble containing the shared baseline and model-specific paths.
#' @export
get_alt_paths <- function(alt_paths, models, baseline, use_cd = FALSE, lambda = 0,
                          instrument_display_name = "Cash Rate", instrument_shock_name = "Monetary policy shock",
                          forecast_start, forecast_end, data_start) {
  inputs <- ezdyn_validate_get_alt_paths(
    alt_paths = alt_paths,
    models = models,
    baseline = baseline,
    use_cd = use_cd,
    lambda = lambda,
    instrument_display_name = instrument_display_name,
    instrument_shock_name = instrument_shock_name,
    forecast_start = forecast_start,
    forecast_end = forecast_end,
    data_start = data_start
  )

  # 1. Convert the canonical baseline to levels keyed only by date/display name.
  baseline_levels <- ezdyn_alt_path_baseline_levels(
    inputs$baseline,
    inputs$output_dates
  )

  # 2. Calculate every scenario for every model. Pretty IRFs provide metadata.
  alternatives <- purrr::imap_dfr(inputs$models, function(model, model_name) {
    ezdyn_alt_path_model_result(
      inputs,
      model,
      model_name,
      baseline_levels
    )
  })

  # 3. Add one shared Baseline path, labelled using the alternative IRF output.
  response_metadata <- ezdyn_alt_path_response_metadata(alternatives)
  shared_baseline <- ezdyn_alt_path_shared_baseline(
    baseline_levels,
    response_metadata
  )

  # 4. Return baseline first within each display-name/date group.
  dplyr::bind_rows(shared_baseline, alternatives) |>
    dplyr::mutate(baseline_order = dplyr::if_else(path_name == "Baseline", 0L, 1L)) |>
    dplyr::arrange(display_name, date, baseline_order, model_name, path_name) |>
    dplyr::select(-baseline_order)
}


#' Plot alternative paths with [plot_pretty()].
#'
#' Compatibility wrapper for the older alternative-path plotting interface.
#' It filters one model and date range, then returns the same list as
#' [plot_pretty()].
#'
#' @param df_raw Output from [get_alt_paths()].
#' @param plot_variables Vector of native variable names for `plot_model`.
#' @param agg_ye Whether to keep year-ended rather than quarterly rows when both
#'   are available for a response.
#' @param incl_baseline Whether to include the shared Baseline path.
#' @param plot_model Model label to plot.
#' @param plot_start_date First plotting date.
#' @param plot_end_date Last plotting date.
#' @param plotter Plotting backend. `"ggrba"` (default) or optional `"ggplot"`.
#'
#' @return List with `graph`, `graph_data`, and `graph_fname`.
#' @export
plot_alt_paths <- function(df_raw, plot_variables, agg_ye = FALSE,
                           incl_baseline = TRUE, plot_model = "DINGO",
                           plot_start_date, plot_end_date, plotter="ggrba") {
  # 1. Retain the legacy model/date selection before using the shared plotter.
  df <- df_raw |>
    dplyr::filter(model_name == plot_model | path_name == "Baseline") |>
    dplyr::filter(date >= as.Date(plot_start_date)) |>
    dplyr::filter(date <= as.Date(plot_end_date))

  # 2. Preserve the legacy frequency selection without owning plot creation.
  if (isTRUE(agg_ye)) {
    df <- df |>
      dplyr::group_by(display_name, path_name, model_name) |>
      dplyr::mutate(
        is_ye = tolower(display_unit) %in% c("year-ended", "year ended"),
        has_ye = any(is_ye, na.rm = TRUE),
        has_non_ye = any(!is_ye, na.rm = TRUE)
      ) |>
      dplyr::filter(!has_ye | is_ye) |>
      dplyr::ungroup() |>
      dplyr::select(-is_ye, -has_ye, -has_non_ye)
  } else {
    df <- df |>
      dplyr::filter(!tolower(display_unit) %in% c("year-ended", "year ended"))
  }
  if (!isTRUE(incl_baseline)) {
    df <- df |>
      dplyr::filter(path_name != "Baseline")
  }

  # 3. Use the canonical plotter for labels, units, colours, and graph data.
  plot_pretty(
    df = df,
    dynare_names = plot_variables,
    plotter = plotter,
    baseline_name = "Baseline",
    collapse_baseline = TRUE
  )
}

# Helper functions ----
# TODO: Consider relocation.
#' Calculate year-ended growth from quarterly growths.
QGROWTH_TO_YE <- function(x) {(1+x/100)*(1+dplyr::lag(x)/100)*(1+dplyr::lag(x,2)/100)*(1+dplyr::lag(x,3)/100)*100-100}

# Convert quarterly growth to year-ended growth where four consecutive quarters exist.
# TODO: Clean up. 
ezdyn_alt_path_ye_values <- function(date, value) {
  tibble::tibble(date = date, value = value) |>
    dplyr::mutate(
      previous_date_1 = dplyr::lag(date),
      previous_date_2 = dplyr::lag(date, 2),
      previous_date_3 = dplyr::lag(date, 3),
      previous_value_1 = dplyr::lag(value),
      previous_value_2 = dplyr::lag(value, 2),
      previous_value_3 = dplyr::lag(value, 3),
      has_four_quarters =
        !is.na(value) & !is.na(previous_value_1) &
        !is.na(previous_value_2) & !is.na(previous_value_3) &
        previous_date_1 == lubridate::`%m-%`(date, lubridate::period(month = 3)) &
        previous_date_2 == lubridate::`%m-%`(date, lubridate::period(month = 6)) &
        previous_date_3 == lubridate::`%m-%`(date, lubridate::period(month = 9)),
      value = dplyr::if_else(
        has_four_quarters,
        (1 + value / 100) * (1 + previous_value_1 / 100) *
          (1 + previous_value_2 / 100) * (1 + previous_value_3 / 100) * 100 - 100,
        NA_real_,
        missing = NA_real_
      )
    ) |>
    dplyr::pull(value)
}

# Validate input and identify the alternative responses to transform.
ezdyn_validate_alt_paths_ye <- function(pretty_df, output_ye_vars, forecast_start) {
  required_columns <- c(
    "date", "dynare_name", "display_name", "display_unit", "path_name",
    "model_name", "value"
  )
  if (!is.data.frame(pretty_df) || !all(required_columns %in% names(pretty_df))) {
    stop("`pretty_df` must be a data frame returned by `get_alt_paths()`.", call. = FALSE)
  }

  pretty_df <- tibble::as_tibble(pretty_df)
  if (!inherits(pretty_df$date, "Date") || anyNA(pretty_df$date) || !is.numeric(pretty_df$value)) {
    stop("`pretty_df` must contain non-missing Date values and numeric `value` values.", call. = FALSE)
  }
  ezdyn_alt_path_date(forecast_start, "forecast_start")

  available_vars <- pretty_df |>
    dplyr::filter(path_name != "Baseline", !is.na(dynare_name)) |>
    dplyr::pull(dynare_name) |>
    unique()
  if (length(available_vars) == 0) {
    stop("`pretty_df` must contain at least one alternative response.", call. = FALSE)
  }

  transform_all <- length(output_ye_vars) == 1 && is.na(output_ye_vars)
  if (!transform_all &&
      (!is.character(output_ye_vars) || length(output_ye_vars) == 0 || anyNA(output_ye_vars))) {
    stop("`output_ye_vars` must be `NA` or a non-empty character vector of Dynare names.", call. = FALSE)
  }

  selected_vars <- if (transform_all) available_vars else unique(output_ye_vars)
  unknown_vars <- setdiff(selected_vars, available_vars)
  if (length(unknown_vars) > 0) {
    stop(
      sprintf("Unknown `output_ye_vars`: %s.", paste(sprintf("`%s`", unknown_vars), collapse = ", ")),
      call. = FALSE
    )
  }

  selected_display_names <- pretty_df |>
    dplyr::filter(path_name != "Baseline", dynare_name %in% selected_vars) |>
    dplyr::pull(display_name) |>
    unique()

  list(
    pretty_df = pretty_df,
    selected_vars = selected_vars,
    selected_display_names = selected_display_names
  )
}

# Apply year-ended labels without duplicating an existing suffix.
ezdyn_alt_path_ye_labels <- function(path) {
  path |>
    dplyr::mutate(
      display_unit = "Year-ended",
      display_name = dplyr::if_else(
        is.na(display_name) | grepl(" \\(YE\\)$", display_name),
        display_name,
        paste0(display_name, " (YE)")
      )
    )
}

#' Convert selected quarterly alternative paths to year-ended rates.
#'
#' Transforms only the requested alternative `dynare_name` values. Their shared
#' Baseline rows are transformed by `display_name`; all other rows are retained
#' unchanged. Values are calculated only where four consecutive quarters exist.
#'
#' @param pretty_df Output pretty data frame from [get_alt_paths()].
#' @param output_ye_vars Optional character vector of variable names in
#'   `dynare_name` format to transform. If `NA` (default), all variables are
#'   transformed.
#' @param forecast_start First forecast date. It is validated for compatibility
#'   with the alternative-path API.
#'
#' @return A pretty data frame with the same rows and columns as `pretty_df`.
#'   Selected rows have year-ended values, `display_unit = "Year-ended"`, and a
#'   ` (YE)` display-name suffix. Initial rows without four quarters remain `NA`.
#' @export
get_alt_paths_ye <- function(pretty_df, output_ye_vars = NA, forecast_start) {
  inputs <- ezdyn_validate_alt_paths_ye(pretty_df, output_ye_vars, forecast_start)
  pretty_df <- inputs$pretty_df |>
    dplyr::mutate(ezdyn_row_order = dplyr::row_number())

  # 1. Transform one shared Baseline series for every selected display name.
  baseline_ye <- pretty_df |>
    dplyr::filter(
      path_name == "Baseline",
      display_name %in% inputs$selected_display_names
    ) |>
    dplyr::group_split(display_name) |>
    purrr::map_dfr(function(path) {
      path |>
        dplyr::arrange(date) |>
        dplyr::mutate(value = ezdyn_alt_path_ye_values(date, value)) |>
        ezdyn_alt_path_ye_labels()
    })

  # 2. Transform each alternative with only its matching prior Baseline history.
  alternative_ye <- pretty_df |>
    dplyr::filter(path_name != "Baseline", dynare_name %in% inputs$selected_vars) |>
    dplyr::group_split(model_name, dynare_name, display_name, path_name) |>
    purrr::map_dfr(function(path) {
      current_display_name <- unique(path$display_name)
      path_start <- min(path$date)
      baseline_history <- pretty_df |>
        dplyr::filter(
          path_name == "Baseline",
          display_name == current_display_name,
          date < path_start
        ) |>
        dplyr::mutate(ezdyn_is_alternative = FALSE)

      dplyr::bind_rows(
        baseline_history,
        path |>
          dplyr::mutate(ezdyn_is_alternative = TRUE)
      ) |>
        dplyr::arrange(date) |>
        dplyr::mutate(value = ezdyn_alt_path_ye_values(date, value)) |>
        dplyr::filter(ezdyn_is_alternative) |>
        dplyr::select(-ezdyn_is_alternative) |>
        ezdyn_alt_path_ye_labels()
    })

  # 3. Bind transformed rows back to untouched rows in their original order.
  transformed <- dplyr::bind_rows(baseline_ye, alternative_ye)
  untouched <- pretty_df |>
    dplyr::filter(!(ezdyn_row_order %in% transformed$ezdyn_row_order))

  dplyr::bind_rows(untouched, transformed) |>
    dplyr::arrange(ezdyn_row_order) |>
    dplyr::select(-ezdyn_row_order)

}

#' Prepare alternative paths output as a wide table
#'
#' @param pretty_df Output pretty data frame from get_alt_paths() or
#'   get_alt_paths_ye().
#' @param output_vars Optional vector or list of variable names in
#'   `dynare_name` format to keep in the output. If `NA` (default), all
#'   variables are included.
#' @param baseline_name_value Scalar baseline label to include in the export.
#' @param forecasts_start_date First forecast date to include for alternative paths.
#' @param output_start_date First baseline-history date to include.
#' @param output_end_date Last date to include in the export.
#' @param include_baseline_path Whether to include a separate shared Baseline
#'   row covering the output range.
#'
#' @return A data frame with `Model`, `Mnemonic`, `Variable Name`, `Baseline Name`,
#'   `Path Name`, and one column per quarter containing the corresponding values.
#'   When `include_baseline_path` is `TRUE`, this includes a shared Baseline row
#'   for each selected response.
#' @export
format_alt_paths_long <- function(pretty_df, output_vars = NA, baseline_name_value = NA, 
                                  forecasts_start_date, output_start_date, output_end_date,
                                  include_baseline_path = FALSE) {
  if (!is.logical(include_baseline_path) || length(include_baseline_path) != 1 ||
      is.na(include_baseline_path)) {
    stop("`include_baseline_path` must be one logical value.", call. = FALSE)
  }

  # 1. Keep selected model-native alternatives and their shared display baseline.
  if (!all(is.na(output_vars))) {
    output_vars <- unlist(output_vars, use.names = FALSE)
    display_names <- pretty_df |>
      dplyr::filter(path_name != "Baseline", dynare_name %in% output_vars) |>
      dplyr::pull(display_name) |>
      unique()
    pretty_df <- pretty_df |>
      dplyr::filter(
        (path_name != "Baseline" & dynare_name %in% output_vars) |
          (path_name == "Baseline" & display_name %in% display_names)
      )
  }

  # Create the variable name column for the output table as 
  #   "Display Name (unit)" if unit exists, otherwise just "Display Name" 
  pretty_df <- pretty_df |>
    dplyr::mutate(
      variable_name = paste0(
        trimws(gsub("\\s*\\([^)]*\\)\\s*$", "", display_name)),
        " (",
        tolower(display_unit),
        ")"
      )
    )

  # 2. Join neutral baseline history to each alternative by display name.
  alt_df <- pretty_df |>
    dplyr::filter(date >= forecasts_start_date & date <= output_end_date) |>
    dplyr::filter(path_name != "Baseline")

  baseline_df <- pretty_df |>
    dplyr::filter(path_name == "Baseline", date >= output_start_date & date < forecasts_start_date)

  baseline_path_df <- pretty_df |>
    dplyr::filter(path_name == "Baseline", date >= output_start_date & date <= output_end_date)

  # 3) Pivot so identifiers are model_name, variable_name, path_name
  #    and dates become columns containing value
  alt_pivot_df <- alt_df |>
    dplyr::select(model_name, dynare_name, display_name, variable_name, path_name, date, value) |>
    tidyr::pivot_wider(
      id_cols = c(model_name, dynare_name, display_name, variable_name, path_name),
      names_from = date,
      values_from = value
    ) |>
    dplyr::arrange(model_name, variable_name, path_name)

  baseline_pivot_df <- baseline_df |>
    dplyr::select(display_name, variable_name, date, value) |>
    tidyr::pivot_wider(
      id_cols = c(display_name, variable_name),
      names_from = date,
      values_from = value
    ) |>
    dplyr::arrange(variable_name)

  # Join the baseline history to the alt path table
  table_df_joined <- alt_pivot_df |>
    dplyr::left_join(baseline_pivot_df, by = c("display_name", "variable_name")) |>
    dplyr::mutate(baseline_name = baseline_name_value) |>
    dplyr::select(-display_name)

  if (isTRUE(include_baseline_path)) {
    baseline_path_pivot_df <- baseline_path_df |>
      dplyr::transmute(
        model_name = NA_character_,
        dynare_name = NA_character_,
        display_name,
        variable_name,
        path_name = "Baseline",
        date,
        value
      ) |>
      tidyr::pivot_wider(
        id_cols = c(model_name, dynare_name, display_name, variable_name, path_name),
        names_from = date,
        values_from = value
      ) |>
      dplyr::mutate(baseline_name = baseline_name_value) |>
      dplyr::select(-display_name)
    table_df_joined <- dplyr::bind_rows(table_df_joined, baseline_path_pivot_df)
  }

  # Set the column order so that the baseline history appears first, 
  #   followed by the alt path values sorted by date.
  id_cols <- c("model_name", "dynare_name", "variable_name", "baseline_name", "path_name")

  date_cols <- names(table_df_joined) |>
    (\(x) x[grepl("^\\d{4}-\\d{2}-\\d{2}$", x)])()

  date_cols_sorted <- date_cols[order(as.Date(date_cols))]

  if (isTRUE(include_baseline_path)) {
    table_df_joined <- table_df_joined |>
      dplyr::mutate(path_order = dplyr::if_else(path_name == "Baseline", 0L, 1L)) |>
      dplyr::arrange(variable_name, path_order, model_name, path_name) |>
      dplyr::select(-path_order)
  }

  table_df_joined <- table_df_joined |>
    dplyr::select(
      dplyr::all_of(id_cols),
      dplyr::all_of(date_cols_sorted),
      dplyr::everything()
    ) |>
    dplyr::rename(
      "Model" = model_name,
      "Mnemonic" = dynare_name,
      "Variable Name" = variable_name,
      "Baseline Name" = baseline_name,
      "Path Name" = path_name
      ) |>
    dplyr::select(
      "Model",
      "Mnemonic",
      "Variable Name",
      "Baseline Name",
      "Path Name",
      dplyr::everything()
      )

  return(table_df_joined)
}