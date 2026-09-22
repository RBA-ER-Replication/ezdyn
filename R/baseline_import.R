# baseline_import.R - Import and translate baseline data for one or more models.
# File Structure: 
    # 1. Validation Functions (validate inputs, normalise dates)
    # 2. Helper functions (read, translate into display units and rescale)
    # 3. The main `import_baseline` function which is exported for use by users!
#  Validation Helper Functions ----
# Validate one full MOO pair used by the baseline API.
ezdyn_validate_baseline_moo <- function(model, label) {
  if (!is.list(model) || !all(c("M_", "oo_") %in% names(model))) {
    stop(sprintf("`%s` must be a full MOO pair with `M_` and `oo_` elements.", label), call. = FALSE)
  }

  model_type <- function(x) {
    if (inherits(x, "dynare")) {
      "dynare"
    } else if (inherits(x, "custom_moo")) {
      "custom_moo"
    } else {
      NA_character_
    }
  }
  types <- c(M_ = model_type(model$M_), oo_ = model_type(model$oo_))
  if (anyNA(types) || types[["M_"]] != types[["oo_"]]) {
    stop(
      sprintf("`%s` must contain matching `dynare` or `custom_moo` `M_` and `oo_` objects.", label),
      call. = FALSE
    )
  }

  varmeta <- model$M_$varmeta
  required_columns <- c("dynare_name", "display_name", "units")
  if (!is.data.frame(varmeta) || !all(required_columns %in% names(varmeta))) {
    stop(
      sprintf("`%s$M_$varmeta` must contain %s.", label, paste(sprintf("`%s`", required_columns), collapse = ", ")),
      call. = FALSE
    )
  }
  ezdyn_validate_var_alias_metadata(varmeta)
  invisible(model)
}

# Validate the named target model list used by import_baseline().
ezdyn_validate_baseline_models <- function(models) {
  if (!is.list(models) || length(models) == 0) {
    stop("`models` must be a non-empty named list of full MOO pairs.", call. = FALSE)
  }
  model_names <- names(models)
  if (is.null(model_names) || anyNA(model_names) || any(model_names == "") || anyDuplicated(model_names)) {
    stop("`models` must have unique, non-empty names.", call. = FALSE)
  }

  for (model_name in model_names) {
    ezdyn_validate_baseline_moo(models[[model_name]], sprintf("models[['%s']]", model_name))
  }
  invisible(models)
}

#' Validate baseline display units across target models.
#'
#' @param baseline Canonical display-name baseline returned by [translate_baseline()].
#' @param models Named list of full target MOO pairs.
#'
#' @return `TRUE`, invisibly, or an error describing conflicting units.
#' @keywords internal
validate_display_names <- function(baseline, models) {
  ezdyn_validate_baseline_models(models)
  if (!is.data.frame(baseline) || !("date" %in% names(baseline))) {
    stop("`baseline` must contain a `date` column.", call. = FALSE)
  }
  display_names <- setdiff(names(baseline), "date")
  if (length(display_names) == 0) {
    return(invisible(TRUE))
  }

  unit_records <- list()
  for (model_name in names(models)) {
    varmeta <- models[[model_name]]$M_$varmeta
    for (display_name in intersect(display_names, as.character(varmeta$display_name))) {
      unit_values <- unique(as.character(varmeta$units[as.character(varmeta$display_name) == display_name]))
      unit_values <- unit_values[!is.na(unit_values) & unit_values != ""]
      if (length(unit_values) != 1) {
        stop(
          sprintf("Model `%s` must provide one non-empty display unit for `%s`.", model_name, display_name),
          call. = FALSE
        )
      }
      unit_records[[length(unit_records) + 1L]] <- data.frame(
        display_name = display_name,
        model_name = model_name,
        display_unit = unit_values[[1]],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(unit_records) == 0) {
    return(invisible(TRUE))
  }

  unit_records <- do.call(rbind, unit_records)
  conflicts <- lapply(unique(unit_records$display_name), function(display_name) {
    rows <- unit_records[unit_records$display_name == display_name, , drop = FALSE]
    if (length(unique(rows$display_unit)) <= 1) {
      return(NULL)
    }
    sprintf(
      "`%s` -> %s",
      display_name,
      paste(sprintf("%s: %s", rows$model_name, rows$display_unit), collapse = "; ")
    )
  })
  conflicts <- Filter(Negate(is.null), conflicts)
  if (length(conflicts) > 0) {
    stop(
      sprintf("Conflicting display units across models: %s.", paste(conflicts, collapse = "; ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Locate and normalize the date column in an un-translated baseline table.
# Also validates the dates.
ezdyn_parse_baseline_dates <- function(baseline, date_col = NULL) {
  if (!is.data.frame(baseline) || ncol(baseline) == 0) {
    stop("`baseline` must contain a date column and at least one baseline column.", call. = FALSE)
  }
  if (anyDuplicated(names(baseline))) {
    stop("Baseline input column names must be unique.", call. = FALSE)
  }

  date_index <- if (!is.null(date_col)) {
    if (is.numeric(date_col) && length(date_col) == 1 && !is.na(date_col) && date_col %% 1 == 0 && date_col >= 1 && date_col <= ncol(baseline)) {
      as.integer(date_col)
    } else if (is.character(date_col) && length(date_col) == 1 && !is.na(date_col) && date_col %in% names(baseline)) {
      match(date_col, names(baseline))
    } else {
      stop("`date_col` must identify exactly one existing baseline column by name or position.", call. = FALSE)
    }
  } else {
    date_candidates <- which(tolower(names(baseline)) == "date")
    if (length(date_candidates) > 1) {
      stop("Baseline input has multiple candidate date columns; supply `date_col` explicitly.", call. = FALSE)
    }
    if (length(date_candidates) == 1) date_candidates else 1L
  }

  if (any(names(baseline)[-date_index] == "date")) {
    stop("Baseline input would contain multiple `date` columns after normalization.", call. = FALSE)
  }
  date_values <- baseline[[date_index]]
  dates <- if (inherits(date_values, "Date") || inherits(date_values, "POSIXt")) {
    as.Date(date_values)
  } else {
    date_strings <- as.character(date_values)
    if (anyNA(date_strings) || any(trimws(date_strings) == "")) {
      stop("Baseline date values cannot be missing or blank.", call. = FALSE)
    }
    if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", date_strings))) {
      as.Date(date_strings)
    } else if (all(grepl("^\\d{4}[Qq][1-4]$", date_strings))) {
      quarter_strings <- sub("[qQ]", "Q", date_strings)
      zoo::as.yearqtr(quarter_strings, format = "%YQ%q") |>
        zoo::as.Date() |>
        end_of_quarter() |>
        start_of_month()
    } else {
      stop("Baseline dates must be Date/POSIX values, ISO `YYYY-MM-DD` strings, or `YYYYQq` strings.", call. = FALSE)
    }
  }

  if (anyNA(dates)) {
    stop("Baseline dates could not be parsed.", call. = FALSE)
  }
  if (anyDuplicated(dates)) {
    stop("Baseline dates must be unique.", call. = FALSE)
  }
  if (is.unsorted(dates, strictly = TRUE)) {
    stop("Baseline dates must be strictly increasing.", call. = FALSE)
  }

  baseline[[date_index]] <- dates
  names(baseline)[[date_index]] <- "date"
  baseline <- baseline[c(date_index, setdiff(seq_len(ncol(baseline)), date_index))]
  tibble::as_tibble(baseline[order(baseline$date), , drop = FALSE])
}

# Helper Functions ----

# Return the one default metadata row for each source-model variable.
ezdyn_baseline_source_map <- function(source_model) {
  ezdyn_validate_baseline_moo(source_model, "source_model")
  varmeta <- source_model$M_$varmeta
  varmeta$.ezdyn_row_id <- seq_len(nrow(varmeta))
  varmeta$dynare_name <- as.character(varmeta$dynare_name)
  varmeta$display_name <- as.character(varmeta$display_name)
  varmeta <- varmeta[
    !is.na(varmeta$dynare_name) & varmeta$dynare_name != "" &
      !is.na(varmeta$display_name) & varmeta$display_name != "",
    ,
    drop = FALSE
  ]
  if (nrow(varmeta) == 0) {
    stop("`source_model$M_$varmeta` has no usable variable metadata.", call. = FALSE)
  }

  source_codes <- unique(varmeta$dynare_name)
  source_rows <- lapply(source_codes, function(source_code) {
    rows <- varmeta[varmeta$dynare_name == source_code, , drop = FALSE]
    if ("default" %in% names(rows) && any(rows$default %in% TRUE, na.rm = TRUE)) {
      rows <- rows[rows$default %in% TRUE, , drop = FALSE]
    }

    display_names <- unique(rows$display_name)
    if (length(display_names) != 1) {
      stop(
        sprintf("Source metadata for `%s` must identify one default display name.", source_code),
        call. = FALSE
      )
    }
    if (nrow(rows) > 1) {
      scale_formulas <- if ("scale_formula" %in% names(rows)) unique(as.character(rows$scale_formula)) else character(0)
      if (length(scale_formulas) > 1) {
        stop(
          sprintf("Source metadata for `%s` has ambiguous default scaling.", source_code),
          call. = FALSE
        )
      }
    }

    data.frame(
      dynare_name = source_code,
      display_name = display_names[[1]],
      metadata_row = rows$.ezdyn_row_id[[1]],
      stringsAsFactors = FALSE
    )
  })
  source_map <- do.call(rbind, source_rows)

  if (anyDuplicated(source_map$display_name)) {
    duplicate_names <- unique(source_map$display_name[duplicated(source_map$display_name)])
    stop(
      sprintf(
        "Source metadata maps multiple Dynare variables to the same display name: %s.",
        paste(sprintf("`%s`", duplicate_names), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  source_map
}

# Rescale a source-model Dynare column exactly once before it becomes a display series.
ezdyn_rescale_baseline_values <- function(values, source_model, source_map_row) {
  varmeta <- source_model$M_$varmeta
  if (!("scale_factor" %in% names(varmeta))) {
    stop(
      sprintf("Source metadata for `%s` has no `scale_factor` for baseline translation.", source_map_row$dynare_name),
      call. = FALSE
    )
  }

  scale_factor <- varmeta$scale_factor[[source_map_row$metadata_row]]
  missing_scale_factor <- is.null(scale_factor) || length(scale_factor) == 0 ||
    (!is.function(scale_factor) && all(is.na(scale_factor)))
  if (missing_scale_factor) {
    stop(
      sprintf("Source metadata for `%s` has no usable scale factor for baseline translation.", source_map_row$dynare_name),
      call. = FALSE
    )
  }

  scaled_values <- if (is.function(scale_factor)) {
    scale_factor(values)
  } else if (is.numeric(scale_factor) && length(scale_factor) == 1) {
    values * scale_factor
  } else {
    stop(
      sprintf("Source metadata for `%s` has an unsupported scale factor for baseline translation.", source_map_row$dynare_name),
      call. = FALSE
    )
  }
  if (!is.numeric(scaled_values) || length(scaled_values) != length(values)) {
    stop(
      sprintf("Rescaling `%s` must return a numeric vector of the original length.", source_map_row$dynare_name),
      call. = FALSE
    )
  }
  scaled_values
}

# Read a workbook with the modern reader first, then the legacy-compatible reader.
ezdyn_read_baseline_workbook <- function(input) {
  readers <- list(
    openxlsx2 = function(path) openxlsx2::read_xlsx(
      path,
      check_names = FALSE,
      skip_empty_rows = TRUE,
      skip_empty_cols = TRUE
    ),
    readxl = function(path) readxl::read_excel(path)
  )
  errors <- character()

  for (reader_name in names(readers)) {
    workbook <- tryCatch(
      readers[[reader_name]](input),
      error = function(condition) {
        errors <<- c(errors, sprintf("%s: %s", reader_name, condition$message))
        NULL
      }
    )
    if (!is.null(workbook)) {
      return(as.data.frame(workbook, check.names = FALSE))
    }
  }

  stop(
    sprintf("Could not read baseline workbook `%s`: %s.", input, paste(errors, collapse = "; ")),
    call. = FALSE
  )
}

# Read a supported raw baseline input without mutating caller-provided data.
ezdyn_read_baseline_input <- function(input) {
  if (is.data.frame(input)) {
    return(as.data.frame(input, check.names = FALSE))
  }
  if (!is.character(input) || length(input) != 1 || is.na(input) || input == "") {
    stop("`input` must be a data frame or one `.xls`/`.xlsx` file path.", call. = FALSE)
  }
  if (!file.exists(input)) {
    stop(sprintf("Baseline file does not exist: `%s`.", input), call. = FALSE)
  }
  if (!(tolower(tools::file_ext(input)) %in% c("xls", "xlsx"))) {
    stop("Baseline files must use a `.xls` or `.xlsx` extension.", call. = FALSE)
  }
  ezdyn_read_baseline_workbook(input)
}

#' Translate a normalized baseline to canonical display-name columns.
#'
#' Matches exact source-model display names before exact Dynare names. Display-name
#' columns are already in general units and are not rescaled; Dynare-name columns
#' are rescaled once with the source model metadata.
#'
#' @param baseline A data frame with normalized `date` column.
#' @param source_model Full source MOO pair used to interpret input columns.
#'
#' @return A tibble with `date` and unique display-name columns.
#' @keywords internal
translate_baseline <- function(baseline, source_model) {
  source_map <- ezdyn_baseline_source_map(source_model)
  if (!is.data.frame(baseline) || !("date" %in% names(baseline)) || !inherits(baseline$date, "Date")) {
    stop("`baseline` must be a normalized data frame with a Date `date` column.", call. = FALSE)
  }
  if (anyDuplicated(names(baseline))) {
    stop("Baseline input column names must be unique.", call. = FALSE)
  }

  input_names <- setdiff(names(baseline), "date")
  if (length(input_names) == 0) {
    stop("Baseline input has no variable columns.", call. = FALSE)
  }

  display_matches <- match(input_names, source_map$display_name)
  dynare_matches <- match(input_names, source_map$dynare_name)
  source_kind <- ifelse(!is.na(display_matches), "display", ifelse(!is.na(dynare_matches), "dynare", NA_character_))
  map_index <- ifelse(!is.na(display_matches), display_matches, dynare_matches)
  unknown_names <- input_names[is.na(source_kind)]
  if (length(unknown_names) > 0) {
    warning(
      sprintf("Ignoring unknown baseline column(s): %s.", paste(sprintf("`%s`", unknown_names), collapse = ", ")),
      call. = FALSE
    )
  }

  translated_names <- source_map$display_name[map_index[!is.na(map_index)]]
  if (length(translated_names) == 0) {
    stop("No baseline columns could be resolved from `source_model` metadata.", call. = FALSE)
  }
  if (anyDuplicated(translated_names)) {
    duplicate_names <- unique(translated_names[duplicated(translated_names)])
    stop(
      sprintf("Multiple input columns translate to the same display name: %s.", paste(sprintf("`%s`", duplicate_names), collapse = ", ")),
      call. = FALSE
    )
  }

  out <- list(date = baseline$date)
  resolved_positions <- which(!is.na(map_index))
  for (position in resolved_positions) {
    input_name <- input_names[[position]]
    values <- baseline[[input_name]]
    if (!is.numeric(values)) {
      stop(sprintf("Baseline column `%s` must be numeric.", input_name), call. = FALSE)
    }

    map_row <- source_map[map_index[[position]], , drop = FALSE]
    display_name <- map_row$display_name[[1]]
    out[[display_name]] <- if (source_kind[[position]] == "display") {
      values
    } else {
      ezdyn_rescale_baseline_values(values, source_model, map_row)
    }
  }
  tibble::as_tibble(out)
}

# Exported Function ----

# Add metadata-declared derived display-name columns to a translated
# (display-name-keyed) baseline, computed from the source model's own raw
# source column already present under its display name. Mirrors
# `augment_derived_variables()` (dynare-name-keyed, used for model-code
# baselines), including applying the derived variable's own `scale_factor` on
# top of its transform, but resolves both the derived variable and its source
# to their default display name first, since a translated baseline is
# display-name-keyed.
ezdyn_augment_derived_baseline <- function(baseline, source_model) {
  source_map <- ezdyn_baseline_source_map(source_model)
  lookup <- ezdyn_derived_variable_lookup(source_model$M_$varmeta)
  name_for <- stats::setNames(source_map$display_name, source_map$dynare_name)

  for (variable in names(lookup)) {
    target_display_name <- unname(name_for[variable])
    if (is.na(target_display_name) || target_display_name %in% names(baseline)) next
    derived <- lookup[[variable]]
    source_display_name <- unname(name_for[derived$source])
    if (is.na(source_display_name) || !(source_display_name %in% names(baseline))) next
    value <- if (is.na(derived$transform)) {
      baseline[[source_display_name]]
    } else {
      ezdyn_derived_transform_registry[[derived$transform]](baseline[[source_display_name]])
    }
    baseline[[target_display_name]] <- ezdyn_own_scale(source_model$M_$varmeta, variable)(value)
  }
  baseline
}

#' Import a canonical baseline for one or more models.
#'
#' Reads an `.xls`/`.xlsx` workbook or in-memory data frame, normalizes a
#' standard or `YYYYQq` date column, translates source Dynare names to display
#' names, and validates units shared by the target models. The workbook reader
#' uses `openxlsx2` first and falls back to `readxl` for legacy `.xls`
#' compatibility. Exact display-name input columns are assumed to already be in
#' general units and are not rescaled. Exact Dynare-name input columns are
#' rescaled once using the source model metadata.
#'
#' @param input `.xls`/`.xlsx` path or data frame containing a date column and
#'   baseline values.
#' @param source_model Full MOO pair whose metadata defines source columns.
#' @param models Named list of full MOO pairs that will use the baseline.
#' @param date_col Optional date-column name or one-based position. When omitted,
#'   an exact case-insensitive `date` column is used; otherwise the first column
#'   is used.
#'
#' @return A tibble with a `Date` `date` column followed by unique canonical
#'   display-name columns in general units.
#' @export
import_baseline <- function(input, source_model, models, date_col = NULL) {
  # 1. Validate all model metadata before any input transformation.
  ezdyn_validate_baseline_moo(source_model, "source_model")
  ezdyn_validate_baseline_models(models)

  # 2. Normalize the raw table and translate it using the specified source model.
  baseline <- ezdyn_read_baseline_input(input) |>
    ezdyn_parse_baseline_dates(date_col = date_col) |>
    translate_baseline(source_model = source_model) |>
    ezdyn_augment_derived_baseline(source_model = source_model)

  # 3. Confirm that every shared display name has compatible target-model units.
  source_model_name <- ".source_model"
  while (source_model_name %in% names(models)) {
    source_model_name <- paste0(source_model_name, "_")
  }
  validate_display_names(baseline, c(stats::setNames(list(source_model), source_model_name), models))
  class(baseline) <- c("ezdyn_baseline", class(baseline))
  baseline
}