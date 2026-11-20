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

# Every display name any of `models` defines (the names a display-name
# baseline column may use).
ezdyn_model_display_names <- function(models) {
  displays <- unlist(lapply(models, function(model) as.character(model$M_$varmeta$display_name)), use.names = FALSE)
  unique(displays[!is.na(displays) & displays != ""])
}

#' Translate a normalized baseline to canonical display-name columns
#'
#' Each input column is either a display name of one of the models, kept as
#' it is (it's already in display units), or - only when `source_model` is
#' given - one of `source_model`'s Dynare names, rescaled once with its
#' metadata and renamed to its display name. `source_model`'s own display and
#' Dynare names take precedence over the other models' display names. Other
#' columns are dropped with a warning.
#'
#' @param baseline A data frame with normalized `date` column.
#' @param source_model Optional full MOO pair whose Dynare names the input may
#'   use, or `NULL` for a baseline in display names only.
#' @param models Named list of full MOO pairs whose display names the input
#'   may use.
#'
#' @return A tibble with `date` and unique display-name columns.
#' @keywords internal
translate_baseline <- function(baseline, source_model = NULL, models = list()) {
  source_map <- if (!is.null(source_model)) ezdyn_baseline_source_map(source_model)
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

  # 1. Resolve each column: the source model's display or Dynare name first,
  # then any model's display name.
  source_display <- if (is.null(source_map)) rep(NA_integer_, length(input_names)) else match(input_names, source_map$display_name)
  source_dynare <- if (is.null(source_map)) rep(NA_integer_, length(input_names)) else match(input_names, source_map$dynare_name)
  model_display <- input_names %in% ezdyn_model_display_names(models)
  source_kind <- ifelse(
    !is.na(source_display), "source_display",
    ifelse(!is.na(source_dynare), "dynare", ifelse(model_display, "display", NA_character_))
  )
  translated_names <- ifelse(
    source_kind == "source_display", source_map$display_name[source_display],
    ifelse(source_kind == "dynare", source_map$display_name[source_dynare], input_names)
  )

  unknown_names <- input_names[is.na(source_kind)]
  if (length(unknown_names) > 0) {
    # Model codes in a display-name baseline need `source_model` to translate.
    codes <- unlist(lapply(models, function(model) as.character(model$M_$varmeta$dynare_name)), use.names = FALSE)
    hint <- if (is.null(source_model) && any(unknown_names %in% codes)) {
      " Some look like model variable codes: to translate a baseline written in one model's codes, supply that model as `source_model`."
    } else {
      ""
    }
    warning(
      sprintf("Ignoring unknown baseline column(s): %s.%s", paste(sprintf("`%s`", unknown_names), collapse = ", "), hint),
      call. = FALSE
    )
  }

  resolved <- !is.na(source_kind)
  if (!any(resolved)) {
    stop(
      if (is.null(source_model)) {
        "No baseline columns match a display name of `models`."
      } else {
        "No baseline columns match a display name of `models` or a variable of `source_model`."
      },
      call. = FALSE
    )
  }
  if (anyDuplicated(translated_names[resolved])) {
    duplicate_names <- unique(translated_names[resolved][duplicated(translated_names[resolved])])
    stop(
      sprintf("Multiple input columns translate to the same display name: %s.", paste(sprintf("`%s`", duplicate_names), collapse = ", ")),
      call. = FALSE
    )
  }

  # 2. Copy display-name columns as they are; rescale Dynare-name columns once.
  out <- list(date = baseline$date)
  for (position in which(resolved)) {
    input_name <- input_names[[position]]
    values <- baseline[[input_name]]
    if (!is.numeric(values)) {
      stop(sprintf("Baseline column `%s` must be numeric.", input_name), call. = FALSE)
    }
    out[[translated_names[[position]]]] <- if (source_kind[[position]] == "dynare") {
      ezdyn_rescale_baseline_values(values, source_model, source_map[source_dynare[[position]], , drop = FALSE])
    } else {
      values
    }
  }
  tibble::as_tibble(out)
}

# Exported Function ----

# Return a model's Dynare name -> display name map: each code's one default
# display name. Codes without exactly one are left out.
ezdyn_display_name_lookup <- function(varmeta) {
  codes <- as.character(varmeta$dynare_name)
  displays <- as.character(varmeta$display_name)
  keep <- !is.na(codes) & codes != "" & !is.na(displays) & displays != ""
  if ("default" %in% names(varmeta)) {
    is_default <- varmeta$default %in% TRUE
    has_default <- codes %in% codes[keep & is_default]
    keep <- keep & (is_default | !has_default)
  }
  codes <- codes[keep]
  displays <- displays[keep]
  single <- vapply(codes, function(code) length(unique(displays[codes == code])) == 1, logical(1))
  unique_codes <- unique(codes[single])
  stats::setNames(displays[match(unique_codes, codes)], unique_codes)
}

# Add metadata-declared derived display-name columns to a translated
# (display-name-keyed) baseline. Each model's derived variables are computed
# from columns already in the baseline, under display names, applying the
# derived variable's own `scale_factor` on top of its transform (as
# `augment_derived_variables()` does for model-code baselines). A column
# already in the baseline is never replaced, and a column that several models
# can derive comes from the first model in `models` that can. Repeats until
# nothing more can be derived, so one model can derive from another's derived
# column.
ezdyn_augment_derived_baseline <- function(baseline, models) {
  repeat {
    added <- FALSE
    for (model in models) {
      varmeta <- model$M_$varmeta
      lookup <- ezdyn_derived_variable_lookup(varmeta)
      name_for <- ezdyn_display_name_lookup(varmeta)
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
        baseline[[target_display_name]] <- ezdyn_own_scale(varmeta, variable)(value)
        added <- TRUE
      }
    }
    if (!added) return(baseline)
  }
}

#' Import a baseline forecast for one or more models
#'
#' @description
#' Reads a baseline - history and forecast, one column per variable - and
#' returns it in the form the dashboard and [get_alt_paths()] use: a `date`
#' column and one column per variable, named by display name and in display
#' units. It works in one of two modes:
#'
#' * **Display names** (`source_model = NULL`, the default): every column is a
#'   display name of one of `models`, already in display units (e.g.
#'   "Inflation", "Nominal Interest Rate"). Columns are kept as they are, so
#'   one baseline can hold variables from several models.
#' * **One model's codes** (`source_model` given): columns may also be
#'   `source_model`'s own variable codes, in its model units (e.g. `pinfobs`,
#'   `robs`). They are rescaled once, with its metadata's `scale_factor`, and
#'   renamed to their display names. Display-name columns of `source_model`
#'   or of `models` are accepted too.
#'
#' Columns that match nothing are dropped with a warning. The dates may be in
#' a standard date format or as `YYYYQq`; workbooks are read with `openxlsx2`,
#' falling back to `readxl` for legacy `.xls` files.
#'
#' The baseline also gains every derived variable (e.g. year-ended inflation,
#' or the change in the policy rate) that the models' metadata defines from
#' columns it has. A column you supply is never replaced, and a column that
#' several models could derive comes from the first model (`source_model`,
#' then `models` in order) that can.
#'
#' Finally, it checks that every model sharing a display name gives it the
#' same units.
#'
#' The baseline only has the variables you supply and those derived from
#' them: a variable with no baseline data can't be plotted as an alternative
#' path.
#'
#' @param input `.xls`/`.xlsx` path or data frame containing a date column and
#'   baseline values.
#' @param source_model Optional: a model (a full MOO pair) whose variable codes
#'   the input uses. `NULL` (the default) for a baseline in display names.
#' @param models Named list of models (full MOO pairs) that will use the
#'   baseline; their display names are the column names accepted.
#' @param date_col Optional date-column name or one-based position. When omitted,
#'   an exact case-insensitive `date` column is used; otherwise the first column
#'   is used.
#'
#' @return A tibble with a `Date` `date` column followed by unique display-name
#'   columns in display units.
#'
#' @examples
#' \dontrun{
#' models <- list("Smets Wouters (2007)" = sw, "Gali Unemployment (2010)" = gali)
#'
#' # A baseline in display names, with variables from both models
#' baseline <- import_baseline("baseline.xlsx", models = models)
#'
#' # A baseline in Smets-Wouters' own codes and units (e.g. `robs`, quarterly)
#' baseline <- import_baseline("sw_data.xlsx", source_model = sw, models = models)
#' }
#' @md
#' @export
import_baseline <- function(input, source_model = NULL, models, date_col = NULL) {
  # 1. Validate all model metadata before any input transformation.
  if (!is.null(source_model)) {
    if (is.list(source_model) && !all(c("M_", "oo_") %in% names(source_model)) &&
        length(source_model) > 0 && all(vapply(source_model, function(x) is.list(x) && all(c("M_", "oo_") %in% names(x)), logical(1)))) {
      stop("`source_model` must be one model; pass the list of models as `models = ...`.", call. = FALSE)
    }
    ezdyn_validate_baseline_moo(source_model, "source_model")
  }
  ezdyn_validate_baseline_models(models)
  source_models <- if (is.null(source_model)) list() else list(source_model)

  # 2. Normalize the raw table, translate it to display names, and add the
  # models' derived variables.
  baseline <- ezdyn_read_baseline_input(input) |>
    ezdyn_parse_baseline_dates(date_col = date_col) |>
    translate_baseline(source_model = source_model, models = models) |>
    ezdyn_augment_derived_baseline(models = c(source_models, unname(models)))

  # 3. Confirm that every shared display name has compatible units.
  source_model_name <- ".source_model"
  while (source_model_name %in% names(models)) {
    source_model_name <- paste0(source_model_name, "_")
  }
  validate_display_names(baseline, c(stats::setNames(source_models, if (length(source_models)) source_model_name), models))
  class(baseline) <- c("ezdyn_baseline", class(baseline))
  baseline
}