# Derived variables ----------------------------------------------------------
#
# Generic, metadata-driven handling for variables that are a transformation
# (or plain alias) of another raw model variable - e.g. year-ended inflation,
# the change in the cash rate, or a variable known internally under a
# different raw name. Declared once via `derived_from`/`derived_transform`
# columns in `varmeta` so baseline preparation (this file) and IRF derivation
# (get_IR_matrix.R) never hardcode a variable name or a transform's
# arithmetic. A derived variable's final value always composes two scale
# factors in a fixed order: the source's own `scale_factor` is applied to the
# raw source values first (already true wherever a caller supplies an
# already-scaled source column, e.g. baseline import), then any transform
# runs, then the derived row's own `scale_factor` is applied on top, via
# `ezdyn_own_scale()`. Both the source and the derived row may carry a real
# (non-identity) `scale_factor` at once - they are no longer required to be
# mutually exclusive.

# One-sided linear filter with fixed weights, padded with leading zeros so the
# output is the same length as the input (e.g. weights c(1, -1) is a first
# difference, rep(1, 4) is a trailing four-quarter sum).
ezdyn_linear_filter <- function(x, weights) {
  n <- length(weights)
  padded <- c(rep(0, n - 1), x)
  as.numeric(stats::filter(padded, weights, sides = 1))[n:(length(x) + n - 1)]
}

# Registered derived-variable transforms. Each is linear, so the same function
# applies to a baseline level column and to an IRF deviation series (a linear
# transform of a deviation stays consistent with adding it onto a baseline).
ezdyn_derived_transform_registry <- list(
  diff = function(x) ezdyn_linear_filter(x, c(1, -1)),
  diff2 = function(x) ezdyn_linear_filter(x, c(1, -2, 1)),
  year_ended_sum = function(x) ezdyn_linear_filter(x, rep(1, 4))
)

# Validate derived-variable metadata: unknown transforms, self-reference, and
# chaining (a `derived_from` that is itself a derived row) are rejected.
ezdyn_validate_derived_variable_metadata <- function(derived_names, sources, transforms) {
  known <- !is.na(transforms)
  unknown <- setdiff(unique(transforms[known]), names(ezdyn_derived_transform_registry))
  if (length(unknown) > 0) {
    stop(sprintf("Unknown `derived_transform`: %s.", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  self_reference <- sources == derived_names
  if (any(self_reference)) {
    stop(sprintf("`derived_from` cannot reference its own row: %s.", paste(derived_names[self_reference], collapse = ", ")), call. = FALSE)
  }
  chained <- sources %in% derived_names
  if (any(chained)) {
    stop(sprintf(
      "`derived_from` must reference a raw variable, not another derived row: %s.",
      paste(derived_names[chained], collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# Build a lookup of dynare_name -> list(source, transform) from varmeta rows
# with a non-blank `derived_from`. `transform` is `NA` for a pure alias.
ezdyn_derived_variable_lookup <- function(varmeta) {
  if (is.null(varmeta) || !("derived_from" %in% names(varmeta))) {
    return(list())
  }
  transforms <- if ("derived_transform" %in% names(varmeta)) as.character(varmeta$derived_transform) else NA_character_
  has_source <- !is.na(varmeta$derived_from) & varmeta$derived_from != ""
  derived_names <- varmeta$dynare_name[has_source]
  sources <- varmeta$derived_from[has_source]
  transforms <- transforms[has_source]
  ezdyn_validate_derived_variable_metadata(derived_names, sources, transforms)
  stats::setNames(Map(list, source = sources, transform = transforms), derived_names)
}

# Return a scaling function for one variable's own `scale_factor` in
# `varmeta` (numeric or function), defaulting to identity when `varmeta` is
# `NULL`, has no `scale_factor` column, has no matching row, or the matched
# value is `NA`. More defensive than the inline lookup in `rescale()`/
# `rescale_irf()` (rescale_irf.R), which errors on a missing `scale_factor`
# column.
ezdyn_own_scale <- function(varmeta, dynare_name) {
  identity_scale <- function(x) x
  if (is.null(varmeta) || !all(c("dynare_name", "scale_factor") %in% names(varmeta)) ||
      !(dynare_name %in% varmeta$dynare_name)) {
    return(identity_scale)
  }
  scale_ <- varmeta$scale_factor[varmeta$dynare_name == dynare_name][[1]]
  if (is.null(scale_) || (!is.function(scale_) && (length(scale_) != 1 || is.na(scale_)))) {
    return(identity_scale)
  }
  if (is.function(scale_)) scale_ else function(x) scale_ * x
}

#' Return response names available for a model, including derived variables.
#'
#' Extends a model's native/raw response-variable names with any
#' metadata-declared derived variable (`derived_from`/`derived_transform` in
#' `varmeta`) whose source variable is itself in `raw_names`. Used to make a
#' derived variable (e.g. the change in the cash rate, or year-ended
#' inflation) selectable anywhere a caller currently enumerates a model's raw
#' response variables.
#'
#' @param raw_names Character vector of a model's native/raw response names
#'   (e.g. endogenous variable names, or an IRF's response-variable names).
#' @param varmeta Variable metadata data frame (`M_$varmeta`), or `NULL`.
#'
#' @return Character vector: `raw_names` plus any resolvable derived names.
#' @export
ezdyn_available_response_names <- function(raw_names, varmeta) {
  raw_names <- unique(as.character(raw_names))
  raw_names <- raw_names[!is.na(raw_names) & raw_names != ""]
  lookup <- ezdyn_derived_variable_lookup(varmeta)
  has_available_source <- vapply(lookup, function(derived) derived$source %in% raw_names, logical(1))
  unique(c(raw_names, names(lookup)[has_available_source]))
}

# Append metadata-declared derived-variable rows to a long response data
# frame (columns `t`, `shock`, `dynare_name`, `value`), grouped by `shock` and
# ordered by `t`. A variable natively present in `irf_data` always wins - its
# row is left untouched and no derived row is added for it. The source values
# are assumed already display-scaled (as in `pretty_irf()`); the derived
# row's own `scale_factor` is then applied on top via `ezdyn_own_scale()`.
ezdyn_augment_derived_irf_rows <- function(irf_data, varmeta) {
  lookup <- ezdyn_derived_variable_lookup(varmeta)
  available <- unique(irf_data$dynare_name)
  lookup <- lookup[!(names(lookup) %in% available)]
  lookup <- Filter(function(derived) derived$source %in% available, lookup)
  if (length(lookup) == 0) {
    return(irf_data)
  }

  derived_rows <- purrr::imap_dfr(lookup, function(derived, variable) {
    irf_data |>
      dplyr::filter(dynare_name == derived$source) |>
      dplyr::group_by(shock) |>
      dplyr::arrange(t, .by_group = TRUE) |>
      dplyr::mutate(
        value = ezdyn_own_scale(varmeta, variable)(
          if (is.na(derived$transform)) value else ezdyn_derived_transform_registry[[derived$transform]](value)
        ),
        dynare_name = variable
      ) |>
      dplyr::ungroup()
  })
  dplyr::bind_rows(irf_data, derived_rows)
}

#' Add metadata-declared derived variables to a model-code baseline.
#'
#' Reads `derived_from`/`derived_transform` columns from `model$M_$varmeta`
#' and computes any declared derived variable not already present in
#' `baseline`, from the raw source column already present in `baseline`. A
#' missing source column yields an `NA` column rather than an error, matching
#' baseline semantics elsewhere. Applies the derived variable's own
#' `scale_factor` on top of its transform (the source column is assumed
#' already scaled by the caller, e.g. baseline import).
#'
#' @param baseline Model-code data frame with a `date` column.
#' @param model Full Dynare or custom-MOO pair.
#'
#' @return `baseline` with derived columns added.
#' @export
augment_derived_variables <- function(baseline, model) {
  lookup <- ezdyn_derived_variable_lookup(model$M_$varmeta)
  for (variable in names(lookup)) {
    if (variable %in% names(baseline)) next
    derived <- lookup[[variable]]
    value <- if (!(derived$source %in% names(baseline))) {
      NA_real_
    } else if (is.na(derived$transform)) {
      baseline[[derived$source]]
    } else {
      ezdyn_derived_transform_registry[[derived$transform]](baseline[[derived$source]])
    }
    baseline[[variable]] <- ezdyn_own_scale(model$M_$varmeta, variable)(value)
  }
  baseline
}
