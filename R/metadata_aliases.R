# metadata_aliases.R 
# Helper functions to help transform between dynare_names (i.e. model variable codes)
# and display name variables. 


# Validate that display aliases can be resolved unambiguously.
#
# Repeated rows for the same code/alias pair are allowed. Most existing ezdyn
# code still assumes one effective metadata row per variable, so this validation
# only protects the alias lookup added for display names and shock descriptions.
ezdyn_validate_alias_metadata <- function(meta, code_col, alias_col, meta_name="metadata") {
    if (is.null(meta) || !(code_col %in% names(meta)) || !(alias_col %in% names(meta))) {
        return(invisible(TRUE))
    }
    alias_df <- meta |>
        dplyr::select(dplyr::all_of(c(code_col, alias_col))) |>
        dplyr::mutate(
            dplyr::across(dplyr::all_of(c(code_col, alias_col)), as.character)
        ) |>
        dplyr::filter(!is.na(.data[[code_col]]), .data[[code_col]] != "",
                      !is.na(.data[[alias_col]]), .data[[alias_col]] != "") |>
        dplyr::distinct()

    if (nrow(alias_df) == 0) {
        return(invisible(TRUE))
    }

    duplicate_aliases <- alias_df |>
        dplyr::group_by(.data[[alias_col]]) |>
        dplyr::summarise(n_codes = dplyr::n_distinct(.data[[code_col]]),
                         codes = paste(sort(unique(.data[[code_col]])), collapse=", "),
                         .groups = "drop") |>
        dplyr::filter(n_codes > 1)

    if (nrow(duplicate_aliases) > 0) {
        stop(sprintf(
            "Duplicate %s aliases found in `%s`: %s. Each `%s` must map to only one `%s`.",
            meta_name,
            alias_col,
            paste(sprintf("'%s' -> [%s]", duplicate_aliases[[alias_col]], duplicate_aliases$codes), collapse="; "),
            alias_col,
            code_col
        ), call. = FALSE)
    }

    codes <- unique(alias_df[[code_col]])
    collisions <- alias_df |>
        dplyr::filter(.data[[alias_col]] != .data[[code_col]], .data[[alias_col]] %in% codes)

    if (nrow(collisions) > 0) {
        stop(sprintf(
            "%s alias/code collisions found in `%s`: %s. A `%s` may equal its own `%s`, but not another row's `%s`.",
            meta_name,
            alias_col,
            paste(sprintf("'%s' for '%s'", collisions[[alias_col]], collisions[[code_col]]), collapse="; "),
            alias_col,
            code_col,
            code_col
        ), call. = FALSE)
    }

    invisible(TRUE)
}

ezdyn_validate_var_alias_metadata <- function(varmeta) {
    ezdyn_validate_alias_metadata(varmeta, "dynare_name", "display_name", "variable")
}

ezdyn_validate_shock_alias_metadata <- function(shock_meta) {
    ezdyn_validate_alias_metadata(shock_meta, "shock", "description", "shock")
}

ezdyn_model_codes <- function(M_, type=c("variable", "shock")) {
    type <- match.arg(type)
    if (!is.list(M_)) {
        return(character(0))
    }
    if (type == "variable") {
        codes <- unique(c(M_$endo.names, M_$endo.vars, M_$varmeta$dynare_name))
    } else {
        codes <- unique(c(M_$exo.names, M_$exo.vars, M_$shock_meta$shock))
    }
    codes <- as.character(codes)
    codes[!is.na(codes) & codes != ""]
}

ezdyn_resolve_aliases <- function(values, M_, meta, code_col, alias_col, valid_codes=NULL, input_name="input") {
    values <- as.character(values)
    if (length(values) == 0) {
        return(values)
    }
    if (is.null(valid_codes)) {
        valid_codes <- character(0)
    }
    valid_codes <- as.character(valid_codes)
    valid_codes <- valid_codes[!is.na(valid_codes) & valid_codes != ""]

    has_alias_meta <- !is.null(meta) && code_col %in% names(meta) && alias_col %in% names(meta)
    alias_df <- NULL
    if (has_alias_meta) {
        ezdyn_validate_alias_metadata(meta, code_col, alias_col, input_name)
        alias_df <- meta |>
            dplyr::select(dplyr::all_of(c(code_col, alias_col))) |>
            dplyr::mutate(
                dplyr::across(dplyr::all_of(c(code_col, alias_col)), as.character)
            ) |>
            dplyr::filter(!is.na(.data[[code_col]]), .data[[code_col]] != "",
                          !is.na(.data[[alias_col]]), .data[[alias_col]] != "") |>
            dplyr::distinct()
        valid_codes <- unique(c(valid_codes, alias_df[[code_col]]))
    }

    resolved <- purrr::map_chr(values, function(value) {
        if (has_alias_meta && value %in% alias_df[[alias_col]]) {
            return(alias_df[[code_col]][match(value, alias_df[[alias_col]])])
        }
        if (value %in% valid_codes || length(valid_codes) == 0) {
            return(value)
        }
        NA_character_
    })

    missing_values <- values[is.na(resolved)]
    if (length(missing_values) > 0) {
        stop(sprintf(
            "Unknown %s name(s): %s. Use a `%s`/`%s` from metadata or a known canonical code.",
            input_name,
            paste(sprintf("'%s'", missing_values), collapse=", "),
            alias_col,
            code_col
        ), call. = FALSE)
    }

    resolved
}

ezdyn_resolve_variable_names <- function(values, M_) {
    if (!is.list(M_)) {
        return(as.character(values))
    }
    ezdyn_resolve_aliases(values, M_, M_$varmeta, "dynare_name", "display_name",
                          valid_codes=ezdyn_model_codes(M_, "variable"),
                          input_name="variable")
}

ezdyn_resolve_shock_names <- function(values, M_) {
    if (!is.list(M_)) {
        return(as.character(values))
    }
    ezdyn_resolve_aliases(values, M_, M_$shock_meta, "shock", "description",
                          valid_codes=ezdyn_model_codes(M_, "shock"),
                          input_name="shock")
}

ezdyn_resolve_target_names <- function(target, M_) {
    if (is.null(names(target))) {
        stop("`target` must be a named list.", call. = FALSE)
    }
    names(target) <- ezdyn_resolve_variable_names(names(target), M_)
    target
}

ezdyn_resolve_shock_timing_names <- function(shock_timing, M_) {
    if (is.null(names(shock_timing))) {
        stop("`shock_timing` must be a named list.", call. = FALSE)
    }
    names(shock_timing) <- ezdyn_resolve_shock_names(names(shock_timing), M_)
    shock_timing
}

#' Return verified anticipated shock codes.
#'
#' Resolves one shock description or canonical code, then verifies that its
#' anticipated shock family is available for every requested period.
#'
#' @param M_ Model structure containing shock metadata and available shock codes.
#' @param shock_name One shock description or canonical code.
#' @param periods Positive, unique integer anticipation periods.
#'
#' @return Named character vector of anticipated shock codes by period.
#' @export
get_anticipated_shocks <- function(M_, shock_name, periods) {
    if (!is.character(shock_name) || length(shock_name) != 1 ||
        is.na(shock_name) || !nzchar(shock_name)) {
        stop("`shock_name` must be one non-empty shock description or code.", call. = FALSE)
    }
    if (!is.numeric(periods) || length(periods) == 0 || anyNA(periods) ||
        any(!is.finite(periods)) || any(periods < 1) || any(periods %% 1 != 0) ||
        anyDuplicated(periods)) {
        stop("`periods` must contain unique positive integers.", call. = FALSE)
    }

    periods <- sort(as.integer(periods))
    shock_code <- ezdyn_resolve_shock_names(shock_name, M_)
    anticipated_shocks <- paste0(shock_code, "_", periods)
    available_shocks <- ezdyn_model_codes(M_, "shock")
    missing_shocks <- setdiff(anticipated_shocks, available_shocks)
    if (length(missing_shocks) > 0) {
        stop(
            sprintf(
                "Shock `%s` does not provide anticipated code(s): %s.",
                shock_name,
                paste(sprintf("`%s`", missing_shocks), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    stats::setNames(anticipated_shocks, as.character(periods))
}

# Target scaling ----------------------------------------------------------

ezdyn_target_scale_factor <- function(varname, M_) {
    if (is.null(M_$varmeta) || !("dynare_name" %in% names(M_$varmeta)) || !("scale_factor" %in% names(M_$varmeta))) {
        return(NULL)
    }
    meta <- M_$varmeta |>
        dplyr::filter(dynare_name == varname)
    if (nrow(meta) == 0) {
        return(NULL)
    }
    if ("default" %in% names(meta) && any(meta$default %in% TRUE, na.rm=TRUE)) {
        meta <- meta |>
            dplyr::filter(default %in% TRUE)
    }
    scale_factors <- meta$scale_factor
    if (length(scale_factors) == 0) {
        return(NULL)
    }
    if (any(purrr::map_lgl(scale_factors, is.function))) {
        stop(sprintf(
            "Target scaling for '%s' uses a function-valued scale factor. Use `scale_targets = FALSE` and supply targets in model units.",
            varname
        ), call. = FALSE)
    }
    numeric_scales <- purrr::map(scale_factors, function(x) {
        if (is.null(x) || length(x) == 0 || all(is.na(x))) {
            return(NA_real_)
        }
        if (is.numeric(x)) {
            return(as.numeric(x[[1]]))
        }
        NA_real_
    }) |>
        unlist()
    numeric_scales <- unique(numeric_scales[!is.na(numeric_scales)])
    if (length(numeric_scales) == 0) {
        return(NULL)
    }
    if (length(numeric_scales) > 1) {
        stop(sprintf(
            "Multiple numeric scale factors found for target variable '%s'. Please make the metadata unambiguous or use `scale_targets = FALSE`.",
            varname
        ), call. = FALSE)
    }
    numeric_scales[[1]]
}

ezdyn_descale_targets <- function(target, M_, scale_targets=TRUE) {
    if (!isTRUE(scale_targets)) {
        return(target)
    }
    purrr::imap(target, function(values, varname) {
        scale_factor <- ezdyn_target_scale_factor(varname, M_)
        if (is.null(scale_factor)) {
            values
        } else {
            values / scale_factor
        }
    })
}

# Pretty IRF validation ---------------------------------------------------

ezdyn_validate_pretty_irf_units <- function(df, unit_symbol_column="unit_symbol_irf") {
    if (is.null(df) || !("display_name" %in% names(df))) {
        return(invisible(TRUE))
    }
    unit_cols <- character(0)
    if ("units" %in% names(df)) {
        unit_cols <- c(unit_cols, "units")
    } else if ("display_unit" %in% names(df)) {
        unit_cols <- c(unit_cols, "display_unit")
    }
    if (unit_symbol_column %in% names(df)) {
        unit_cols <- c(unit_cols, unit_symbol_column)
    }

    for (unit_col in unit_cols) {
        unit_data <- tibble::tibble(
            display_name = as.character(df$display_name),
            unit_value = as.character(df[[unit_col]])
        )
        conflicts <- unit_data |>
            dplyr::filter(!is.na(display_name), display_name != "",
                          !is.na(unit_value), unit_value != "") |>
            dplyr::distinct(display_name, unit_value) |>
            dplyr::group_by(display_name) |>
            dplyr::summarise(n_values = dplyr::n(),
                             values = paste(sort(unique(unit_value)), collapse=", "),
                             .groups = "drop") |>
            dplyr::filter(n_values > 1)
        if (nrow(conflicts) > 0) {
            stop(sprintf(
                "Multiple `%s` values found for the same `display_name`: %s. Each display name must have one `%s` value for pretty IRF plotting.",
                unit_col,
                paste(sprintf("'%s' -> [%s]", conflicts$display_name, conflicts$values), collapse="; "),
                unit_col
            ), call. = FALSE)
        }
    }

    invisible(TRUE)
}
