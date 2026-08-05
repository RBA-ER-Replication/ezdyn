# display_names.R 
# Hamish Sullivan, 2025

# TODO: In a future version, set up so that there isn't only one
# dynare name corresponding to each display name by default.

#' Get the display name for a Dynare variable
#'
#' Returns the display name for a single Dynare variable. If multiple
#' transformations of a variable are defined in the metadata, this returns the default.
#'
#' @param dynare_name_ Character scalar. Dynare variable name.
#' @param M_ Dynare model object containing `varmeta`.
#'
#' @return Character scalar display name.
#' @export
#' @examples
#' \dontrun{
#' display_name("infl_obs", M_)
#' }
display_name <- function(dynare_name_, M_) {
  metadata <- M_$varmeta |>
    dplyr::filter(dynare_name == dynare_name_, default == TRUE)
  detected_names <- unique(metadata$display_name)
  if (length(detected_names) == 0) {
    display_name_ <- dynare_name_
  } else if (length(detected_names) == 1) {
    display_name_ <- detected_names
  } else if (length(detected_names) > 1) {
    stop("There should only be one display_name defined per dynare_name in the metadata.")
  }
  display_name_
}

#' Get display names for multiple Dynare variables
#'
#' @param dynare_names Character vector of Dynare variable names.
#' @param M_ Dynare model object containing `varmeta`.
#' @param df Logical. If TRUE, return a data frame with columns `Variable` and
#'   `display_name`.
#'
#' @return Named character vector or tibble.
#' @export
#' @examples
#' \dontrun{
#' display_names(c("infl_obs", "r_obs"), M_)
#' }
display_names <- function(dynare_names, M_, df=F) {
  out <- purrr::map(dynare_names, purrr::partial(display_name, M_=M_)) |>
    unlist()
  names(out) <- dynare_names

  if (df) {
    out <- tibble::enframe(out, name = "Variable", value="display_name")
  }

  out
}

#' Return all display names for endogenous variables
#'
#' @param M_ Dynare model object containing `endo.names`.
#'
#' @return Named character vector mapping display names to Dynare names.
#' @export
all_display_names <- function(M_) {
  y <- M_$endo.names |>
    display_names(M_)
  setNames(names(y), y)
}

#' Get only those display names which are defined in the variable metadata.
#' TEMPORARY 'quick and dirty' function for until this implementation is reworked.
#' @param alt_paths_only whether to only pull variables with alt paths == T in the variable metadata.
#' @param hsd_only whether to only pull variables with hsd == T in the variable metadata.
#' @export
defined_display_names <- function(M_, defined_only=T, alt_paths_only=F, hsd_only=F) {

  vars <- M_$varmeta
  if (alt_paths_only) {
    vars <- dplyr::filter(vars, alt_paths == T)
  }

  if (hsd_only) {
    vars <- dplyr::filter(vars, hsd == T)
  }

  vars |>
    dplyr::pull("display_name")
}

#' Get display-name lists for the Shiny selector
#'
#' Returns a list of named vectors for Shiny inputs, split by core and other
#' variables and optionally filtered to alternative-path variables.
#'
#' @param M_ Dynare model object containing `varmeta` and `endo.names`.
#' @param core_only Logical. If TRUE, return only core variables.
#' @param alt_paths_only Logical. If TRUE, only return variables with
#'   `alt_paths == TRUE`.
#' @param extra_names List of extra display names to append.
#'
#' @return Named list of character vectors.
#' @export
#' @param core_only show only the core variables
#' @param alt_paths only return variables with alt_paths=TRUE in the variable metadata
#' @param extra_names Any extra names to force-add to the list of displaynames.
#' This is used in the dashboard to force add the Unemployment rate as an extra
#' variable (since it isn't in DINGO).
get_shiny_displaynames <- function(M_, core_only=F, alt_paths_only=F, extra_names=list()) {
  if (alt_paths_only) {
    M_temp <- M_
    M_temp$varmeta <- dplyr::filter(M_temp$varmeta, alt_paths == TRUE)
    all_names <- all_display_names(M_temp)
  } else {
    all_names <- all_display_names(M_)
  }

  # Append any extra names to the list
  all_names <- append(all_names, extra_names)

  if (core_only) {
    core_only <- all_names[names(all_names) != all_names]
    # TODO: Remove hardcoding of which variables are in DINGO and MARTIN
    out <- list(`DINGO and MARTIN` = core_only[core_only %in% VARS_DINGO_AND_MARTIN],
                `DINGO only` = core_only[!(core_only %in% VARS_DINGO_AND_MARTIN)])
  } else {
    out <- list(`Core Variables` = all_names[names(all_names) != all_names],
                `Other Variables` = all_names[names(all_names) == all_names])
  }
  out
}

#' Get the display unit or unit symbol for a Dynare variable
#'
#' @param dynare_name_ Character scalar. Dynare variable name.
#' @param M_ Dynare model object containing `varmeta`.
#' @param unit_symbol Logical. If TRUE, return a unit symbol instead of the
#'   display unit.
#' @param symbol_type Character. Either `"irf"` or `"baseline"` to select the
#'   unit symbol column.
#'
#' @return Character scalar display unit or unit symbol.
#' @export
#' @examples
#' \dontrun{
#' display_unit("infl_obs", M_)
#' display_unit("infl_obs", M_, unit_symbol = TRUE, symbol_type = "baseline")
#' }
display_unit <- function(dynare_name_, M_, unit_symbol=FALSE, symbol_type="irf") {
  metadata <- M_$varmeta |>
    dplyr::filter(dynare_name == dynare_name_)
  if (unit_symbol) {
    symbol_col <- if (symbol_type == "irf") "unit_symbol_irf" else "unit_symbol_baseline"
    detected_symbols <- unique(metadata[[symbol_col]])
    detected_symbols <- detected_symbols[!is.na(detected_symbols)]
    if (length(detected_symbols) == 0) {
      display_unit_ <- ""
    } else if (length(detected_symbols) == 1) {
      display_unit_ <- detected_symbols
    } else {
      stop("There should only be one unit symbol defined per dynare_name in the metadata.")
    }
  } else {
    detected_units <- unique(metadata$units)
    if (length(detected_units) == 0) {
      display_unit_ <- ""
    } else if (length(detected_units) == 1) {
      display_unit_ <- detected_units
    } else if (length(detected_units) > 1) {
      stop("There should only be one 'unit' defined per dynare_name in the metadata.")
    }
  }
  display_unit_
}


#' Get display units or unit symbols for multiple Dynare variables
#'
#' @param dynare_names Character vector of Dynare variable names.
#' @param M_ Dynare model object containing `varmeta`.
#' @param df Logical. If TRUE, return a data frame with the unit column.
#' @param unit_symbol Logical. If TRUE, return unit symbols instead of display
#'   units.
#' @param symbol_type Character. Either `"irf"` or `"baseline"`.
#'
#' @return Named character vector or tibble.
#' @export
#' @examples
#' \dontrun{
#' display_units(c("infl_obs", "r_obs"), M_)
#' display_units(c("infl_obs", "r_obs"), M_, unit_symbol = TRUE, symbol_type = "baseline")
#' }
display_units <- function(dynare_names, M_, df=F, unit_symbol=FALSE, symbol_type="irf") {
  out <- purrr::map(dynare_names, purrr::partial(display_unit, M_=M_, unit_symbol=unit_symbol, symbol_type=symbol_type)) |>
    unlist()
  names(out) <- dynare_names
  if (df) {
    value_name <- if (unit_symbol) {
      if (symbol_type == "irf") "unit_symbol_irf" else "unit_symbol_baseline"
    } else {
      "display_unit"
    }
    out <- tibble::enframe(out, name = "Variable", value=value_name)
  }
  out
}

#' Get display names, units, and unit symbols
#'
#' @param dynare_names Character vector of Dynare variable names.
#' @param M_ Dynare model object containing `varmeta`.
#' @param unit_symbol_irf Whether to include the unit symbols (IRF) in the dataframe.
#' @param unit_symbol_baseline Whether to include the unit symbol (baseline) in the dataframe.
#' @return Tibble with columns for display name, unit, and unit symbols.
#' @export
display_names_and_units <- function(dynare_names, M_, unit_symbol_irf=F, unit_symbol_baseline=F) {
  out <- display_units(dynare_names, M_, df=T) |>
    dplyr::full_join(display_names(dynare_names, M_, df=T), by="Variable")
  if (unit_symbol_irf) {
    out <- dplyr::full_join(out, display_units(dynare_names, M_, df=T, unit_symbol=TRUE, symbol_type="irf"), by="Variable")
  }
  if (unit_symbol_baseline) {
    out <- dplyr::full_join(out, display_units(dynare_names, M_, df=T, unit_symbol=TRUE, symbol_type="baseline"), by="Variable")
  }
  out
}

