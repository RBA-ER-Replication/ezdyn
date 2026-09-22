# IRF scenario and shared helpers -----------------------------------------
#
# This file owns shared selectors, curated Scenario Mode support, and common
# plot text. Custom IRF selection and calculation logic lives separately in
# `dash_irf_custom_helpers.R`.

#' Return valid named Shiny choices.
#'
#' @param values Character values.
#'
#' @return A named character vector.
irf_choices <- function(values) {
  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  stats::setNames(values, values)
}

#' Preserve a selection when it remains compatible with updated choices.
#'
#' @param current Current selected values.
#' @param choices Updated choices.
#' @param fallback Preferred fallback values.
#'
#' @return Character vector.
irf_preserve_selection <- function(current, choices, fallback = character()) {
  selected <- intersect(current, choices)
  if (length(selected) > 0) {
    return(selected)
  }

  selected <- intersect(fallback, choices)
  if (length(selected) > 0) selected else utils::head(choices, 1)
}

#' Return the response names provided by a model pair.
#'
#' Includes native/raw response variables plus any metadata-declared derived
#' variable (e.g. year-ended inflation, the change in the cash rate) whose
#' source variable is itself native to the model.
#'
#' @param model A Dynare or custom-MOO pair.
#'
#' @return Character vector of response codes.
irf_model_responses <- function(model) {
  raw_names <- unique(c(
    model$M_$endo.names,
    model$M_$endo.vars,
    model$oo_$irf$resp_var
  )) |>
    as.character() |>
    stats::na.omit()
  ezdyn_available_response_names(raw_names, model$M_$varmeta)
}

#' Return metadata-backed display names provided by a model pair.
#'
#' @param model A Dynare or custom-MOO pair.
#'
#' @return Character vector of display names.
irf_model_display_names <- function(model) {
  metadata <- model$M_$varmeta
  if (is.null(metadata) || !all(c("dynare_name", "display_name") %in% names(metadata))) {
    return(character())
  }

  response_names <- irf_model_responses(model)
  rows <- metadata$dynare_name %in% response_names
  irf_choices(metadata$display_name[rows]) |>
    unname()
}

#' Return metadata capabilities common to all selected models.
#'
#' Preserves the metadata row order of the first selected model rather than
#' sorting alphabetically, so selectors match the source `variable_metadata`
#' workbook.
#'
#' @param models Named list of selected model pairs.
#' @param extractor Function returning character capabilities for one model.
#'
#' @return A named character vector.
irf_common_choices <- function(models, extractor) {
  if (length(models) == 0) {
    return(character())
  }

  capabilities <- lapply(models, extractor)
  common <- Reduce(intersect, capabilities)
  irf_choices(common)
}

#' Return response codes a model provides without a metadata display name.
#'
#' Complements [irf_model_display_names()]: a response is "unnamed" when it has
#' no metadata row, or a blank `display_name`. Its raw Dynare code is used as
#' its display value instead, matching the fallback in `ezdyn::display_name()`.
#'
#' @param model A Dynare or custom-MOO pair.
#'
#' @return Character vector of raw response codes.
irf_model_unnamed_responses <- function(model) {
  response_names <- irf_model_responses(model)
  metadata <- model$M_$varmeta
  named_codes <- if (is.null(metadata) || !all(c("dynare_name", "display_name") %in% names(metadata))) {
    character()
  } else {
    metadata$dynare_name[!is.na(metadata$display_name) & nzchar(metadata$display_name)]
  }
  irf_choices(setdiff(response_names, named_codes))
}

#' Return response-variable choices for selected models, grouped by whether
#' each variable has a metadata display name.
#'
#' Named variables (metadata `display_name`) and raw response codes (no
#' metadata match) are grouped into separate optgroups, mirroring the style
#' used by [irf_shock_selector()]. Both groups are restricted to responses
#' common to every selected model.
#'
#' @param models Named list of selected model pairs.
#'
#' @return List with grouped `choices` (a named list of named character
#'   vectors for `selectInput`/`selectizeInput`) and flat `values`.
irf_response_choices <- function(models) {
  named <- irf_common_choices(models, irf_model_display_names)
  unnamed <- irf_common_choices(models, irf_model_unnamed_responses)
  choices <- list()
  if (length(named) > 0) choices[["Named variables"]] <- named
  if (length(unnamed) > 0) choices[["Variable codes"]] <- unnamed
  list(choices = choices, values = unname(c(named, unnamed)))
}

#' Return Scenario Mode response-variable choices grouped by whether each
#' variable has a metadata display name.
#'
#' A row is "named" when its `display_name` differs from its `dynare_name`
#' (the fallback used by `ezdyn::display_name()` when no metadata matches).
#'
#' @param data Pretty IRF data frame with `dynare_name` and `display_name`.
#'
#' @return List with grouped `choices` and flat `values` (see
#'   [irf_response_choices()]).
irf_scenario_response_choices <- function(data) {
  if (is.null(data) || nrow(data) == 0 ||
      !all(c("dynare_name", "display_name") %in% names(data))) {
    return(list(choices = list(), values = character()))
  }

  is_named <- data$display_name != data$dynare_name
  named <- irf_choices(data$display_name[is_named])
  unnamed <- irf_choices(data$display_name[!is_named])
  choices <- list()
  if (length(named) > 0) choices[["Named variables"]] <- named
  if (length(unnamed) > 0) choices[["Variable codes"]] <- unnamed
  list(choices = choices, values = unname(c(named, unnamed)))
}
#' Filter curated Scenario Mode IRFs with dashboard selector values.
#'
#' @param scenarios Curated Scenario Mode data.
#' @param models Model labels to retain.
#' @param scenario_names Scenario-group labels to retain.
#' @param shocks Scenario-variant labels to retain.
#' @param display_names Display names to retain.
#'
#' @return A filtered data frame.
irf_filter_scenarios <- function(scenarios, models, scenario_names, shocks, display_names) {
  scenarios |>
    dplyr::filter(
      model_name %in% models,
      scenario %in% scenario_names,
      shock %in% shocks,
      display_name %in% display_names
    )
}

#' Return a deterministic footnote for selected Scenario Mode IRFs.
#'
#' @param scenarios Filtered Scenario Mode IRF data.
#'
#' @return One character string or `NULL`.
irf_scenario_footnote <- function(scenarios) {
  if (!("footnote" %in% names(scenarios))) {
    return(NULL)
  }

  footnotes <- unique(scenarios$footnote)
  footnotes <- footnotes[!is.na(footnotes) & footnotes != ""]
  if (length(footnotes) == 0) NULL else paste(sort(footnotes), collapse = "\n")
}

#' Return a Scenario Mode availability message.
#'
#' @param models Named list of selected model pairs.
#' @param scenarios Filtered Scenario Mode data.
#'
#' @return One character message or `NULL`. A message naming unavailable models
#'   is advisory when at least one selected model has curated rows.
irf_scenario_message <- function(models, scenarios) {
  if (length(models) == 0) {
    return("Select at least one model.")
  }
  if (is.null(scenarios) || nrow(scenarios) == 0) {
    return("No curated scenarios are available for the selected model combination.")
  }
  available_models <- intersect(names(models), unique(scenarios$model_name))
  unavailable_models <- setdiff(names(models), available_models)
  if (length(unavailable_models) > 0) {
    return(sprintf(
      "This curated scenario is unavailable for %s. Showing results for %s.",
      paste(unavailable_models, collapse = ", "),
      paste(available_models, collapse = ", ")
    ))
  }

  NULL
}

#' Return the graph title for curated Scenario Mode IRFs.
#'
#' @param display_names Selected response display names.
#' @param scenario_name Selected scenario label.
#'
#' @return One character string.
irf_scenario_plot_title <- function(display_names, scenario_name) {
  if (length(display_names) == 1) {
    sprintf("%s — Response of %s", scenario_name, display_names)
  } else {
    sprintf("Responses to %s", scenario_name)
  }
}
