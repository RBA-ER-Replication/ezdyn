# Custom IRF helpers -------------------------------------------------------
#
# This file owns the model-specific shock selector and calculation adapters for
# Custom IRF and Custom IRF (raw shocks). Shared Scenario Mode helpers remain
# in `dash_irf_helpers.R`.

#' Return direct-shock codes with defined display names.
#'
#' @param model A Dynare or custom-MOO pair.
#'
#' @return Named character vector of native shock codes by display name.
irf_model_shock_labels <- function(model) {
  metadata <- model$M_$shock_meta
  if (is.null(metadata) || !all(c("shock", "description") %in% names(metadata))) {
    return(character())
  }

  shock_names <- unique(c(
    model$M_$exo.names,
    model$M_$exo.vars,
    model$oo_$irf$shock
  )) |>
    as.character() |>
    stats::na.omit()
  descriptions <- as.character(metadata$description[match(shock_names, metadata$shock)])
  defined <- !is.na(descriptions) & nzchar(descriptions)
  stats::setNames(
    shock_names[defined],
    tools::toTitleCase(tolower(descriptions[defined]))
  )
}

#' Return model-specific rows for shocks with defined display names.
#'
#' @param model A Dynare or custom-MOO pair.
#' @param model_name Dashboard label for `model`.
#'
#' @return Data frame with model label, native shock code, and display name.
irf_model_shock_options <- function(model, model_name) {
  labels <- irf_model_shock_labels(model)
  data.frame(
    model_name = rep(model_name, length(labels)),
    shock = unname(labels),
    display_name = names(labels),
    stringsAsFactors = FALSE
  )
}

#' Build custom-IRF shock choices and model-specific native-shock mappings.
#'
#' Only shocks with explicit metadata display names are included. A display name
#' is common only when it identifies one shock in every selected model.
#'
#' @param models Named list of selected model pairs.
#'
#' @return List with grouped `choices`, available `values`, `map`, and model names.
irf_shock_selector <- function(models) {
  model_names <- names(models)
  empty <- list(
    choices = list(),
    values = character(),
    map = data.frame(),
    model_names = model_names
  )
  if (length(models) == 0) {
    return(empty)
  }

  options <- purrr::imap_dfr(models, irf_model_shock_options) |>
    dplyr::distinct(model_name, shock, display_name)
  if (nrow(options) == 0) {
    return(empty)
  }

  common_display_names <- if (length(models) > 1) {
    options |>
      dplyr::group_by(display_name) |>
      dplyr::summarise(
        model_count = dplyr::n_distinct(model_name),
        shock_count = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::filter(
        model_count == length(models),
        shock_count == length(models)
      ) |>
      dplyr::pull(display_name)
  } else {
    character()
  }
  common_map <- options |>
    dplyr::filter(display_name %in% common_display_names) |>
    dplyr::mutate(selection = paste0("common::", display_name))
  model_map <- options |>
    dplyr::filter(!(display_name %in% common_display_names)) |>
    dplyr::mutate(selection = paste("model", model_name, shock, sep = "::"))
  map <- dplyr::bind_rows(common_map, model_map)

  choices <- list()
  common_choices <- common_map |>
    dplyr::distinct(selection, display_name) |>
    dplyr::arrange(display_name)
  if (nrow(common_choices) > 0) {
    choices[["Common shocks"]] <- stats::setNames(
      common_choices$selection,
      common_choices$display_name
    )
  }
  for (model_label in model_names) {
    model_choices <- model_map |>
      dplyr::filter(model_name == model_label) |>
      dplyr::distinct(selection, display_name) |>
      dplyr::arrange(display_name)
    if (nrow(model_choices) > 0) {
      choices[[model_label]] <- stats::setNames(
        model_choices$selection,
        model_choices$display_name
      )
    }
  }

  list(
    choices = choices,
    values = unique(map$selection),
    map = map,
    model_names = model_names
  )
}

#' Return the preferred one-shock-per-model custom-IRF selection.
#'
#' @param selector Value returned by [irf_shock_selector()].
#'
#' @return Character vector of selector values.
irf_default_shock_selection <- function(selector) {
  if (nrow(selector$map) == 0) {
    return(character())
  }
  common_selection <- selector$map$selection[
    startsWith(selector$map$selection, "common::")
  ]
  if (length(common_selection) > 0) {
    return(common_selection[[1]])
  }

  unname(vapply(selector$model_names, function(model_name) {
    selector$map$selection[match(model_name, selector$map$model_name)]
  }, character(1)))
}

#' Preserve a valid custom-IRF shock selection or use the preferred default.
#'
#' @param current Current selector values.
#' @param selector Value returned by [irf_shock_selector()].
#'
#' @return Character vector of selector values.
irf_preserve_shock_selection <- function(current, selector) {
  selected <- intersect(current, selector$values)
  resolved <- irf_resolve_model_shocks(selector, selected)
  if (isTRUE(resolved$valid)) selected else irf_default_shock_selection(selector)
}

#' Resolve grouped choices to one native shock code for each selected model.
#'
#' @param selector Value returned by [irf_shock_selector()].
#' @param selected Character vector of selector values.
#'
#' @return List with `valid`, `message`, named `shocks`, and named display labels.
irf_resolve_model_shocks <- function(selector, selected) {
  selected <- unique(as.character(selected))
  selected <- selected[!is.na(selected) & nzchar(selected)]
  selected <- intersect(selected, selector$values)
  selected_map <- selector$map[selector$map$selection %in% selected, , drop = FALSE]
  selected_models <- unique(selected_map$model_name)
  missing_models <- setdiff(selector$model_names, selected_models)
  duplicate_models <- unique(selected_map$model_name[duplicated(selected_map$model_name)])

  if (length(missing_models) > 0) {
    return(list(
      valid = FALSE,
      message = sprintf("Choose one displayed shock for each selected model. Missing: %s.", paste(missing_models, collapse = ", "))
    ))
  }
  if (length(duplicate_models) > 0) {
    return(list(
      valid = FALSE,
      message = sprintf("Choose only one shock for each model. Multiple shocks are selected for: %s.", paste(duplicate_models, collapse = ", "))
    ))
  }

  selected_map <- selected_map[match(selector$model_names, selected_map$model_name), , drop = FALSE]
  list(
    valid = TRUE,
    message = NULL,
    shocks = stats::setNames(selected_map$shock, selected_map$model_name),
    display_names = stats::setNames(selected_map$display_name, selected_map$model_name)
  )
}

#' Format resolved custom-IRF shock descriptions for chart text.
#'
#' @param shock_display_names Named shock display names by model.
#'
#' @return One character string.
irf_shock_description <- function(shock_display_names) {
  model_names <- names(shock_display_names)
  shock_display_names <- as.character(shock_display_names)
  names(shock_display_names) <- model_names
  if (length(shock_display_names) == 0) {
    return("")
  }
  if (length(unique(shock_display_names)) == 1) {
    return(shock_display_names[[1]])
  }

  paste(
    sprintf("%s: %s", names(shock_display_names), shock_display_names),
    collapse = "; "
  )
}

#' Return whether a model exposes any displayed custom-IRF shocks.
#'
#' @param model A Dynare or custom-MOO pair.
#'
#' @return One logical value.
irf_model_has_displayed_shock <- function(model) {
  length(irf_model_shock_labels(model)) > 0
}

#' Return enabled IRF mode choices for a dashboard configuration.
#'
#' @param config Validated dashboard configuration.
#'
#' @return A named character vector.
irf_mode_choices <- function(config) {
  modes <- character()
  if (!is.null(config$irf$scenarios) && nrow(config$irf$scenarios) > 0) {
    modes <- c(modes, `Default (Pre-prepared scenarios)` = "scenario")
  }

  model_is_usable <- vapply(config$models, function(model) {
    irf_model_has_displayed_shock(model) && length(irf_model_display_names(model)) > 0
  }, logical(1))
  if (any(model_is_usable)) {
    modes <- c(
      modes,
      `Custom IRF` = "targeted",
      `Custom IRF (raw shocks)` = "direct"
    )
  }

  modes
}

#' Return a custom-mode selection message when model capabilities are incompatible.
#'
#' @param mode Current custom IRF mode.
#' @param models Named list of selected model pairs.
#'
#' @return One character message or `NULL`.
irf_custom_message <- function(mode, models) {
  if (length(models) == 0) {
    return("Select at least one model.")
  }
  if (any(!vapply(models, irf_model_has_displayed_shock, logical(1)))) {
    return("Every selected model must provide at least one displayed IRF shock.")
  }
  if (identical(mode, "targeted") &&
      length(irf_response_choices(models)$values) == 0) {
    return("The selected models do not share a target response variable.")
  }
  if (identical(mode, "direct") &&
      length(irf_response_choices(models)$values) == 0) {
    return("The selected models do not share a response variable.")
  }

  NULL
}

#' Return a graph title for a Custom IRF mode.
#'
#' @param mode Current custom IRF mode.
#' @param display_names Selected response display names.
#' @param shock_display_names Selected title-cased shock labels.
#'
#' @return One character string.
irf_custom_plot_title <- function(mode, display_names, shock_display_names = NULL) {
  shock_label <- irf_shock_description(shock_display_names)
  if (!nzchar(shock_label)) {
    shock_label <- switch(mode, targeted = "Custom IRF", direct = "Custom IRF (Raw Shocks)")
  }
  if (length(display_names) == 1) {
    sprintf("Response of %s to %s", display_names, shock_label)
  } else {
    sprintf("Responses to %s", shock_label)
  }
}

#' Return the mode-specific Custom IRF plot footnote.
#'
#' @param mode Current custom IRF mode.
#' @param data Pretty IRF data.
#' @param use_cd Whether cognitive discounting was requested.
#' @param lambda Cognitive-discounting parameter.
#' @param shock_display_names Selected title-cased shock labels.
#' @param target_display_name Metadata display name of the calibrated target.
#' @param target_value First-quarter calibrated target in displayed units.
#'
#' @return One character string.
irf_custom_footnote <- function(
    mode,
    data,
    use_cd = FALSE,
    lambda = 0,
    shock_display_names = NULL,
    target_display_name = NULL,
    target_value = NULL) {
  if (identical(mode, "direct")) {
    shock_label <- irf_shock_description(shock_display_names)
    if (!nzchar(shock_label)) {
      return("Custom IRF raw shocks are uncalibrated.")
    }
    if (length(unique(shock_display_names)) == 1) {
      return(sprintf("%s is an uncalibrated raw shock.", shock_label))
    }
    return(sprintf("%s are uncalibrated raw shocks.", shock_label))
  }

  calibration <- "Custom IRF is calibrated to the selected first-quarter target."
  if (!is.null(shock_display_names) && length(shock_display_names) > 0 &&
      !is.null(target_display_name) && !is.null(target_value)) {
    unit_symbols <- unique(as.character(data$unit_symbol_irf[
      data$display_name %in% target_display_name
    ]))
    unit_symbols <- unit_symbols[!is.na(unit_symbols) & nzchar(unit_symbols)]
    unit <- if (length(unit_symbols) > 0) paste0(" ", unit_symbols[[1]]) else ""
    shock_label <- irf_shock_description(shock_display_names)
    verb <- if (length(unique(shock_display_names)) == 1) "is" else "are each"
    calibration <- sprintf(
      "%s %s calibrated to a %s%s impact on %s in the first quarter.",
      shock_label,
      verb,
      format(target_value, trim = TRUE, scientific = FALSE),
      unit,
      target_display_name
    )
  }
  if (!isTRUE(use_cd) || lambda <= 0) {
    return(calibration)
  }
  if (all(data$effective_use_cd)) {
    return(paste(
      calibration,
      sprintf("Cognitive discounting is used (lambda = %s).", lambda)
    ))
  }

  paste(
    calibration,
    sprintf("Cognitive discounting was requested (lambda = %s).", lambda),
    "Models without an anticipated shock use an unanticipated target IRF.",
    sep = "\n"
  )
}

#' Normalize pretty IRF units for custom-mode rendering.
#'
#' @param irf Pretty IRF data returned by ezdyn.
#'
#' @return Pretty IRF data with complete display units.
irf_normalize_pretty <- function(irf) {
  if ("display_unit" %in% names(irf)) {
    irf$display_unit[is.na(irf$display_unit) | irf$display_unit == ""] <- "Unit not specified"
  }
  irf
}

#' Return whether a model supports a one-quarter anticipated shock.
#'
#' @param model A Dynare or custom-MOO pair.
#' @param shock_name Selected unanticipated shock code.
#'
#' @return One logical value.
irf_supports_cd <- function(model, shock_name) {
  paste0(shock_name, "_1") %in% c(model$M_$exo.names, model$M_$exo.vars, model$oo_$irf$shock)
}

#' Validate one selected native shock code for every model.
#'
#' @param models Named list of selected model pairs.
#' @param model_shocks Named native shock codes.
#'
#' @return Named character vector ordered by `models`.
irf_validate_model_shocks <- function(models, model_shocks) {
  model_names <- names(models)
  if (!is.character(model_shocks) || is.null(names(model_shocks)) ||
      !setequal(names(model_shocks), model_names) || anyDuplicated(names(model_shocks))) {
    stop("`model_shocks` must name one displayed shock for every selected model.", call. = FALSE)
  }

  model_shocks <- model_shocks[model_names]
  for (model_name in model_names) {
    available_shocks <- unname(irf_model_shock_labels(models[[model_name]]))
    if (length(model_shocks[[model_name]]) != 1 || is.na(model_shocks[[model_name]]) ||
        !(model_shocks[[model_name]] %in% available_shocks)) {
      stop(
        sprintf("Model `%s` does not provide displayed shock `%s`.", model_name, model_shocks[[model_name]]),
        call. = FALSE
      )
    }
  }
  model_shocks
}

#' Run one-quarter Custom IRFs for selected models.
#'
#' @param models Named list of selected model pairs.
#' @param target_display_name Metadata display name to target.
#' @param target_value First-quarter target in displayed units.
#' @param model_shocks Named native shock codes, one per model.
#' @param horizon Positive integer IRF horizon.
#' @param use_cd Whether cognitive discounting was requested.
#' @param lambda Cognitive-discounting parameter.
#'
#' @return A pretty IRF data frame.
irf_run_targeted <- function(
    models,
    target_display_name,
    target_value,
    model_shocks,
    horizon,
    use_cd,
    lambda) {
  if (!is.numeric(target_value) || length(target_value) != 1 || !is.finite(target_value)) {
    stop("`target_value` must be one finite numeric value.", call. = FALSE)
  }
  if (!is.numeric(horizon) || length(horizon) != 1 || !is.finite(horizon) ||
      horizon < 1 || horizon %% 1 != 0) {
    stop("`horizon` must be one positive integer.", call. = FALSE)
  }

  target <- stats::setNames(list(target_value), target_display_name)
  requested_cd <- isTRUE(use_cd) && lambda > 0
  model_shocks <- irf_validate_model_shocks(models, model_shocks)

  ezdyn_combine_models_dfr(models, function(model, model_name) {
    shock_name <- model_shocks[[model_name]]
    shock_timing <- stats::setNames(list(1L), shock_name)
    effective_use_cd <- requested_cd && irf_supports_cd(model, shock_name)
    if (requested_cd && !effective_use_cd) {
      warning(
        sprintf(
          "Model `%s` does not support anticipated shock `%s_1`; using an unanticipated target IRF.",
          model_name,
          shock_name
        ),
        call. = FALSE
      )
    }

    shock_nickname <- if (effective_use_cd) {
      "Custom target (cognitive discounting)"
    } else if (requested_cd) {
      "Custom target (unanticipated fallback)"
    } else {
      "Custom target"
    }
    run_target <- function() {
      if (effective_use_cd) {
        get_irf_target_gabaix(
          model$M_, model$oo_, horizon = horizon,
          target = target, shock_timing = shock_timing, lambda = lambda,
          pretty = TRUE, shock_nickname = shock_nickname,
          scale_targets = TRUE
        )
      } else {
        get_irf_target(
          model$M_, model$oo_, horizon = horizon,
          target = target, shock_timing = shock_timing,
          pretty = TRUE, shock_nickname = shock_nickname,
          scale_targets = TRUE
        )
      }
    }

    run_target() |>
      irf_normalize_pretty() |>
      dplyr::mutate(
        effective_use_cd = effective_use_cd,
        irf_mode = "targeted"
      )
  }, context = "Custom targeted IRF")
}

#' Run Custom IRF raw shocks for selected models.
#'
#' @param models Named list of selected model pairs.
#' @param model_shocks Named native shock codes, one per model.
#' @param horizon Positive integer IRF horizon.
#'
#' @return A pretty IRF data frame.
irf_run_direct <- function(models, model_shocks, horizon) {
  if (!is.numeric(horizon) || length(horizon) != 1 || !is.finite(horizon) ||
      horizon < 1 || horizon %% 1 != 0) {
    stop("`horizon` must be one positive integer.", call. = FALSE)
  }
  model_shocks <- irf_validate_model_shocks(models, model_shocks)

  ezdyn_combine_models_dfr(models, function(model, model_name) {
    shock_name <- model_shocks[[model_name]]
    get_irf(
      model$M_, model$oo_, shock_names = shock_name,
      horizon = horizon, pretty = TRUE
    ) |>
      irf_normalize_pretty() |>
      dplyr::mutate(irf_mode = "direct")
  }, context = "Direct shock IRF")
}

#' Label pretty IRF output with selected model-specific shocks.
#'
#' @param data Pretty IRF data with a `model_name` column.
#' @param model_shocks Named native shock codes, one per model.
#' @param shock_display_names Named shock display names, one per model.
#'
#' @return Pretty IRF data with displayed `shock` and native `shock_code`.
irf_label_selected_shocks <- function(data, model_shocks, shock_display_names) {
  model_names <- unique(as.character(data$model_name))
  if (!setequal(names(model_shocks), model_names) ||
      !setequal(names(shock_display_names), model_names)) {
    stop("Selected shock mappings must name every model in the pretty IRF data.", call. = FALSE)
  }

  data |>
    dplyr::mutate(
      shock_code = unname(model_shocks[model_name]),
      shock = unname(shock_display_names[model_name])
    )
}
