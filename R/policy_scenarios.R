# Optimal policy scenarios ---------------------------------------------------

#' Run one optimal-policy scenario for one model.
#'
#' `baseline` has a quarterly `date` column and includes the output, loss,
#' constraint, and instrument variables named in `policy_strategy`, each
#' column keyed by either `model`'s own dynare code or (as in a canonical
#' baseline returned by [import_baseline()]) its display name. The returned
#' paths are rescaled and labelled with model metadata.
#'
#' @param policy_strategy Named list with `output_name`, `output_variables`,
#'   `loss_variables`, `loss_weights`, `discount_factor`, `T_loss`,
#'   `T_instrument`, and `commit` (`"commit"`, `"timeless"`, or `"disc"`).
#'   Optional `instrument_variable` and `instrument_shock` identify the model's
#'   policy instrument and innovation. Variable entries (`output_variables`,
#'   `loss_variables`, `instrument_variable`, and constraint `variable`s) may
#'   be display names or dynare codes, resolved against `model`; a trailing
#'   `_gap_loss` suffix is resolved on the underlying variable.
#'   `instrument_shock` may be a shock description or code.
#' @param baseline Wide baseline data frame keyed by `date` plus one column per
#'   variable referenced from `policy_strategy`, named by either `model`'s own
#'   dynare code or its display name.
#' @param model Full Dynare or custom-MOO pair.
#' @param use_cd Whether to use partially anticipated policy shocks.
#' @param lambda Cognitive-discounting parameter.
#' @param forecast_start First forecast quarter.
#' @param forecast_end Final forecast quarter.
#' @param loss_variable_vintages Optional long history for timeless commitment.
#' @param preloss_start First historical commitment quarter for timeless policy.
#' @param policy_irfs Optional cached array returned by [build_policy_irfs()].
#'
#' @return A long data frame containing baseline and alternative paths.
#' @export
get_policy_scenario <- function(
    policy_strategy,
    baseline,
    model,
    use_cd = FALSE,
    lambda = 0,
    forecast_start,
    forecast_end,
    loss_variable_vintages = NULL,
    preloss_start = NULL,
    policy_irfs = NULL) {
  ezdyn_validate_policy_model(model)
  required_strategy <- c(
    "output_name", "output_variables", "loss_variables", "loss_weights",
    "discount_factor", "T_loss", "T_instrument", "commit"
  )
  missing_strategy <- setdiff(required_strategy, names(policy_strategy))
  if (length(missing_strategy) > 0) {
    stop(sprintf("`policy_strategy` is missing: %s.", paste(missing_strategy, collapse = ", ")), call. = FALSE)
  }
  if (!is.data.frame(baseline) || !("date" %in% names(baseline))) {
    stop("`baseline` must be a data frame with a Date `date` column.", call. = FALSE)
  }

  forecast_dates <- seq(as.Date(forecast_start), as.Date(forecast_end), by = "quarter")
  T <- length(forecast_dates)
  T_loss <- as.integer(policy_strategy$T_loss)
  T_instrument <- as.integer(policy_strategy$T_instrument)
  commit <- as.character(policy_strategy$commit)
  if (!(commit %in% c("commit", "timeless", "disc"))) {
    stop("`policy_strategy$commit` must be `commit`, `timeless`, or `disc`.", call. = FALSE)
  }
  if (T_loss < 1 || T_instrument < 1 || T_loss > T || T_instrument > T) {
    stop("Policy loss and instrument horizons must be positive and within the supplied forecast horizon.", call. = FALSE)
  }

  # 1. Resolve output/loss/instrument/constraint variables and the instrument
  # shock to this model's own dynare codes. Identifiers may be display names,
  # shock descriptions, or already-canonical codes (see
  # `ezdyn_resolve_policy_variable_names()`/`ezdyn_resolve_policy_shock_name()`)
  # so the same `policy_strategy` can be reused across models with different
  # internal codes but shared display names.
  output_variables_in <- unlist(policy_strategy$output_variables, use.names = FALSE)
  loss_variables_in <- unlist(policy_strategy$loss_variables, use.names = FALSE)
  instrument_variable_in <- policy_strategy$instrument_variable
  if (is.null(instrument_variable_in)) instrument_variable_in <- "r_obs"
  instrument_shock <- policy_strategy$instrument_shock
  if (is.null(instrument_shock)) instrument_shock <- "eps_r"
  constraints <- policy_strategy$constraints
  if (is.null(constraints)) constraints <- list()
  constraint_variables_in <- vapply(constraints, `[[`, character(1), "variable")

  output_variables <- ezdyn_resolve_policy_variable_names(output_variables_in, model$M_)
  loss_variables <- ezdyn_resolve_policy_variable_names(loss_variables_in, model$M_)
  instrument_variable <- ezdyn_resolve_policy_variable_names(instrument_variable_in, model$M_)
  instrument_shock <- ezdyn_resolve_policy_shock_name(instrument_shock, model$M_)
  constraint_variables_resolved <- ezdyn_resolve_policy_variable_names(constraint_variables_in, model$M_)
  constraints <- Map(function(constraint, resolved) {
    constraint$variable <- resolved
    constraint
  }, constraints, constraint_variables_resolved)
  constraint_variables <- unique(constraint_variables_resolved)

  variables <- unique(c(output_variables, loss_variables, constraint_variables, instrument_variable))
  policy_variables <- unique(c(loss_variables, constraint_variables, instrument_variable))

  # 2. Locate each resolved variable's baseline column. It may be named by
  # its resolved (dynare-code) identifier - a legacy model-code-keyed baseline
  # - or by the identifier as originally supplied in `policy_strategy` (e.g. a
  # display name, or a synthetic `<display name>_gap_loss` column a caller
  # added onto a canonical, display-name-keyed baseline), or by `model`'s own
  # display name for the resolved code (needed for a display-name-keyed
  # baseline when a variable was only ever supplied as a dynare code, e.g. a
  # response/output variable never repeated under its display name
  # elsewhere in `policy_strategy`). The same resolved variable can be
  # supplied under different original identifiers in different roles (e.g.
  # an output variable given as a dynare code and the instrument given as
  # its display name, both resolving to the same model variable), so every
  # original candidate is kept, not just the first encountered. Once
  # located, columns are renamed to the resolved identifiers so every
  # calculation below keeps operating in resolved-name space.
  originals_for <- list()
  for (role in list(
    list(resolved = output_variables, original = output_variables_in),
    list(resolved = loss_variables, original = loss_variables_in),
    list(resolved = constraint_variables_resolved, original = constraint_variables_in),
    list(resolved = instrument_variable, original = instrument_variable_in)
  )) {
    for (i in seq_along(role$resolved)) {
      key <- role$resolved[[i]]
      originals_for[[key]] <- unique(c(originals_for[[key]], role$original[[i]]))
    }
  }
  baseline_columns <- stats::setNames(vapply(variables, function(variable) {
    if (variable %in% names(baseline)) return(variable)
    candidates <- unique(c(originals_for[[variable]], display_name(variable, model$M_)))
    found <- candidates[candidates %in% names(baseline)]
    if (length(found) > 0) return(found[[1]])
    NA_character_
  }, character(1)), variables)

  missing_variables <- policy_variables[is.na(baseline_columns[policy_variables])]
  if (length(missing_variables) > 0) {
    stop(sprintf("Baseline is missing policy variable(s): %s.", paste(missing_variables, collapse = ", ")), call. = FALSE)
  }

  baseline$date <- as.Date(baseline$date)
  found_columns <- baseline_columns[!is.na(baseline_columns) & baseline_columns != names(baseline_columns)]
  names(baseline)[match(found_columns, names(baseline))] <- names(found_columns)
  baseline_forecast <- baseline[match(forecast_dates, baseline$date), variables, drop = FALSE]
  if (anyNA(baseline_forecast[, policy_variables, drop = FALSE])) {
    stop("Baseline must cover every forecast quarter and contain complete policy inputs.", call. = FALSE)
  }
  Y0 <- t(as.matrix(baseline_forecast))
  T_preloss <- 0L
  if (commit == "timeless") {
    if (is.null(preloss_start)) {
      stop("Timeless commitment requires `preloss_start`.", call. = FALSE)
    }
    T_preloss <- length(seq(as.Date(preloss_start), as.Date(forecast_start), by = "quarter")) - 1L
  }
  T_irf <- max(T, T_loss + T_preloss)
  T_instrument_irf <- max(T_instrument, T_instrument + T_preloss)
  dY <- ezdyn_policy_irfs(
    policy_irfs, model, variables, T_irf, T_instrument_irf, use_cd, lambda, instrument_shock
  )

  i_loss <- match(loss_variables, variables)
  loss_Y0 <- Y0[i_loss, seq_len(T_loss), drop = FALSE]
  loss_dY <- dY[i_loss, seq_len(T_loss), seq_len(T_instrument), drop = FALSE]
  weights <- as.numeric(policy_strategy$loss_weights)
  if (length(weights) != length(loss_variables) || any(!is.finite(weights)) || any(weights < 0)) {
    stop("`loss_weights` must be one non-negative finite value per loss variable.", call. = FALSE)
  }
  discount_factor <- as.numeric(policy_strategy$discount_factor)
  if (length(discount_factor) != 1 || !is.finite(discount_factor) || discount_factor < 0 || discount_factor > 1) {
    stop("`discount_factor` must be one value from zero to one.", call. = FALSE)
  }

  if (commit == "commit") {
    constraint_matrices <- ezdyn_policy_constraints(
      constraints, dY[, seq_len(T_loss), seq_len(T_instrument), drop = FALSE], Y0[, seq_len(T_loss), drop = FALSE]
    )
    shocks <- ezdyn_policy_commit(loss_Y0, loss_dY, weights, discount_factor, constraint_matrices)
  } else if (commit == "disc") {
    constraint_variables <- unique(c(loss_variables, constraint_variables))
    constraint_indices <- match(constraint_variables, variables)
    constraint_weights <- numeric(length(constraint_variables))
    constraint_weights[match(loss_variables, constraint_variables)] <- weights
    constraint_Y0 <- Y0[constraint_indices, seq_len(T_loss), drop = FALSE]
    constraint_dY <- dY[constraint_indices, seq_len(T_loss), seq_len(T_instrument), drop = FALSE]
    constraint_matrices <- ezdyn_policy_discretion_constraints(constraints, constraint_dY)
    shocks <- ezdyn_policy_discretion(
      constraint_Y0, constraint_dY, constraint_weights, discount_factor, constraint_matrices
    )
  } else {
    history <- ezdyn_policy_history(
      loss_variable_vintages, loss_variables, preloss_start, forecast_start, T_loss, loss_Y0
    )
    constraint_matrices <- ezdyn_policy_timeless_constraints(
      constraints,
      dY[, seq_len(T_loss + T_preloss), seq_len(T_instrument + T_preloss), drop = FALSE],
      Y0[, seq_len(T_loss), drop = FALSE],
      loss_variable_vintages,
      preloss_start,
      forecast_start,
      T_loss,
      instrument_variable
    )
    shocks <- ezdyn_policy_timeless(
      history,
      dY[i_loss, seq_len(T_loss + T_preloss), seq_len(T_instrument + T_preloss), drop = FALSE],
      weights,
      discount_factor,
      constraint_matrices
    )
  }
  ezdyn_validate_policy_solution(constraints, dY[, seq_len(T_loss), seq_len(T_instrument), drop = FALSE], Y0[, seq_len(T_loss), drop = FALSE], shocks)

  output_dY <- dY[output_variables, seq_len(T), seq_len(T_instrument), drop = FALSE]
  alternative <- ezdyn_apply_policy_shocks(output_dY, shocks)
  scenario_dates <- c(seq(as.Date(forecast_start), by = "-1 quarter", length.out = 2)[[2]], forecast_dates)
  alternative <- rbind(0, t(alternative))
  colnames(alternative) <- output_variables
  scenario <- data.frame(date = scenario_dates, alternative, check.names = FALSE)
  baseline_rows <- baseline[, c("date", output_variables), drop = FALSE]

  names_units <- display_names_and_units(
    output_variables,
    model$M_,
    unit_symbol_baseline = TRUE
  )
  alternative_long <- tidyr::pivot_longer(scenario, -date, names_to = "dynare_name", values_to = "irf")
  baseline_long <- tidyr::pivot_longer(baseline_rows, -date, names_to = "dynare_name", values_to = "baseline")
  scenario_long <- dplyr::left_join(alternative_long, baseline_long, by = c("date", "dynare_name")) |>
    dplyr::left_join(names_units, by = c("dynare_name" = "Variable")) |>
    dplyr::mutate(
      model_name = model$M_$model_name,
      model_colour = if (is.null(model$M_$model_colour)) NA_character_ else model$M_$model_colour,
      alternative = baseline + irf
    ) |>
    tidyr::pivot_longer(c(baseline, alternative), names_to = "path_name", values_to = "value") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      path_name = ifelse(path_name == "alternative", as.character(policy_strategy$output_name), "Baseline"),
      display_name = as.character(ifelse(is.na(display_name) | display_name == "", dynare_name, display_name))
    )
  baseline_history <- baseline_long |>
    dplyr::filter(!(date %in% scenario_dates)) |>
    dplyr::left_join(names_units, by = c("dynare_name" = "Variable")) |>
    dplyr::transmute(
      date,
      dynare_name,
      display_name = as.character(ifelse(is.na(display_name) | display_name == "", dynare_name, display_name)),
      display_unit,
      unit_symbol_baseline,
      model_name = model$M_$model_name,
      model_colour = if (is.null(model$M_$model_colour)) NA_character_ else model$M_$model_colour,
      path_name = "Baseline",
      value = baseline
    )
  dplyr::bind_rows(scenario_long, baseline_history) |>
    dplyr::arrange(date)
}

#' Run optimal-policy scenarios for one or more models.
#'
#' Loops [get_policy_scenario()] over every model in `models` and every
#' strategy in `strategies`, so a dashboard or script only needs to supply a
#' named list of models to compare optimal policy across them - the same
#' "list of models in, one combined labelled data frame out" convention as
#' [get_alt_paths()]/[get_irfs_target()]. Every model receives the same
#' `strategies`/`use_cd`/`lambda`. A model with no anticipated policy shocks
#' (`<shock>_1`, `<shock>_2`, ...), or any model when `use_cd` is `FALSE` or
#' `lambda` is `0`, uses unanticipated shocks, for which timeless commitment
#' and discretion are not distinct: it gets a single commitment strategy with
#' no pre-forecast history, labelled `"Optimal (<model>)"`. A model with some
#' anticipated shocks, but too few for a strategy's optimisation horizon (plus
#' any timeless-commitment history), skips that strategy with a warning.
#'
#' @param strategies List of `policy_strategy` specifications (see
#'   [get_policy_scenario()]). Each is run against every model in `models`;
#'   any `output_name` already present is ignored and replaced with
#'   `"Optimal (<model>, Commitment)"`/`"Optimal (<model>, Discretion)"`,
#'   built from the model's name in `models` and `strategy$commit`
#'   (`"commit"`/`"timeless"` map to "Commitment", `"disc"` maps to
#'   "Discretion"), or `"Optimal (<model>)"` for a model using unanticipated
#'   shocks.
#' @param models Named list of full Dynare or custom-MOO pairs.
#' @param baseline Wide baseline data frame shared by every model, as in
#'   [get_policy_scenario()].
#' @param use_cd Whether to use partially anticipated policy shocks.
#' @param lambda Cognitive-discounting parameter.
#' @param forecast_start First forecast quarter.
#' @param forecast_end Final forecast quarter.
#' @param loss_variable_vintages Optional long history for timeless commitment.
#' @param preloss_start First historical commitment quarter for timeless policy.
#' @param irf_cache Optional environment used to cache each model/strategy's
#'   [build_policy_irfs()] result across repeated calls (e.g. across Shiny
#'   reactive invalidations that change only loss weights/targets, not
#'   variables/horizons). `NULL` (default) recomputes every call.
#'
#' @return A long data frame containing every model's baseline and
#'   alternative paths, combined via [get_policy_scenario()], labelled by
#'   each model's name in `models`.
#' @export
get_policy_scenarios <- function(
    strategies,
    models,
    baseline,
    use_cd = FALSE,
    lambda = 0,
    forecast_start,
    forecast_end,
    loss_variable_vintages = NULL,
    preloss_start = NULL,
    irf_cache = NULL) {
  ezdyn_validate_baseline_models(models)
  if (!is.list(strategies) || length(strategies) == 0) {
    stop("`strategies` must be a non-empty list of policy strategies.", call. = FALSE)
  }

  compute <- function(model, model_name) {
    # Unanticipated policy shocks (no anticipated shocks, or no anticipation)
    # make timeless commitment and discretion meaningless, so each strategy
    # becomes the same plain commitment strategy, run once.
    model_strategies <- unique(lapply(strategies, function(strategy) {
      strategy$unanticipated <- !(use_cd && lambda != 0) || ezdyn_policy_anticipated_count(model, strategy) == 0
      if (strategy$unanticipated) strategy$commit <- "commit"
      strategy
    }))
    # A plain lapply()/bind_rows() (not purrr::map_dfr()) so a genuine failure
    # for one strategy propagates its real, unwrapped message up to
    # ezdyn_combine_models_dfr()'s per-model error attribution, rather than
    # being replaced by purrr's own opaque "In index: n" wrapping.
    dplyr::bind_rows(lapply(model_strategies, function(strategy) {
      ezdyn_policy_scenario_for_model(
        strategy, model, model_name, baseline, use_cd, lambda,
        forecast_start, forecast_end, loss_variable_vintages, preloss_start, irf_cache
      )
    }))
  }

  ezdyn_combine_models_dfr(models, compute, context = "get_policy_scenarios()")
}

# Count the anticipated versions (`<shock>_1`, `<shock>_2`, ...) of a
# strategy's policy shock that a model supplies.
ezdyn_policy_anticipated_count <- function(model, strategy) {
  instrument_shock <- strategy$instrument_shock
  if (is.null(instrument_shock)) instrument_shock <- "eps_r"
  instrument_shock <- ezdyn_resolve_policy_shock_name(instrument_shock, model$M_)
  exogenous <- model$M_$exo.names
  if (is.null(exogenous)) exogenous <- model$M_$exo.vars
  length(grep(paste0("^", instrument_shock, "_[0-9]+$"), exogenous))
}

# Run one (model, strategy) pair for get_policy_scenarios(): label the
# strategy generically from the model's list name and commitment mode (just
# the model's name if its shocks are unanticipated),
# rebuild the variable/horizon inputs build_policy_irfs() needs (mirroring
# get_policy_scenario()'s own instrument fallbacks so the pre-computed IRF
# array always covers the instrument), and read/write the optional cache.
ezdyn_policy_scenario_for_model <- function(strategy, model, model_name, baseline, use_cd, lambda,
                                            forecast_start, forecast_end, loss_variable_vintages,
                                            preloss_start, irf_cache) {
  unanticipated <- isTRUE(strategy$unanticipated)
  if (unanticipated) use_cd <- FALSE
  strategy$output_name <- if (unanticipated) {
    sprintf("Optimal (%s)", model_name)
  } else {
    sprintf("Optimal (%s, %s)", model_name, ezdyn_policy_mode_label(strategy$commit))
  }

  instrument_variable <- strategy$instrument_variable
  if (is.null(instrument_variable)) instrument_variable <- "r_obs"
  instrument_shock <- strategy$instrument_shock
  if (is.null(instrument_shock)) instrument_shock <- "eps_r"
  constraint_variables <- vapply(strategy$constraints, `[[`, character(1), "variable")

  variables <- unique(c(
    unlist(strategy$output_variables, use.names = FALSE),
    unlist(strategy$loss_variables, use.names = FALSE),
    constraint_variables,
    instrument_variable
  ))
  preloss <- if (identical(strategy$commit, "timeless") && !is.null(preloss_start)) {
    length(seq(as.Date(preloss_start), as.Date(forecast_start), by = "quarter")) - 1L
  } else {
    0L
  }
  horizon <- max(length(seq(as.Date(forecast_start), as.Date(forecast_end), by = "quarter")), strategy$T_loss + preloss)
  instrument_horizon <- strategy$T_instrument + preloss

  # A custom-MOO model (e.g. an IRF spreadsheet import) is capped at its own
  # pre-tabulated horizon - unlike a Dynare model, it cannot be simulated
  # further on demand. Timeless commitment's combined horizon (loss/
  # instrument horizon plus the full preloss history back to
  # `preloss_start`) can easily exceed that cap for a model with a much
  # shorter policy strategy in mind. Rather than let this crash deep inside
  # `build_policy_irfs()`'s array indexing ("subscript out of bounds"),
  # gracefully skip just this (model, strategy) combination - every other
  # compatible combination (other strategies for this model, and every
  # strategy for other models) still runs, matching the same
  # graceful-degradation precedent already used for unsupported cognitive
  # discounting above.
  max_horizon <- ezdyn_policy_max_horizon(model)
  required_horizon <- max(horizon, instrument_horizon)
  if (required_horizon > max_horizon) {
    warning(
      sprintf(
        "Skipping `%s` for model `%s`: it only supplies impulse responses up to horizon %d, but this strategy requires %d (including any timeless-commitment history back to `preloss_start`). Reduce the loss/optimisation horizon or commitment-start date to include this model.",
        strategy$output_name, model_name, max_horizon, required_horizon
      ),
      call. = FALSE
    )
    return(NULL)
  }

  # Likewise skip a strategy needing more anticipated policy shocks than the
  # model supplies (rather than silently falling back to unanticipated shocks).
  anticipated <- ezdyn_policy_anticipated_count(model, strategy)
  if (use_cd && lambda != 0 && instrument_horizon > anticipated) {
    warning(
      sprintf(
        "Skipping `%s` for model `%s`: it only supplies %d anticipated policy shocks, but this strategy requires %d (including any timeless-commitment history back to `preloss_start`). Reduce the optimisation horizon or commitment-start date to include this model.",
        strategy$output_name, model_name, anticipated, instrument_horizon
      ),
      call. = FALSE
    )
    return(NULL)
  }

  policy_irfs <- ezdyn_cached_policy_irfs(
    irf_cache, model, model_name, variables, horizon, instrument_horizon,
    use_cd, lambda, instrument_shock
  )
  # Discretion with binding constraints may have no solution: omit just this
  # strategy, explaining what happened and how to fix it.
  tryCatch(
    get_policy_scenario(
      strategy, baseline, model, use_cd, lambda, forecast_start, forecast_end,
      loss_variable_vintages, preloss_start, policy_irfs
    ),
    ezdyn_policy_discretion_failure = function(condition) {
      constrained <- unique(vapply(
        strategy$constraints,
        function(constraint) sprintf("%s (%s bound)", constraint$variable, constraint$direction),
        character(1)
      ))
      warning(
        sprintf(
          "Omitted `%s` for model `%s`: no discretionary solution was found%s (%s). With constraints, the discretion may fail to find a stable, time-consistent solution. To fix this, try shortening the constraint period or relaxing the constraint.",
          strategy$output_name, model_name,
          if (length(constrained) > 0) paste(" with the constraint(s) on", paste(constrained, collapse = ", ")) else "",
          sub("\\.$", "", condition$message)
        ),
        call. = FALSE
      )
      NULL
    }
  )
}

# Map a policy strategy's `commit` mode to a generic display label - shared
# by every model, replacing any dashboard-specific hardcoded label.
ezdyn_policy_mode_label <- function(commit) {
  switch(commit, commit = "Commitment", timeless = "Commitment", disc = "Discretion", commit)
}

# Return the maximum horizon a model's IRF data can supply. A Dynare model
# can be simulated to any horizon on demand (no cap); a custom-MOO model
# (e.g. an imported IRF spreadsheet) is capped at its own pre-tabulated
# `oo_$irf$t` maximum (see `get_irf_custom()` in get_irf.R).
ezdyn_policy_max_horizon <- function(model) {
  if (inherits(model$oo_, "custom_moo") && !is.null(model$oo_$irf$t)) {
    max(model$oo_$irf$t)
  } else {
    Inf
  }
}

# Read/write build_policy_irfs() results from an optional cache environment,
# keyed by model, variables, horizons, and CD settings - mirrors the
# dashboard's own former per-session cache so repeated calls that only change
# loss weights/targets (not variables/horizons) avoid recomputing IRFs.
ezdyn_cached_policy_irfs <- function(cache, model, model_name, variables, horizon, instrument_horizon,
                                     use_cd, lambda, instrument_shock) {
  if (is.null(cache)) {
    return(build_policy_irfs(model, variables, horizon, instrument_horizon, use_cd, lambda, instrument_shock))
  }
  key <- paste(model_name, paste(variables, collapse = "|"), horizon, instrument_horizon, use_cd, lambda, sep = "::")
  if (exists(key, envir = cache, inherits = FALSE)) {
    return(get(key, envir = cache, inherits = FALSE))
  }
  value <- build_policy_irfs(model, variables, horizon, instrument_horizon, use_cd, lambda, instrument_shock)
  assign(key, value, envir = cache)
  value
}

#' Build monetary-policy IRFs for optimal policy.
#'
#' @param model Full Dynare or custom-MOO pair.
#' @param variables Model variable display names or dynare codes to retain.
#'   A trailing `_gap_loss` suffix is resolved on the underlying variable.
#' @param horizon Response horizon.
#' @param instrument_horizon Policy-shock horizon.
#' @param use_cd Whether to use partially anticipated policy shocks.
#' @param lambda Cognitive-discounting parameter.
#' @param instrument_shock Shock description or model code for the policy innovation.
#'
#' @return A variables by horizon by instrument-horizon array in displayed units,
#'   named by `model`'s own resolved dynare codes.
#' @export
build_policy_irfs <- function(model, variables, horizon, instrument_horizon, use_cd = FALSE, lambda = 0, instrument_shock = "eps_r") {
  ezdyn_validate_policy_model(model)
  variables <- ezdyn_resolve_policy_variable_names(variables, model$M_)
  instrument_shock <- ezdyn_resolve_policy_shock_name(instrument_shock, model$M_)
  exogenous <- model$M_$exo.names
  if (is.null(exogenous)) exogenous <- model$M_$exo.vars
  if (is.null(exogenous)) {
    stop("Model has no exogenous shock names.", call. = FALSE)
  }
  if (use_cd && lambda != 0) {
    anticipated <- grep(paste0("^", instrument_shock, "_[0-9]+$"), exogenous, value = TRUE)
    if (instrument_horizon > length(anticipated)) {
      warning("Policy model does not support the requested anticipated-shock horizon; using unanticipated policy shocks.", call. = FALSE)
      use_cd <- FALSE
    }
  }
  max_horizon <- ezdyn_policy_max_horizon(model)
  required_horizon <- max(horizon, instrument_horizon)
  if (required_horizon > max_horizon) {
    stop(
      sprintf(
        "Policy model only supplies impulse responses up to horizon %d, but this calculation requires %d. Reduce `horizon`/`instrument_horizon`, or use a model with longer IRF data.",
        max_horizon, required_horizon
      ),
      call. = FALSE
    )
  }
  matrix_irf <- if (use_cd && lambda != 0) {
    get_ir_matrix_gabaix(model$M_, model$oo_, horizon, instrument_shock, instrument_horizon, lambda, var_names = variables)$Mh_total
  } else {
    get_ir_matrix(model$M_, model$oo_, horizon, stats::setNames(list(seq_len(instrument_horizon)), instrument_shock), var_names = variables)
  }
  available <- dimnames(matrix_irf)[[1]]
  missing_variables <- setdiff(variables, available)
  if (length(missing_variables) > 0) {
    stop(sprintf("Policy model has no IRFs for: %s.", paste(missing_variables, collapse = ", ")), call. = FALSE)
  }
  dY <- matrix_irf[variables, seq_len(horizon), 1, seq_len(instrument_horizon), drop = FALSE]
  dY <- array(dY, dim = c(length(variables), horizon, instrument_horizon), dimnames = list(variables, NULL, NULL))
  for (i in seq_len(instrument_horizon)) {
    values <- as.data.frame(t(dY[, , i, drop = FALSE][, , 1]))
    names(values) <- variables
    dY[, , i] <- t(as.matrix(rescale(values, model$M_)))
  }
  dY
}

ezdyn_validate_policy_model <- function(model) {
  ezdyn_validate_baseline_moo(model, "model")
  invisible(model)
}

ezdyn_policy_irfs <- function(cached, model, variables, horizon, instrument_horizon, use_cd, lambda, instrument_shock) {
  if (!is.null(cached) &&
      all(variables %in% dimnames(cached)[[1]]) &&
      dim(cached)[2] >= horizon && dim(cached)[3] >= instrument_horizon) {
    return(cached[variables, seq_len(horizon), seq_len(instrument_horizon), drop = FALSE])
  }
  build_policy_irfs(model, variables, horizon, instrument_horizon, use_cd, lambda, instrument_shock)
}

ezdyn_apply_policy_shocks <- function(dY, shocks) {
  dimensions <- dim(dY)
  response <- matrix(
    dY,
    nrow = dimensions[1] * dimensions[2],
    ncol = dimensions[3]
  ) %*% as.numeric(shocks)
  matrix(response, nrow = dimensions[1], ncol = dimensions[2])
}

ezdyn_policy_constraints <- function(constraints, dY, Y0) {
  A_ineq <- matrix(0, 0, dim(dY)[3])
  b_ineq <- numeric()
  for (constraint in constraints) {
    index <- match(constraint$variable, dimnames(dY)[[1]])
    if (is.na(index) || !(constraint$direction %in% c("lower", "upper"))) {
      stop("Policy constraints require an available variable and `lower` or `upper` direction.", call. = FALSE)
    }
    for (period in constraint$period[constraint$period <= dim(dY)[2]]) {
      A <- dY[index, period, ]
      b <- constraint$value - Y0[index, period]
      if (constraint$direction == "lower") {
        A <- -A
        b <- -b
      }
      A_ineq <- rbind(A_ineq, A)
      b_ineq <- c(b_ineq, b)
    }
  }
  list(A_ineq = A_ineq, b_ineq = b_ineq)
}

ezdyn_validate_policy_solution <- function(constraints, dY, Y0, shocks) {
  if (length(constraints) == 0) return(invisible(NULL))
  values <- Y0 + ezdyn_apply_policy_shocks(dY, shocks)
  for (constraint in constraints) {
    periods <- constraint$period[constraint$period <= ncol(values)]
    if (isTRUE(constraint$historical) || length(periods) == 0) next
    constrained <- values[constraint$variable, periods]
    valid <- if (constraint$direction == "lower") {
      constrained >= constraint$value - 1e-5
    } else {
      constrained <= constraint$value + 1e-5
    }
    if (!all(valid)) {
      stop(sprintf("Policy solution violates the %s bound on %s.", constraint$direction, constraint$variable), call. = FALSE)
    }
  }
  invisible(NULL)
}

ezdyn_policy_quadratic <- function(H, f, constraints) {
  ezdyn_policy_constrained_solution(H, f, constraints)$shocks
}

ezdyn_policy_constrained_solution <- function(H, f, constraints) {
  if (nrow(constraints$A_ineq) == 0) {
    return(list(shocks = ezdyn_policy_solve(H, f), multiplier = numeric()))
  }
  if (!requireNamespace("quadprog", quietly = TRUE)) {
    stop("Constrained policy requires the `quadprog` package.", call. = FALSE)
  }
  solution <- quadprog::solve.QP(H + diag(1e-10, nrow(H)), -f, -t(constraints$A_ineq), -constraints$b_ineq)
  list(shocks = as.numeric(solution$solution), multiplier = solution$Lagrangian)
}

ezdyn_policy_commit <- function(Y0, dY, weights, discount_factor, constraints) {
  n_loss <- nrow(Y0)
  horizon <- ncol(Y0)
  response <- matrix(dY, nrow = n_loss * horizon, ncol = dim(dY)[3])
  W <- diag(as.vector(weights %o% discount_factor^(0:(horizon - 1))))
  H <- 2 * crossprod(response, W %*% response)
  f <- 2 * crossprod(response, W %*% as.vector(Y0))
  ezdyn_policy_quadratic((H + t(H)) / 2, f, constraints)
}

ezdyn_policy_discretion_constraints <- function(constraints, dY) {
  horizon <- dim(dY)[2]
  C_ineq <- vector("list", horizon)
  b_ineq <- vector("list", horizon)
  for (constraint in constraints) {
    index <- match(constraint$variable, dimnames(dY)[[1]])
    if (is.na(index) || !(constraint$direction %in% c("lower", "upper"))) {
      stop("Policy constraints require an available variable and `lower` or `upper` direction.", call. = FALSE)
    }
    for (period in constraint$period[constraint$period <= horizon]) {
      C <- numeric(dim(dY)[1])
      C[[index]] <- if (constraint$direction == "lower") -1 else 1
      C_ineq[[period]] <- rbind(C_ineq[[period]], C)
      b_ineq[[period]] <- c(
        b_ineq[[period]],
        if (constraint$direction == "lower") -constraint$value else constraint$value
      )
    }
  }
  list(C_ineq = C_ineq, b_ineq = b_ineq)
}

ezdyn_policy_discretion <- function(Y0, dY, weights, discount_factor, constraints) {
  n_variables <- dim(dY)[1]
  horizon <- dim(dY)[2]
  instruments <- dim(dY)[3]
  if (instruments != horizon) {
    stop("Discretionary policy requires equal loss and instrument horizons.", call. = FALSE)
  }
  response_long <- matrix(dY, nrow = n_variables * horizon, ncol = instruments)
  C_long <- matrix(0, 0, n_variables * horizon)
  b_long <- numeric()
  constraint_periods <- integer()
  for (period in seq_len(horizon)) {
    C <- constraints$C_ineq[[period]]
    if (!is.null(C)) {
      expanded <- matrix(0, nrow(C), n_variables * horizon)
      expanded[, ((period - 1) * n_variables + 1):(period * n_variables)] <- C
      C_long <- rbind(C_long, expanded)
      b_long <- c(b_long, constraints$b_ineq[[period]])
      constraint_periods <- c(constraint_periods, rep(period, nrow(C)))
    }
  }
  active <- as.numeric(C_long %*% as.vector(Y0)) >= b_long - 1e-8

  for (iteration in seq_len(200)) {
    policy_conditions <- matrix(0, instruments, n_variables * horizon)
    constants <- numeric(instruments)
    assigned <- integer(length(b_long))
    discretionary_response <- vector("list", instruments)

    for (period in rev(seq_len(instruments))) {
      remaining_horizon <- horizon - period + 1
      effects <- ((period - 1) * n_variables + 1):(n_variables * horizon)
      response <- matrix(
        dY[, seq_len(remaining_horizon), seq_len(remaining_horizon), drop = FALSE],
        nrow = n_variables * remaining_horizon,
        ncol = remaining_horizon
      )
      current_response <- response[, 1, drop = FALSE]
      if (period == instruments) {
        discretionary_response[[period]] <- current_response
      } else {
        future_conditions <- policy_conditions[(period + 1):instruments, effects, drop = FALSE]
        future_response <- response[, -1, drop = FALSE]
        adjustment <- solve(future_conditions %*% future_response, -(future_conditions %*% current_response))
        discretionary_response[[period]] <- current_response + future_response %*% adjustment
      }

      eligible <- which(constraint_periods >= period & assigned == 0)
      affected <- eligible[abs(C_long[eligible, effects, drop = FALSE] %*% discretionary_response[[period]]) > 1e-8]
      assigned[affected] <- period
      binding <- which(active & assigned == period)
      if (length(binding) > 1) {
        ezdyn_policy_discretion_failure("More than one constraint binds the same discretionary policy decision.")
      }
      if (length(binding) == 1) {
        policy_conditions[period, effects] <- C_long[binding, effects]
        constants[[period]] <- b_long[[binding]]
      } else {
        W <- kronecker(diag(discount_factor^(0:(remaining_horizon - 1))), diag(weights))
        policy_conditions[period, effects] <- t(discretionary_response[[period]]) %*% W
      }
    }

    shocks <- as.numeric(solve(policy_conditions %*% response_long, constants - policy_conditions %*% as.vector(Y0)))
    values <- as.vector(Y0) + as.numeric(response_long %*% shocks)
    inequality <- as.numeric(C_long %*% values - b_long)
    multipliers <- numeric(length(b_long))
    for (index in which(active)) {
      period <- assigned[[index]]
      effects <- ((period - 1) * n_variables + 1):(n_variables * horizon)
      W <- kronecker(diag(discount_factor^(0:(horizon - period))), diag(weights))
      multipliers[[index]] <- -drop(t(discretionary_response[[period]]) %*% W %*% values[effects]) /
        drop(C_long[index, effects] %*% discretionary_response[[period]])
    }
    # Violations below 1e-5 are numerical noise; a tighter tolerance lets a
    # near-degenerate constraint switch on and off indefinitely.
    if (all(inequality <= 1e-5) && all(multipliers[active] >= -1e-8)) return(shocks)

    active_old <- active
    active[inequality > 1e-5] <- TRUE
    active[active & multipliers < -1e-8] <- FALSE
    if (identical(active, active_old)) {
      ezdyn_policy_discretion_failure("Discretionary constraint active set cannot resolve the solution.")
    }
  }
  ezdyn_policy_discretion_failure("Discretionary constraint active set did not converge.")
}

# Signal that no discretionary solution was found, with a class so callers can
# omit just that strategy rather than abort the whole calculation.
ezdyn_policy_discretion_failure <- function(message) {
  stop(structure(
    class = c("ezdyn_policy_discretion_failure", "error", "condition"),
    list(message = message, call = NULL)
  ))
}

ezdyn_policy_history <- function(vintages, variables, preloss_start, forecast_start, horizon, current) {
  if (is.null(vintages)) stop("Timeless commitment requires loss-variable vintages.", call. = FALSE)
  required <- c("date", "vintage", variables)
  if (!all(required %in% names(vintages))) stop("Loss-variable vintages are incomplete.", call. = FALSE)
  vintage_dates <- seq(as.Date(preloss_start), as.Date(forecast_start), by = "quarter")
  dates <- seq(as.Date(preloss_start), by = "quarter", length.out = length(vintage_dates) - 1 + horizon)
  out <- array(NA_real_, c(length(variables), length(dates), length(vintage_dates)), list(variables, as.character(dates), as.character(vintage_dates)))
  for (i in seq_len(length(vintage_dates) - 1)) {
    slice <- vintages[
      vintages$vintage == vintage_dates[[i]] & as.Date(vintages$date) %in% dates,
      c("date", variables),
      drop = FALSE
    ]
    positions <- match(as.Date(slice$date), dates)
    out[, positions, i] <- t(as.matrix(slice[, variables, drop = FALSE]))
    if (anyNA(out[, i:length(dates), i])) stop("Loss-variable vintages are incomplete over the required horizon.", call. = FALSE)
  }
  start <- length(vintage_dates)
  out[, start:(start + horizon - 1), length(vintage_dates)] <- current
  out
}

ezdyn_policy_timeless_constraints <- function(constraints, dY, Y0, vintages, preloss_start, forecast_start, horizon, instrument_variable) {
  preloss <- length(seq(as.Date(preloss_start), as.Date(forecast_start), by = "quarter")) - 1L
  instrument_history <- ezdyn_policy_history(
    vintages, instrument_variable, preloss_start, forecast_start, horizon,
    Y0[instrument_variable, , drop = FALSE]
  )
  historical <- Filter(function(x) isTRUE(x$historical), constraints)
  current <- Filter(function(x) !isTRUE(x$historical), constraints)
  lapply(seq_len(preloss + 1L), function(period) {
    remaining <- dim(dY)[3] - period + 1L
    historical_period <- lapply(historical, function(x) { x$period <- seq_len(remaining); x })
    current_data <- if (period == preloss + 1L) ezdyn_policy_constraints(current, dY[, seq_len(horizon), seq_len(dim(dY)[3] - preloss), drop = FALSE], Y0) else list(A_ineq = matrix(0, 0, remaining), b_ineq = numeric())
    history_data <- ezdyn_policy_constraints(
      historical_period,
      dY[instrument_variable, seq_len(remaining), seq_len(remaining), drop = FALSE],
      matrix(
        instrument_history[instrument_variable, period:(period + remaining - 1), period],
        nrow = 1,
        dimnames = list(instrument_variable, NULL)
      )
    )
    list(A_ineq = rbind(history_data$A_ineq, current_data$A_ineq), b_ineq = c(history_data$b_ineq, current_data$b_ineq))
  })
}

ezdyn_policy_timeless <- function(history, dY, weights, discount_factor, constraints) {
  preloss <- dim(history)[3] - 1L
  n_loss <- dim(history)[1]
  horizon <- dim(history)[2] - preloss
  instruments <- dim(dY)[3] - preloss
  expectation_lagged <- rep(0, n_loss * (preloss + horizon))
  multiplier_lagged <- rep(0, preloss + instruments)
  for (period in seq_len(preloss + 1L)) {
    loss_horizon <- preloss + horizon - period + 1L
    instrument_horizon <- preloss + instruments - period + 1L
    Y0 <- as.vector(history[, period:dim(history)[2], period, drop = FALSE][, , 1])
    response <- matrix(
      dY[, seq_len(loss_horizon), seq_len(instrument_horizon), drop = FALSE],
      nrow = n_loss * loss_horizon,
      ncol = instrument_horizon
    )
    W <- diag(as.vector(weights %o% discount_factor^(0:(loss_horizon - 1L))))
    H <- crossprod(response, W %*% response)
    f <- crossprod(response, W %*% (Y0 - expectation_lagged)) + multiplier_lagged
    solution <- ezdyn_policy_constrained_solution((H + t(H)) / 2, f, constraints[[period]])
    shocks <- solution$shocks
    expectation <- Y0 + as.numeric(response %*% shocks)
    expectation_lagged <- expectation[(n_loss + 1):length(expectation)]
    multiplier <- -crossprod(constraints[[period]]$A_ineq, solution$multiplier)
    multiplier_lagged <- multiplier[-1] / discount_factor
  }
  shocks
}

# Solve H %*% shocks = -f. A shock with no effect on the loss (e.g. in the last
# quarter, when the loss variables only respond with a lag) leaves H singular;
# such shocks are left at zero.
ezdyn_policy_solve <- function(H, f) {
  as.numeric(tryCatch(solve(H, -f), error = function(...) {
    shocks <- qr.coef(qr(H), -f)
    shocks[is.na(shocks)] <- 0
    shocks
  }))
}
