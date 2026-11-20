# Optimal policy scenarios ---------------------------------------------------

#' Run one optimal-policy scenario for one model
#'
#' `get_policy_scenario()` implements the sufficient-statistics approach to
#' optimal policy: given a quadratic loss function, a baseline forecast, and
#' a model's policy impulse responses, it solves for the optimal path of the
#' policy instrument (and the implications for every other output variable)
#' under commitment or discretion, with optional constraints.
#'
#' ```r
#' strategy <- list(
#'   output_name      = "Optimal (ModelA, commitment)",
#'   output_variables = c("Cash Rate", "Inflation", "Unemployment"),
#'   loss_variables   = c("Inflation", "Unemployment", "Cash Rate"),
#'   loss_weights     = c(1, 1, 0.5),
#'   discount_factor  = 0.99,
#'   T_loss           = 20,
#'   T_instrument     = 20,
#'   commit           = "commit"
#' )
#'
#' optimal <- get_policy_scenario(strategy, baseline = baseline, model = models$ModelA,
#'                                 forecast_start = "2026-09-01",
#'                                 forecast_end   = "2031-06-01")
#'
#' plot_pretty(optimal,
#'             display_names = c("Cash Rate", "Inflation", "Unemployment"))$graph
#' ```
#' The baseline must be constructed first (e.g. with [import_baseline()]).
#' `T_loss` is the number of forecast quarters over which the loss is
#' counted; `T_instrument` is the number of quarters of policy shocks chosen
#' (i.e. over which policy is optimised). Both must lie between 1 and the
#' number of quarters from `forecast_start` to `forecast_end`. Under
#' discretion, `T_loss` must also be at least `T_instrument`: policy is re-set
#' in each instrument quarter to minimise the loss from then on, so every
#' instrument quarter must fall within the loss horizon.
#'
#' `commit` selects the strategy: `"disc"` for discretion, or one of two
#' forms of commitment - `"commit"`, in which commitment starts today, and
#' `"timeless"`, in which commitment is assumed to have been in place since
#' some past date, `preloss_start` (see below). Loss variables are
#' penalised as deviations from zero; to target a non-zero value, construct
#' a gap variable by subtracting the target from the actual variable, add it
#' to the baseline (and vintages), e.g. `Inflation_gap_loss`, and use it as
#' the loss variable.
#'
#' For a model whose policy shocks are all unanticipated - because
#' `use_cd = FALSE`, `lambda = 0`, or the model has no anticipated shocks
#' (`<shock>_1`, `<shock>_2`, ...) - commitment and discretion coincide. An
#' error is raised if the model's impulse responses or anticipated shocks
#' do not cover the required horizon (including any commitment history).
#'
#' See [get_policy_scenarios()] to run a list of strategies across a list of
#' models at once (as the dashboard does).
#'
#' `baseline` has a quarterly `date` column and includes the output, loss,
#' constraint, and instrument variables named in `policy_strategy`, each
#' column keyed by either `model`'s own dynare code or (as in a canonical
#' baseline returned by [import_baseline()]) its display name. The returned
#' paths are rescaled and labelled with model metadata.
#'
#' @section Commitment start and forecast vintages:
#' Under `"timeless"`, the problem is re-solved at each historical forecast
#' vintage from `preloss_start` to today, so it needs those forecasts, via
#' `loss_variable_vintages` and `preloss_start`:
#' ```r
#' strategy$commit <- "timeless"
#' optimal <- get_policy_scenario(strategy, baseline = baseline, model = models$ModelA,
#'                                 forecast_start = "2026-09-01",
#'                                 forecast_end   = "2031-06-01",
#'                                 loss_variable_vintages = vintages,
#'                                 preloss_start  = "2024-03-01")
#' ```
#' `loss_variable_vintages` is a long data frame with columns:
#' * `vintage`: the first forecast quarter of each historical forecast, one
#'   per quarter from `preloss_start` to the quarter before `forecast_start`;
#' * `date`: the quarter being forecast, from the vintage quarter to the end
#'   of today's loss horizon;
#' * one column per loss variable, named as in `loss_variables` - by display
#'   name (e.g. `Inflation_gap_loss`) or by the model's own variable code
#'   (e.g. `pi_obs_gap_loss`) - and already expressed as a deviation from
#'   target. Display names let one table serve every model; and
#' * only if a constraint is marked `historical = TRUE` (see below), the
#'   policy instrument's level, named like `instrument_variable` (e.g.
#'   `Cash Rate` or `r_obs`).
#'
#' Each vintage must cover every quarter from its own first forecast quarter
#' to the end of today's loss horizon, and today's forecast is taken from
#' `baseline`. Dates may be Dates, date-times (e.g. as read by
#' `readxl::read_excel()`), `YYYY-MM-DD` strings or `YYYYQq` strings, and any
#' day within a quarter stands for that quarter. Blank `vintage` or `date`
#' entries, or two rows for the same vintage and quarter, are errors.
#' Vintages outside `preloss_start` to the quarter before `forecast_start`,
#' and quarters after the loss horizon, are ignored. When `preloss_start` is
#' `forecast_start` (the default), there are no past vintages, so
#' `loss_variable_vintages` isn't needed.
#'
#' @section Constraints:
#' An optional `constraints` entry in `policy_strategy` is a list of
#' bounds, each a list with `variable`, `direction` (`"lower"` or
#' `"upper"`), `value`, and `period` (forecast periods, with 1 the first
#' forecast quarter; periods beyond `T_loss` are ignored), e.g.
#' `list(variable = "Cash Rate", direction = "lower", value = 0.1, period = 1:20)`.
#' Under `"timeless"`, a bound on the policy instrument can also be marked
#' `historical = TRUE`, so it is assumed to have been in place in every
#' historical vintage as well, generating "make-up" policy today via the
#' carried-forward Lagrange multipliers. This needs the instrument's level in
#' `loss_variable_vintages`.
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
#' @param loss_variable_vintages Historical forecasts for timeless commitment,
#'   as described in "Commitment start and forecast vintages". Only used when
#'   `commit = "timeless"` and `preloss_start` is before `forecast_start`.
#' @param preloss_start First historical commitment quarter for timeless
#'   policy, on or before `forecast_start`; any day in the quarter will do.
#'   `NULL` (default) means `forecast_start`, i.e. no commitment history.
#' @param policy_irfs Optional cached array returned by [build_policy_irfs()].
#'
#' @return A long data frame containing baseline and alternative paths.
#' @md
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
  if (commit == "disc") ezdyn_validate_discretion_horizons(T_loss, T_instrument)

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
  baseline_columns <- ezdyn_policy_columns(names(baseline), variables, originals_for, model$M_)

  missing_variables <- policy_variables[is.na(baseline_columns[policy_variables])]
  if (length(missing_variables) > 0) {
    stop(sprintf("Baseline is missing policy variable(s): %s.", paste(missing_variables, collapse = ", ")), call. = FALSE)
  }

  baseline$date <- as.Date(baseline$date)
  baseline <- ezdyn_policy_rename_columns(baseline, baseline_columns)
  baseline_forecast <- baseline[match(forecast_dates, baseline$date), variables, drop = FALSE]
  if (anyNA(baseline_forecast[, policy_variables, drop = FALSE])) {
    stop("Baseline must cover every forecast quarter and contain complete policy inputs.", call. = FALSE)
  }
  Y0 <- t(as.matrix(baseline_forecast))
  T_preloss <- 0L
  if (commit == "timeless") {
    preloss_start <- ezdyn_policy_preloss_start(preloss_start, forecast_start)
    T_preloss <- length(seq(preloss_start, as.Date(forecast_start), by = "quarter")) - 1L
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
    # Vintage columns are located the same way as baseline columns, then
    # renamed to the resolved identifiers used below. Commitment starting at
    # the forecast start has no past vintages, so they aren't needed.
    loss_variable_vintages <- if (T_preloss > 0L) ezdyn_policy_vintages(
      loss_variable_vintages, loss_variables, instrument_variable,
      any(vapply(constraints, function(x) isTRUE(x$historical), logical(1))),
      originals_for, model$M_
    )
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

#' Run optimal-policy scenarios for one or more models
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
#' Likewise, a strategy that fails for a model (e.g. timeless commitment with
#' incomplete vintages) is omitted with a warning giving the reason, and the
#' rest still run; only if every one fails is that an error.
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
#' @param loss_variable_vintages Past baselines for timeless commitment; see
#'   [get_policy_scenario()] for the format.
#' @param preloss_start First historical commitment quarter for timeless
#'   policy; `NULL` (default) means `forecast_start`.
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

  # A failing (model, strategy) pair is omitted, so the others still run. Its
  # message becomes a warning if anything else succeeds, or the error if
  # nothing does.
  failures <- character()
  succeeded <- FALSE
  compute <- function(model, model_name) {
    # Unanticipated policy shocks (no anticipated shocks, or no anticipation)
    # make timeless commitment and discretion meaningless, so each strategy
    # becomes the same plain commitment strategy, run once.
    model_strategies <- unique(lapply(strategies, function(strategy) {
      strategy$unanticipated <- !(use_cd && lambda != 0) || ezdyn_policy_anticipated_count(model, strategy) == 0
      if (strategy$unanticipated) strategy$commit <- "commit"
      strategy
    }))
    dplyr::bind_rows(lapply(model_strategies, function(strategy) {
      result <- tryCatch(
        ezdyn_policy_scenario_for_model(
          strategy, model, model_name, baseline, use_cd, lambda,
          forecast_start, forecast_end, loss_variable_vintages, preloss_start, irf_cache
        ),
        error = function(condition) {
          failures <<- c(failures, sprintf(
            "Omitted `%s` for model `%s`: %s",
            ezdyn_policy_output_name(strategy, model_name), model_name, conditionMessage(condition)
          ))
          NULL
        }
      )
      if (!is.null(result) && nrow(result) > 0) succeeded <<- TRUE
      result
    }))
  }

  out <- ezdyn_combine_models_dfr(models, compute, context = "get_policy_scenarios()")
  if (length(failures) > 0) {
    if (!succeeded) stop(paste(failures, collapse = "\n"), call. = FALSE)
    for (failure in failures) warning(failure, call. = FALSE)
  }
  out
}

# Label a strategy from the model's list name and commitment mode (just the
# model's name if its shocks are unanticipated).
ezdyn_policy_output_name <- function(strategy, model_name) {
  if (isTRUE(strategy$unanticipated)) {
    sprintf("Optimal (%s)", model_name)
  } else {
    sprintf("Optimal (%s, %s)", model_name, ezdyn_policy_mode_label(strategy$commit))
  }
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
  strategy$output_name <- ezdyn_policy_output_name(strategy, model_name)

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
  preloss <- if (identical(strategy$commit, "timeless")) {
    preloss_start <- ezdyn_policy_preloss_start(preloss_start, forecast_start)
    length(seq(preloss_start, as.Date(forecast_start), by = "quarter")) - 1L
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

# Stack constraints as rows of `A_ineq %*% shocks <= b_ineq`. `period` gives
# each row's quarter (1 = the first quarter of `dY`), which timeless
# commitment needs to tell past from future constraints.
ezdyn_policy_constraints <- function(constraints, dY, Y0) {
  A_ineq <- matrix(0, 0, dim(dY)[3])
  b_ineq <- numeric()
  row_period <- integer()
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
      row_period <- c(row_period, as.integer(period))
    }
  }
  list(A_ineq = A_ineq, b_ineq = b_ineq, period = row_period)
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

# Discretion re-optimises in each instrument period against the loss from that
# period on, so every instrument period must fall within the loss horizon. A
# longer loss horizon is fine: the last policymakers just look further ahead.
ezdyn_validate_discretion_horizons <- function(T_loss, T_instrument) {
  if (T_loss < T_instrument) {
    stop(sprintf(
      paste(
        "Discretion needs a loss horizon at least as long as the instrument (optimisation) horizon,",
        "but the loss horizon is %d and the instrument horizon is %d. A discretionary policymaker",
        "re-optimises each quarter against the loss from that quarter on, so the policy set in",
        "quarters %d to %d would have no loss left to minimise. Lengthen the loss horizon or",
        "shorten the instrument horizon."
      ),
      T_loss, T_instrument, T_loss + 1L, T_instrument
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# Solve discretionary policy by backward induction over the `instruments`
# policy periods. The policymaker in period p sets that period's instrument
# against the loss over periods p to `horizon`, taking the reactions of the
# later policymakers (p + 1 to `instruments`) as given. The loss horizon may
# exceed the instrument horizon.
ezdyn_policy_discretion <- function(Y0, dY, weights, discount_factor, constraints) {
  n_variables <- dim(dY)[1]
  horizon <- dim(dY)[2]
  instruments <- dim(dY)[3]
  ezdyn_validate_discretion_horizons(horizon, instruments)
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
      remaining_instruments <- instruments - period + 1
      effects <- ((period - 1) * n_variables + 1):(n_variables * horizon)
      response <- matrix(
        dY[, seq_len(remaining_horizon), seq_len(remaining_instruments), drop = FALSE],
        nrow = n_variables * remaining_horizon,
        ncol = remaining_instruments
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

# Locate each resolved policy variable's column among `columns`: the resolved
# identifier itself, any identifier it was originally supplied as
# (`originals_for`), or `M_`'s display name for it (`<display name>_gap_loss`
# for a gap column). Returns the matching column name per variable, or `NA`.
ezdyn_policy_columns <- function(columns, variables, originals_for, M_) {
  stats::setNames(vapply(variables, function(variable) {
    if (variable %in% columns) return(variable)
    base <- sub("_gap_loss$", "", variable)
    shown <- display_name(base, M_)
    if (base != variable) shown <- paste0(shown, "_gap_loss")
    candidates <- unique(c(originals_for[[variable]], shown))
    found <- candidates[candidates %in% columns]
    if (length(found) > 0) return(found[[1]])
    NA_character_
  }, character(1)), variables)
}

# Rename the columns located by `ezdyn_policy_columns()` to their resolved
# identifiers.
ezdyn_policy_rename_columns <- function(data, columns) {
  found <- columns[!is.na(columns) & columns != names(columns)]
  names(data)[match(found, names(data))] <- names(found)
  data
}

# Check and prepare timeless-commitment vintages: locate one
# `<loss variable>_gap_loss` column per loss variable (and the instrument's
# level when a historical constraint needs it), named as in the strategy or
# by display name, and rename them to the resolved identifiers.
ezdyn_policy_vintages <- function(vintages, loss_variables, instrument_variable, historical, originals_for, M_) {
  # Name each column as the user supplied the variable, e.g. a display name.
  expected <- function(variables) {
    vapply(variables, function(variable) {
      original <- originals_for[[variable]]
      if (length(original) > 0) original[[1]] else variable
    }, character(1), USE.NAMES = FALSE)
  }
  if (is.null(vintages)) {
    ezdyn_vintage_stop(sprintf(
      "Timeless commitment needs loss-variable vintages (`loss_variable_vintages`): a data frame with `vintage` and `date` columns and the column(s) %s.",
      paste0("`", expected(loss_variables), "`", collapse = ", ")
    ))
  }
  vintages <- ezdyn_check_vintage_keys(vintages)
  needed <- loss_variables
  if (historical) needed <- unique(c(needed, instrument_variable))
  columns <- ezdyn_policy_columns(names(vintages), needed, originals_for, M_)
  missing <- needed[is.na(columns)]
  if (length(missing) > 0) {
    missing_gaps <- intersect(missing, loss_variables)
    problems <- c(
      if (length(missing_gaps) > 0) sprintf(
        "%s (one `<loss variable>_gap_loss` column per loss variable: the gap from its target at the time)",
        paste0("`", expected(missing_gaps), "`", collapse = ", ")
      ),
      if (instrument_variable %in% missing) sprintf(
        "`%s` (the instrument's level, for the historical ELB/bound constraint)",
        expected(instrument_variable)
      )
    )
    ezdyn_vintage_stop(sprintf(
      "Loss-variable vintages are missing column(s) needed for timeless commitment: %s.",
      paste(problems, collapse = "; and ")
    ))
  }
  not_numeric <- columns[!vapply(vintages[columns], is.numeric, logical(1))]
  if (length(not_numeric) > 0) {
    ezdyn_vintage_stop(sprintf(
      "Loss-variable vintage column(s) %s must be numeric.",
      paste0("`", not_numeric, "`", collapse = ", ")
    ))
  }
  vintages <- ezdyn_policy_rename_columns(vintages, columns)
  # Keep the supplied column names for error messages.
  attr(vintages, "ezdyn_columns") <- columns
  vintages
}

# Raise a vintage problem with its own condition class, so callers can tell
# it apart from other failures.
ezdyn_vintage_stop <- function(message) {
  stop(structure(
    class = c("ezdyn_policy_vintage_error", "error", "condition"),
    list(message = message, call = NULL)
  ))
}

# List up to `max` items, then say how many more there are.
ezdyn_format_some <- function(items, max = 10L, sep = ", ") {
  shown <- paste(utils::head(items, max), collapse = sep)
  if (length(items) > max) shown <- sprintf("%s%sand %d more", shown, sep, length(items) - max)
  shown
}

# Convert dates to the package's quarter convention: the first day of the
# quarter's last month, so any day in April to June becomes 1 June. Accepts
# Dates, POSIXct (by its own wall-clock date, so the time zone cannot shift
# the day), `YYYY-MM-DD` strings and `YYYYQq` strings. Missing or blank
# values stay NA.
ezdyn_as_quarter_dates <- function(values, what) {
  if (inherits(values, "Date")) {
    dates <- values
  } else if (inherits(values, "POSIXt")) {
    dates <- as.Date(format(values, "%Y-%m-%d"))
  } else if (is.character(values) || is.factor(values)) {
    strings <- trimws(as.character(values))
    strings[!is.na(strings) & strings == ""] <- NA_character_
    dates <- rep(as.Date(NA), length(strings))
    iso <- !is.na(strings) & grepl("^\\d{4}-\\d{2}-\\d{2}$", strings)
    dates[iso] <- as.Date(strings[iso], format = "%Y-%m-%d")
    quarter <- !is.na(strings) & grepl("^\\d{4}[Qq][1-4]$", strings)
    dates[quarter] <- as.Date(sprintf(
      "%s-%02d-01", substr(strings[quarter], 1, 4), 3L * as.integer(substr(strings[quarter], 6, 6))
    ))
    bad <- which(!is.na(strings) & is.na(dates))
    if (length(bad) > 0) {
      ezdyn_vintage_stop(sprintf(
        "%s has dates that can't be read, e.g. \"%s\" in row %d. Use dates, `YYYY-MM-DD` strings or `YYYYQq` strings.",
        what, strings[[bad[[1]]]], bad[[1]]
      ))
    }
  } else if (all(is.na(values))) {
    dates <- rep(as.Date(NA), length(values))
  } else {
    ezdyn_vintage_stop(sprintf(
      "%s must hold dates, `YYYY-MM-DD` strings or `YYYYQq` strings, not %s values%s.",
      what, class(values)[[1]],
      if (is.numeric(values)) " (e.g. Excel date serial numbers: format the cells as dates)" else ""
    ))
  }
  out <- rep(as.Date(NA), length(dates))
  known <- !is.na(dates)
  out[known] <- start_of_month(end_of_quarter(dates[known]))
  out
}

# Check the `vintage` and `date` keys of loss-variable vintages: present,
# readable as dates (snapped to the quarter grid), never blank, and unique.
# Returns a data frame with clean Date keys.
ezdyn_check_vintage_keys <- function(vintages) {
  if (!is.data.frame(vintages)) {
    ezdyn_vintage_stop("Loss-variable vintages must be a data frame with `vintage` and `date` columns.")
  }
  missing_keys <- setdiff(c("vintage", "date"), names(vintages))
  if (length(missing_keys) > 0) {
    ezdyn_vintage_stop(sprintf(
      "Loss-variable vintages are missing the %s column(s).",
      paste0("`", missing_keys, "`", collapse = ", ")
    ))
  }
  vintages <- as.data.frame(vintages, check.names = FALSE)
  for (key in c("vintage", "date")) {
    vintages[[key]] <- ezdyn_as_quarter_dates(vintages[[key]], sprintf("The vintages' `%s` column", key))
    blank <- which(is.na(vintages[[key]]))
    if (length(blank) > 0) {
      ezdyn_vintage_stop(sprintf(
        "The vintages' `%s` column is blank in row(s) %s.", key, ezdyn_format_some(blank)
      ))
    }
  }
  keys <- paste(format(vintages$vintage), format(vintages$date))
  repeated <- unique(keys[duplicated(keys)])
  if (length(repeated) > 0) {
    pairs <- vapply(repeated, function(key) {
      rows <- which(keys == key)
      sprintf("vintage %s, date %s (rows %s)", format(vintages$vintage[[rows[[1]]]]), format(vintages$date[[rows[[1]]]]), paste(rows, collapse = ", "))
    }, character(1))
    ezdyn_vintage_stop(sprintf(
      "Loss-variable vintages have more than one row for %s. Each vintage needs one row per quarter (dates in the same quarter count as the same quarter).",
      ezdyn_format_some(pairs, max = 5L, sep = "; ")
    ))
  }
  vintages
}

# Shorten runs of consecutive quarters, e.g. "2026-03-01 to 2026-09-01".
ezdyn_format_quarter_runs <- function(dates) {
  dates <- sort(unique(dates))
  index <- as.integer(format(dates, "%Y")) * 4L + (as.integer(format(dates, "%m")) - 1L) %/% 3L
  run <- cumsum(c(TRUE, diff(index) != 1L))
  runs <- vapply(split(dates, run), function(d) {
    if (length(d) == 1) format(d[[1]]) else paste(format(d[[1]]), "to", format(d[[length(d)]]))
  }, character(1))
  paste(runs, collapse = ", ")
}

# The commitment start for timeless commitment: `forecast_start` if NULL,
# otherwise snapped to the quarter grid, and never after `forecast_start`.
ezdyn_policy_preloss_start <- function(preloss_start, forecast_start) {
  forecast_start <- as.Date(forecast_start)
  if (is.null(preloss_start)) return(forecast_start)
  if (length(preloss_start) != 1) stop("`preloss_start` must be one date.", call. = FALSE)
  preloss_start <- ezdyn_as_quarter_dates(preloss_start, "`preloss_start`")
  if (is.na(preloss_start)) stop("`preloss_start` must be one date.", call. = FALSE)
  if (preloss_start > forecast_start) {
    stop(sprintf(
      "`preloss_start` (%s) is after `forecast_start` (%s): commitment has to start on or before the forecast start.",
      format(preloss_start), format(forecast_start)
    ), call. = FALSE)
  }
  preloss_start
}

# The forecasts made at each past vintage from `preloss_start` to the
# quarter before `forecast_start`, then the current baseline (`current`):
# an array of variables x dates x vintages. Vintages are only read when
# `preloss_start` is before `forecast_start`; every gap is reported at once.
ezdyn_policy_history <- function(vintages, variables, preloss_start, forecast_start, horizon, current) {
  vintage_dates <- seq(as.Date(preloss_start), as.Date(forecast_start), by = "quarter")
  dates <- seq(as.Date(preloss_start), by = "quarter", length.out = length(vintage_dates) - 1 + horizon)
  out <- array(NA_real_, c(length(variables), length(dates), length(vintage_dates)), list(variables, as.character(dates), as.character(vintage_dates)))
  past <- seq_len(length(vintage_dates) - 1)
  if (length(past) > 0) {
    if (is.null(vintages)) ezdyn_vintage_stop("Timeless commitment requires loss-variable vintages.")
    required <- c("date", "vintage", variables)
    if (!all(required %in% names(vintages))) {
      ezdyn_vintage_stop(sprintf(
        "Loss-variable vintages are missing column(s): %s.",
        paste0("`", setdiff(required, names(vintages)), "`", collapse = ", ")
      ))
    }
    labels <- attr(vintages, "ezdyn_columns")
    label <- function(variable) {
      if (!is.null(labels) && variable %in% names(labels) && !is.na(labels[[variable]])) labels[[variable]] else variable
    }
    problems <- character()
    for (i in past) {
      vintage <- format(vintage_dates[[i]])
      wanted <- dates[i:length(dates)]
      slice <- vintages[
        as.Date(vintages$vintage) == vintage_dates[[i]] & as.Date(vintages$date) %in% dates,
        c("date", variables),
        drop = FALSE
      ]
      if (nrow(slice) == 0) {
        problems <- c(problems, sprintf("vintage %s has no rows", vintage))
        next
      }
      positions <- match(as.Date(slice$date), dates)
      out[, positions, i] <- t(as.matrix(slice[, variables, drop = FALSE]))
      found <- wanted %in% as.Date(slice$date)
      if (any(!found)) {
        problems <- c(problems, sprintf("vintage %s has no row for %s", vintage, ezdyn_format_quarter_runs(wanted[!found])))
      }
      present <- wanted[found]
      for (variable in variables) {
        blank <- present[is.na(out[variable, as.character(present), i])]
        if (length(blank) > 0) {
          problems <- c(problems, sprintf(
            "`%s` in vintage %s is blank for %s", label(variable), vintage, ezdyn_format_quarter_runs(blank)
          ))
        }
      }
    }
    if (length(problems) > 0) {
      ezdyn_vintage_stop(sprintf(
        "Loss-variable vintages are incomplete: %s. Each vintage from %s to %s needs a row, with values in %s, for every quarter from its own quarter to %s.",
        ezdyn_format_some(problems, max = 8L, sep = "; "),
        format(vintage_dates[[1]]), format(vintage_dates[[length(past)]]),
        paste0("`", vapply(variables, label, character(1)), "`", collapse = ", "),
        format(dates[[length(dates)]])
      ))
    }
  }
  start <- length(vintage_dates)
  out[, start:(start + horizon - 1), length(vintage_dates)] <- current
  out
}

ezdyn_policy_timeless_constraints <- function(constraints, dY, Y0, vintages, preloss_start, forecast_start, horizon, instrument_variable) {
  preloss <- length(seq(as.Date(preloss_start), as.Date(forecast_start), by = "quarter")) - 1L
  historical <- Filter(function(x) isTRUE(x$historical), constraints)
  current <- Filter(function(x) !isTRUE(x$historical), constraints)
  # The instrument's history is only needed for historical constraints.
  instrument_history <- if (length(historical) > 0) {
    ezdyn_policy_history(
      vintages, instrument_variable, preloss_start, forecast_start, horizon,
      Y0[instrument_variable, , drop = FALSE]
    )
  }
  none <- function(remaining) list(A_ineq = matrix(0, 0, remaining), b_ineq = numeric(), period = integer())
  lapply(seq_len(preloss + 1L), function(period) {
    remaining <- dim(dY)[3] - period + 1L
    historical_period <- lapply(historical, function(x) { x$period <- seq_len(remaining); x })
    current_data <- if (period == preloss + 1L) ezdyn_policy_constraints(current, dY[, seq_len(horizon), seq_len(dim(dY)[3] - preloss), drop = FALSE], Y0) else none(remaining)
    history_data <- if (length(historical) == 0) none(remaining) else ezdyn_policy_constraints(
      historical_period,
      dY[instrument_variable, seq_len(remaining), seq_len(remaining), drop = FALSE],
      matrix(
        instrument_history[instrument_variable, period:(period + remaining - 1), period],
        nrow = 1,
        dimnames = list(instrument_variable, NULL)
      )
    )
    list(
      A_ineq = rbind(history_data$A_ineq, current_data$A_ineq),
      b_ineq = c(history_data$b_ineq, current_data$b_ineq),
      period = c(history_data$period, current_data$period)
    )
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
    # Carry forward only the multipliers on constraints dated after this
    # quarter. Those constraints reappear, with fresh multipliers, in the
    # next quarter's problem, so their old multipliers enter its first-order
    # condition with the opposite sign. This quarter's constraints are past
    # by then: their effect is already captured by measuring the next
    # quarter's loss against this plan's expected path (`expectation_lagged`),
    # so carrying them too would count them twice.
    future <- constraints[[period]]$period > 1L
    multiplier <- -crossprod(
      constraints[[period]]$A_ineq[future, , drop = FALSE],
      solution$multiplier[future]
    )
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
