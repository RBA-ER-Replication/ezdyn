# get_irfs_multi.R
# Multi-model IRF helpers: get_irfs(), get_irfs_target(), get_irfs_target_gabaix().
# Each loops the corresponding single-model function (get_irf(), get_irf_target(),
# get_irf_target_gabaix()) over every model in a named list and combines the
# labelled pretty output, so cross-model IRF comparisons need one call instead
# of one call per model plus a manual rbind(). See README.md.

# Loop `compute(model, model_name)` over every model, tag the result with the
# model's list name (not necessarily M_$model_name - lets the same model be
# compared twice under different labels, matching get_alt_paths()'s
# convention), attribute errors to the failing model, and combine into one
# data frame. Shared by the multi-model IRF functions below and by the
# dashboard's Custom IRF adapters (irf_run_targeted()/irf_run_direct() in
# dash_irf_custom_helpers.R) - the one canonical "loop over named models,
# compute, attribute errors, combine" implementation.
ezdyn_combine_models_dfr <- function(models, compute, context) {
    purrr::imap_dfr(models, function(model, model_name) {
        tryCatch(
            # `.env$model_name` (rather than bare `model_name`) is required
            # here: `compute()` output already carries its own `model_name`
            # column (stamped by `pretty_irf()` from `M_$model_name`), so a
            # bare `model_name` on the right-hand side would resolve to that
            # existing data column instead of this function's argument.
            compute(model, model_name) |> dplyr::mutate(model_name = .env$model_name),
            error = function(condition) {
                stop(
                    sprintf("%s failed for model `%s`: %s", context, model_name, condition$message),
                    call. = FALSE
                )
            }
        )
    })
}

# Same error attribution as `ezdyn_combine_models_dfr()`, but returns a named
# list (one entry per model) instead of combining rows - used when a
# multi-model IRF function is called with `pretty = FALSE`, where per-model
# results (arrays/matrices) are not a common shape to bind together.
ezdyn_map_models <- function(models, compute, context) {
    purrr::imap(models, function(model, model_name) {
        tryCatch(
            compute(model, model_name),
            error = function(condition) {
                stop(
                    sprintf("%s failed for model `%s`: %s", context, model_name, condition$message),
                    call. = FALSE
                )
            }
        )
    })
}

#' Get impulse responses to given shocks for multiple models at once.
#'
#' Loops [get_irf()] over every model in `models` and combines the labelled
#' output, so cross-model IRF comparisons need one call instead of one call
#' per model plus a manual `rbind()`.
#'
#' @param models Named list of full model MOO pairs (see [get_alt_paths()]).
#' @param shock_names Character vector of shock codes or descriptions, as in
#'   [get_irf()]. Each is resolved independently for every model using that
#'   model's own shock metadata, so the same call works whether every model
#'   shares a shock description or each has its own code for the same shock.
#' @param horizon Horizon to calculate impulse responses over.
#' @param pretty Logical. If `TRUE` (default), returns one combined pretty
#'   data frame with a `model_name` column identifying each model by its name
#'   in `models`. If `FALSE`, returns a named list of each model's own
#'   [get_irf()] result (not combined, since raw results are not a common
#'   shape across models).
#' @param data_frame Logical. If `TRUE`, each model's own non-pretty result is
#'   a data frame instead of an array. Ignored when `pretty = TRUE`.
#'
#' @return A combined pretty data frame (`pretty = TRUE`) or a named list of
#'   per-model results (`pretty = FALSE`).
#' @export
#' @examples
#' \dontrun{
#' get_irfs(models, shock_names = "Monetary policy shock", horizon = 16)
#' }
get_irfs <- function(models, shock_names, horizon, pretty = TRUE, data_frame = pretty) {
    ezdyn_validate_baseline_models(models)
    compute <- function(model, model_name) {
        get_irf(model$M_, model$oo_, shock_names, horizon, pretty = pretty, data_frame = data_frame)
    }
    if (pretty) {
        ezdyn_combine_models_dfr(models, compute, context = "get_irfs()")
    } else {
        ezdyn_map_models(models, compute, context = "get_irfs()")
    }
}

#' Get targeted impulse responses for multiple models at once.
#'
#' Loops [get_irf_target()] over every model in `models` and combines the
#' labelled output, so cross-model comparisons need one call instead of one
#' call per model plus a manual `rbind()`. Named `get_irfs_target()` (rather
#' than `get_irf_targets()`) to keep the multi-model prefix consistent with
#' [get_irfs()] and [get_irfs_target_gabaix()]: `get_irfs_*` always loops the
#' single-model `get_irf*` function of the same name.
#'
#' @param models Named list of full model MOO pairs (see [get_alt_paths()]).
#' @param horizon Horizon to calculate impulse responses over.
#' @param target Named list of target paths, as in [get_irf_target()]. Names
#'   are resolved independently for every model using that model's own
#'   variable metadata.
#' @param shock_timing Named list mapping shocks to the periods they are
#'   active, as in [get_irf_target()]. Names are resolved independently for
#'   every model using that model's own shock metadata.
#' @param pretty Logical. If `TRUE` (default), returns one combined pretty
#'   data frame with a `model_name` column identifying each model by its name
#'   in `models`. If `FALSE`, returns a named list of each model's own
#'   [get_irf_target()] result (not combined).
#' @param shock_nickname Optional nickname for the shock, used only if
#'   `pretty = TRUE`.
#' @param scale_targets Logical, as in [get_irf_target()].
#' @param cache Logical. If `TRUE`, read/write each model's underlying IRF
#'   from/to its own `oo_$.irf_cache` (see `ezdyn_irf_cache()`).
#'
#' @return A combined pretty data frame (`pretty = TRUE`) or a named list of
#'   per-model results (`pretty = FALSE`).
#' @export
#' @examples
#' \dontrun{
#' target <- list("Cash Rate" = c(1, 1.25))
#' shock_timing <- list("Monetary policy shock" = 1:2)
#' get_irfs_target(
#'   models,
#'   horizon = 20, target = target, shock_timing = shock_timing,
#'   shock_nickname = "Cash-rate hike"
#' )
#' }
get_irfs_target <- function(models, horizon, target, shock_timing, pretty = TRUE,
                             shock_nickname = "", scale_targets = TRUE, cache = FALSE) {
    ezdyn_validate_baseline_models(models)
    compute <- function(model, model_name) {
        get_irf_target(
            model$M_, model$oo_, horizon, target, shock_timing,
            pretty = pretty, shock_nickname = shock_nickname,
            scale_targets = scale_targets, cache = cache
        )
    }
    if (pretty) {
        ezdyn_combine_models_dfr(models, compute, context = "get_irfs_target()")
    } else {
        ezdyn_map_models(models, compute, context = "get_irfs_target()")
    }
}

#' Get targeted impulse responses with partially anticipated shocks for
#' multiple models at once.
#'
#' Loops [get_irf_target_gabaix()] over every model in `models` and combines
#' the labelled output. Unlike [get_alt_paths()], this is a pure loop over the
#' existing single-model function: a model that cannot supply the anticipated
#' shock codes needed for `shock_timing` errors for that model (identified in
#' the error message), rather than silently falling back to an unanticipated
#' IRF.
#'
#' @inheritParams get_irfs_target
#' @param lambda Discount factor for partially anticipated shocks (0-1), as in
#'   [get_irf_target_gabaix()].
#'
#' @return A combined pretty data frame (`pretty = TRUE`, including
#'   `gabaix_lambda`) or a named list of per-model results (`pretty = FALSE`).
#' @export
#' @examples
#' \dontrun{
#' target <- list("Cash Rate" = c(1, 1.25))
#' shock_timing <- list("Monetary policy shock" = 1:2)
#' get_irfs_target_gabaix(
#'   models,
#'   horizon = 20, target = target, shock_timing = shock_timing, lambda = 0.8
#' )
#' }
get_irfs_target_gabaix <- function(models, horizon, target, shock_timing, lambda,
                                    pretty = TRUE, shock_nickname = "",
                                    scale_targets = TRUE, cache = FALSE) {
    ezdyn_validate_baseline_models(models)
    compute <- function(model, model_name) {
        get_irf_target_gabaix(
            model$M_, model$oo_, horizon, target, shock_timing, lambda,
            pretty = pretty, shock_nickname = shock_nickname,
            scale_targets = scale_targets, cache = cache
        )
    }
    if (pretty) {
        ezdyn_combine_models_dfr(models, compute, context = "get_irfs_target_gabaix()")
    } else {
        ezdyn_map_models(models, compute, context = "get_irfs_target_gabaix()")
    }
}
