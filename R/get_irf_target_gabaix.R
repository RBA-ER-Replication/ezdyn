#' Targeted IRFs with partially anticipated shocks (Gabaix-style adjustment).
#'
#' Modified version of `get_irf_target` that discounts future shocks using
#' the Haderer and Ryan (2025) approach. Note: shocks are discounted rather than
#' the state vector, so this is not strictly Gabaix discounting.
#'
#' @param M_ Dynare model structure containing model specifications and parameters.
#' @param oo_ Dynare output structure containing simulation results.
#' @param horizon Horizon to calculate impulse responses over.
#' @param target Named list of target paths with `NA` for unconstrained periods.
#'   Names can be variable 'display_name's from variable metadata or 'dynare_names'.
#' @param shock_timing Named list mapping shocks to the periods they are active.
#'   Names can be shock descriptions from shock metadata or the standard shock codes.
#' For example, `list("eps_r" = 1:4)` or `list("Cash rate shock" = 1:4)`, depending on the shock metadata.
#' @param lambda Discount factor for partially anticipated shocks (0-1).
#' @param pretty Logical. If `TRUE`, returns IRFs in pretty format with names/units.
#' @param shock_nickname Optional nickname for the shock, used only if `pretty=TRUE`.
#' @param scale_targets Logical. If `TRUE` (default), target values are assumed
#'   to be in displayed IRF units and are divided by each variable's numeric
#'   `scale_factor` before solving. If a scale factor is a function, this option
#'    is not usable and so an error is thrown; use `scale_targets=FALSE` to
#'    supply targets directly in model units. If FALSE, targets should be provided in
#'    'unscaled' units (i.e. the units used in the dynare object or input IRF).
#' @param cache Logical. If `TRUE`, read/write the underlying IRF from/to
#'   `oo_$.irf_cache` (see `ezdyn_irf_cache()`).
#'
#' @return An MxH matrix (response vars x horizon) or pretty data frame when
#'   `pretty=TRUE` (includes `gabaix_lambda`).
#' @export
#' @examples
#' \dontrun{
#' get_irf_target_gabaix(M_, oo_, 12, list(r_obs = rep(0.1, 6)), list(eps_r = 1:6), 0.8)
#' get_irf_target_gabaix(M_, oo_, 40, list("r_obs" = c(1,1,1,1,1)), list("eps_r"=1:5), 0.8)
#' get_irf_target_gabaix(M_, oo_, 40, list("r_obs" = c(0.1,0.2,1,2,3,4, rep(10, 36))), list("eps_r"=1:4), 0.8)
#' get_irf_target_gabaix(M_, oo_, 12, list("Cash Rate" = rep(0.1, 6)), list("Cash rate shock" = 1:6), 0.8)
#' }
#'
get_irf_target_gabaix <- function(M_, oo_, horizon,
                                  target, shock_timing, lambda, pretty=FALSE, shock_nickname="", scale_targets=TRUE, cache=FALSE) {
    target <- ezdyn_resolve_target_names(target, M_) |>
        ezdyn_descale_targets(M_, scale_targets=scale_targets)
    shock_timing <- ezdyn_resolve_shock_timing_names(shock_timing, M_)
    # Flatten the target matrix so that each row is a response variable for a
    # particular time period.
    target_flat <- target |>
        flatten_target()

    # Construct IRF
    t_max_target <- as.data.frame(target) |> 
        nrow()
    T_ <- max(c(t_max_target, horizon))
    T_shock <- max(unlist(shock_timing))
    shock_name <- names(shock_timing)

    Mh <- get_ir_matrix_gabaix(M_, oo_, T_, shock_name, T_shock, lambda, cache = cache)

    Mh_total <- Mh$Mh_total[drop=F,,,,unlist(shock_timing)]
    Mh_marginal <- Mh$Mh_marginal[drop=F,,,unlist(shock_timing),]

    # Flatten the impulse response matrix, and subset to variables we are calibrating against.
    # Each row is a response variable for a time period,
    # Each column is an individual shock (e.g. cash rate shock in period 3).
    # See `flatten_ir_matrix` docs for more detail.
    Mh_flat <- Mh_total |>
        flatten_ir_matrix(selected_resp_vars=names(target))
    target_nan_ix <- is.na(target_flat)
    if (sum(!target_nan_ix) != ncol(Mh_flat)) {
        stop("Mh_flat should be square. i.e. same number of shocks as target values.")
    }
    # Calculate shocks to hit target.
    shocks <- qr.solve(Mh_flat[which(!target_nan_ix),], target_flat[!target_nan_ix,])

    # Given these shocks, solve for the IRF of ALL endogenous variables.
    Mh_flat_all <- Mh_total |>
        flatten_ir_matrix()

    irf <- Mh_flat_all %*% shocks
    # Reshape output so rows are time and columns are endogenous variables.
    out <- irf |>
        matrix(nrow=dim(Mh_total)[[1]], ncol=dim(Mh_total)[[2]],
               dimnames = list("response_var" = dimnames(Mh_total)[[1]], "t" = dimnames(Mh_total)[[2]])) |>
        t()
    if (pretty) {
        out <- out |>
            as.data.frame() |>
            dplyr::mutate(t = 1:nrow(out), shock=shock_nickname) |>
            pretty_irf(M_) |>
            dplyr::mutate(gabaix_lambda=lambda)
    }
    out
}

