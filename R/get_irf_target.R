#' Targeted IRFs that hit user-supplied paths.
#'
#' Computes the shock sequence (given list of shocks with periods each is active)
#'  required to match target paths for selected variables, then returns the IRF for all endogenous variables.
#'
#' @param M_ Dynare model structure containing model specifications and parameters.
#' @param oo_ Dynare output structure containing simulation results.
#' @param horizon Horizon to calculate impulse responses over.
#' @param target Named list of target paths. Each entry is a numeric vector
#'   for the target variable. Names can be variable display names from
#'   Names can be variable 'display_name's from variable metadata or 'dynare_name's.
#' @param shock_timing Named list mapping shocks to the periods they are active,
#'   e.g. `list("eps_r" = 1:4)`. Names can be shock descriptions from
#'   `M_$shock_meta$description`; descriptions are matched before shock codes.
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
#'   `pretty=TRUE`.
#' @export
#' @examples
#' \dontrun{
#' get_irf_target(M_, oo_, 12, list(r_obs = rep(1, 1)), list(eps_r = 1), shock_nickname = "1 period cash rate shock")
#' get_irf_target(M_, oo_, 12, list(r_obs = rep(0.1, 6)), list(eps_r = 1:6))
#' get_irf_target(M_, oo_, 40, list("r_obs"=rep(1,10)), list("eps_r"=1:10))
#' get_irf_target(M_, oo_, 40, list("r_obs" = rep(3, 10)), list("eps_r"=rep(1:10)))
#' get_irf_target(M_, oo_, 40, list("r_obs" = c(0.1,0.2,1,2,3,4)), list("eps_r"=1:4))
#' get_irf_target(M_, oo_, 12, list("Cash Rate" = rep(0.1, 6)), list("Cash rate shock" = 1:6))
#' get_irf_target(M_, oo_, 12, list(r_obs = rep(0.001, 6)), list(eps_r = 1:6), scale_targets=FALSE)
#' }
get_irf_target <- function(M_, oo_, horizon, target, shock_timing, pretty=FALSE, shock_nickname="", scale_targets=TRUE, cache=FALSE) {
    # If target names are in display names, resolve to dynare names. 
    # Then if target variable has a scaling in the metadata, and scale_targets=TRUE,
    # descale targets so that they are in the units of the underlying variables in the oo_ object. 
    target <- ezdyn_resolve_target_names(target, M_) |>
        ezdyn_descale_targets(M_, scale_targets=scale_targets)
    # Same renaming procedure for shock names
    shock_timing <- ezdyn_resolve_shock_timing_names(shock_timing, M_)
    
    t_max_target <- as.data.frame(target) |> 
        nrow()
    T_ <- max(c(t_max_target, horizon))
    # Get impulse responses to every shock that occurs.
    Mh <- get_ir_matrix(M_, oo_, T_, shock_timing, cache = cache)
    
    # Flatten the target matrix so that each row is a response variable for a 
    # particular time period.
    target_flat <- target |>
        flatten_target()
    
    # Modify the impulse response matrix: 
        # 1. Subset it to (a) only include response variables being targeted and 
        #   (b) only include periods being targeted. 
        # 2. Flatten it, so each row is a response variable for a time period,
            # and each column is an individual shock in a period (e.g. cash rate 
            # shock in period 3).
            # See `flatten_ir_matrix` for more detail.
    Mh_flat <- Mh |>
        flatten_ir_matrix(selected_periods = 1:t_max_target, 
                          selected_resp_vars=names(target))
    
    if (sum(!is.na(target_flat)) != ncol(Mh_flat)) {
        stop("Mh_flat should be square. i.e. same number of shocks as target values.")
    }
    # Back out shocks required to hit the target.
    shocks <- qr.solve(Mh_flat, target_flat)
    # Given these shocks, solve for the IRF of ALL endogenous variables. 
    Mh_flat_all <- Mh |>
        flatten_ir_matrix()
    irf <- Mh_flat_all %*% shocks # TODO: Add NAN filter in here.
    
    # Reshape output so rows are time and columns are endogenous variables. 
    out <- irf |> 
        matrix(nrow=dim(Mh)[[1]], ncol=dim(Mh)[[2]], 
               dimnames = list("response_var" = dimnames(Mh)[[1]], "t" = dimnames(Mh)[[2]])) |>
        t()
    if (pretty) {
        out <- out |> 
            as.data.frame() |>
            dplyr::mutate(t = 1:nrow(out), shock=shock_nickname) |>
            pretty_irf(M_)
    }
    out
}
