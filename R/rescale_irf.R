#' Rescale IRF using function or rescaling factor provided by user in metadata.
#'
#' @param irf IRF data frame or matrix with columns matching `M_$endo.names`.
#' @param M_ Dynare or custom model structure containing `varmeta` with scaling
#'   definitions.
#'
#' @return Rescaled IRF object with the same shape as input.
#' @export
rescale_irf <- function(irf, M_) {
    for (k in seq_along(M_$endo.names)){
        varname <- M_$endo.names[[k]]
        if (varname %in% M_$varmeta$dynare_name) {
            scale_ = M_$varmeta$scale_factor[M_$varmeta$dynare_name == varname][[1]]
            if (is.null(scale_)) { # If no scale defined, just return unscaled.
                scale <- function(x) {x}    
            } else if (!is.function(scale_)) { # If the scale is not a function (integer), the function should be x*scale
                scale <- function(x) {scale_*x}
            } else { # Case when scale is already a function
                scale <- scale_
            }
            if (!is.null(scale)) {
                irf[, varname] <- scale(irf[, varname]) 
            }    
        }
    }    
    irf
}

#' Rescale an arbitrary matrix/dataframe (e.g. IRF) using metadata-defined scaling.
#'
#' @param obj Object with columns matching `M_$varmeta$dynare_name`.
#' @param M_ Dynare or custom model structure containing `varmeta` with scaling
#'   definitions.
#'
#' @return Rescaled object with the same shape as input.
#' @export
rescale <- function(obj, M_) {
    for (varname in colnames(obj)){
        if (varname %in% M_$varmeta$dynare_name) {
            scale_ = M_$varmeta$scale_factor[M_$varmeta$dynare_name == varname][[1]]
            if (is.null(scale_)) { # If no scale defined, just return unscaled.
                scale <- function(x) {x}    
            } else if (!is.function(scale_)) { # If the scale is not a function (integer), the function should be x*scale
                scale <- function(x) {scale_*x}
            } else { # Case when scale is already a function
                scale <- scale_
            }
            if (!is.null(scale)) {
                obj[, varname] <- scale(obj[, varname]) 
            }    
        }
    }    
    obj
}


# Scale factors -----------------------------------------------------------
#' Calculate a scaling factor from a metadata expression and dataframe of parameters.
#'
#' @param expr Scaling formula as a string (e.g. "4 * ss_share").
#' @param params_df One-row data frame of model parameters.
#'
#' @return Numeric scaling factor.
#' @export
calc_scale_factor <- function(expr, params_df) {
    if (is.na(expr)) {
        scale_factor <- 1    
    } else {
        scale_factor <- eval(rlang::parse_expr(expr), as.list(params_df))
    }
    unlist(scale_factor)
}

#' Calculate scaling factors for all variables in metadata.
#'
#' @param expr_df Data frame with a `scale_formula` column.
#' @param M_ Model structure containing `param_df`.
#'
#' @return Metadata data frame with a new `scale_factor` column.
#' @export
calc_scale_factors <- function(expr_df, M_) {
    fn <- purrr::partial(calc_scale_factor, params_df=M_$param_df)
    expr_df$scale_factor <- purrr::map(expr_df$scale_formula, fn)
    expr_df
}

