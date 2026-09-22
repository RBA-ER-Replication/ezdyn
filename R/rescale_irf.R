# Build an O(1)-lookup, first-match environment mapping `dynare_name` to its
# `scale_factor`, so `rescale()`/`rescale_irf()` avoid re-scanning the whole
# `varmeta` table (an O(n^2) cost across all IRF response variables) for every
# column being rescaled.
ezdyn_scale_lookup <- function(M_) {
    lookup <- new.env(parent = emptyenv())
    varmeta <- M_$varmeta
    if (is.null(varmeta) || !("dynare_name" %in% names(varmeta)) || !("scale_factor" %in% names(varmeta))) {
        return(lookup)
    }
    dynare_names <- varmeta$dynare_name
    for (i in seq_along(dynare_names)) {
        varname <- dynare_names[[i]]
        if (!is.na(varname) && nzchar(varname) && !exists(varname, envir = lookup, inherits = FALSE)) {
            assign(varname, varmeta$scale_factor[[i]], envir = lookup)
        }
    }
    lookup
}

# Apply one variable's cached scale factor/function to its IRF column. A
# missing lookup entry means the variable isn't in `varmeta`, so `obj` is left
# untouched (matches the previous `varname %in% ...` behaviour); a `NULL`
# scale factor means "no scaling", equivalent to the previous identity-`scale`
# no-op.
ezdyn_apply_scale <- function(obj, varname, lookup) {
    if (!exists(varname, envir = lookup, inherits = FALSE)) {
        return(obj)
    }
    scale_ <- get(varname, envir = lookup, inherits = FALSE)
    if (is.null(scale_)) {
        return(obj)
    }
    if (is.function(scale_)) {
        obj[, varname] <- scale_(obj[, varname])
    } else {
        obj[, varname] <- scale_ * obj[, varname]
    }
    obj
}

#' Rescale IRF using function or rescaling factor provided by user in metadata.
#'
#' @param irf IRF data frame or matrix with columns matching `M_$endo.names`.
#' @param M_ Dynare or custom model structure containing `varmeta` with scaling
#'   definitions.
#'
#' @return Rescaled IRF object with the same shape as input.
#' @export
rescale_irf <- function(irf, M_) {
    lookup <- ezdyn_scale_lookup(M_)
    for (varname in M_$endo.names) {
        irf <- ezdyn_apply_scale(irf, varname, lookup)
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
    lookup <- ezdyn_scale_lookup(M_)
    for (varname in colnames(obj)) {
        obj <- ezdyn_apply_scale(obj, varname, lookup)
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

