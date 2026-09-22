# TODOs: 
# - make array and data_frame options mutually exclusive/a single argument
# - if rescale = TRUE, return rescaled IRF. If FALSE, return unscaled IRF.
# - add documentation for each function
# - add some error handling (e.g. if shock name not found, if horizon exceeds available data, etc.)



#' Get the impulse response function for given shocks. 
#' If array is true, returns the following MxHxS array: 
#' M: Number of response variables to IRF 
#' H: Number of time periods (i.e. horizon)
#' S: Number of shock variables.
#' @param M_ Model structure containing model metadata.
#' @param oo_ Model output object.
#' @param shock_names Character vector of shock codes. If `M_$shock_meta` has a
#'   `description` column, descriptions can also be supplied.
#' @param horizon Horizon to calculate impulse responses over.
#' @param pretty Boolean. If TRUE, returns the rescaled IRFs with names and units in a nice dataframe. 
#' Rescaling, names and units are based on metadata in M_. 
#' @param data_frame Logical. If TRUE, return a data frame instead of an array.
#' @details
#' `M_` and `oo_` must have matching `dynare` or `custom_moo` classes.
#' Load non-Dynare models as a custom MOO and pass its `M_` and `oo_` objects explicitly.
#' @export
get_irf <- function(M_, oo_, shock_names, horizon, pretty=FALSE, data_frame=pretty) {
    shock_names <- ezdyn_resolve_shock_names(shock_names, M_)
    model_type <- ezdyn_irf_model_type(M_, oo_)
    fn <- switch(
        model_type,
        dynare = get_irf_dynare,
        custom_moo = get_irf_custom
    )
    out <- fn(M_, oo_, shock_names, horizon, data_frame)
    if (pretty) {
        out <- pretty_irf(out, M_)
    }
    out
}

# Validate that the model and result objects identify the same supported MOO type.
ezdyn_irf_model_type <- function(M_, oo_) {
    model_type <- function(x) {
        if (inherits(x, "dynare")) {
            "dynare"
        } else if (inherits(x, "custom_moo")) {
            "custom_moo"
        } else {
            NA_character_
        }
    }

    types <- c(M_ = model_type(M_), oo_ = model_type(oo_))
    if (anyNA(types) || types[["M_"]] != types[["oo_"]]) {
        stop(
            "`M_` and `oo_` must have matching `dynare` or `custom_moo` classes.",
            call. = FALSE
        )
    }

    types[["M_"]]
}

get_irf_custom <- function(M_, oo_, shock_names, horizon, data_frame=FALSE) {
    if (horizon > max(oo_$irf$t)) {
        warning(sprintf("Requested horizon exceeds available IRF data. Only providing horizon up to %s", max(oo_$irf$t)))
    }

    resps <- oo_$irf |>
        dplyr::filter(t <= horizon, shock %in% shock_names)

    if (data_frame) {
        out <- resps |>
            tidyr::pivot_wider(id_cols=c("t", "shock"), names_from="resp_var", values_from="value")
    } else { # Convert to array of same format as get_irf_dynare
        out_array <- array(dim=c(length(unique(resps$resp_var)), length(unique(resps$t)), length(unique(resps$shock))))
                # Split by shock and pivot to response_var x t matrices
        out <- resps |>
            dplyr::group_by(shock) |>
            dplyr::group_split()
        shock_levels <- resps |>
            dplyr::distinct(shock) |>
            dplyr::pull(shock)

        mats <- purrr::map(out, ~ .x |>
            dplyr::select(resp_var, t, value) |>
            tidyr::pivot_wider(names_from = t, values_from = value) |>
            dplyr::arrange(resp_var) |>
            tibble::column_to_rownames("resp_var") |>
            as.matrix()
        )
        
        out_array <- array(dim = c(nrow(mats[[1]]), ncol(mats[[1]]), length(mats)))
        for (i in seq_along(mats)) {
            out_array[,,i] <- mats[[i]]
        }
        dimnames(out_array) <- list(
            "response_var" = rownames(mats[[1]]),
            "t" = colnames(mats[[1]]),
            "shock_var" = shock_levels
        )
        out <- out_array
    }
    out
}


#' Compute IRFs from Dynare decision-rule matrices.
#'
#' @param M_ Dynare model structure.
#' @param oo_ Dynare model output structure.
#' @param shock_names Character vector of Dynare shock codes.
#' @param horizon Horizon to calculate impulse responses over.
#' @param data_frame Logical. If TRUE, return a data frame instead of an array.
#' @keywords internal
get_irf_dynare <- function(M_, oo_, shock_names, horizon, data_frame=FALSE) {
    # Get decision rule matrices
    dr <- get_dr_matrices(M_, oo_)
    A <- dr$A
    B <- dr$B
    get_irf_for_single_shock <- function(shock_name) {
        # Subset B so that it only contains the variables being shocked.
        B_subset <- B[,shock_name]
        
        ## Construct impulse response for shocks occurring in time t=(0 or 1?)
        matrix.pow <- expm:::`%^%`
        # Slow (O(n^2)) but easier to read implementation
        # irf <- function(t) {
        #     matrix.pow(A, (t-1)) %*% B_subset
        # }
        # Ct_A <- purrr::map(1:horizon, irf) |>
        #     purrr::reduce(cbind)
        
        # Super-fast implementation
        Ct <- purrr::accumulate(
          .x = seq_len(horizon-1),
          .f = function(prev, t) A %*% prev,
          .init = B_subset
        ) |> 
          purrr::reduce(cbind)
        colnames(Ct) <- NULL

        if (data_frame) {
            Ct <- as.data.frame(t(Ct)) |>
                dplyr::mutate(t = 1:horizon,
                             shock = shock_name)
        }
        Ct
    }
    out <- purrr::map(shock_names, get_irf_for_single_shock)
    names(out) <- shock_names
    # Transform into MxHxS array
    # M: Number of response variables to IRF 
    # H: Number of time periods (i.e. horizon)
    # S: Number of shocks.
    if (data_frame) {
        out <- dplyr::bind_rows(out)
    } else {
        out_array <- array(dim=c(dim(out[[1]]), length(out)))
        for (i in seq_along(out)) {
            out_array[,,i] <- out[[i]]
        }
        dimnames(out_array) <- list("response_var"=rownames(out[[1]]), 
                                    "t"= colnames(out[[1]]),
                                    "shock_var"= names(out))
        out <- out_array
    }
    out
}

