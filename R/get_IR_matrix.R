#' Create a MxHxSxT matrix where:
#' M: Number of response variables to IRF 
#' H: Number of time periods (i.e. horizon)
#' S: Number of shock variables.
#' T: Maximum period in which a shock occurs.
#' @param shock_timing Named list. Name is the name of the shock, values are a list of periods in which the shock is active. 
#' @param horizon horizon for which the irf is plotted.
#' @param var_names Optional response variables to retain. Derived policy
#'   variables ending in `_gap_loss` inherit the IRF of their underlying level.
#' @param cache Logical. If `TRUE`, read/write the result from/to `oo_$.irf_cache`
#'   (see `ezdyn_irf_cache()`), keyed on `horizon`, `shock_timing` and `var_names`.
get_ir_matrix <- function(M_, oo_, horizon=40, shock_timing, var_names = NULL, cache = FALSE) {
    key <- deparse1(list(horizon, shock_timing, var_names))
    ezdyn_irf_cache(oo_, cache, key, function() {
    shock_names <- names(shock_timing)
    base_irf <- get_irf(M_, oo_, shock_names, horizon)
    if (!is.null(var_names)) {
        base_irf <- ezdyn_subset_irf_variables(base_irf, var_names, M_$varmeta)
    }
    # Helper function. If a given shock occurs at a specific shock period, 
    # modify the IRF so that it starts at the shock period and ends at the horizon.
    irf_for_shock <- function(shock_name, shock_period) {
        periods_left <- horizon - shock_period
        # Initialise the shock array as zeros.
        shock <- array(0, dim=dim(base_irf[,,shock_name]), dimnames=dimnames(base_irf[,,shock_name]))
        # Add the shock for the relevant periods.  
        shock[,shock_period:horizon] <- base_irf[,1:(periods_left+1), shock_name]
        shock
    }
    
    # Create a MxHxSxT matrix where:
    # M: Number of response variables to IRF 
    # H: Number of time periods (i.e. horizon)
    # S: Number of shock variables.
    # T: Maximum period in which a shock occurs
    
    # Determine the value of T; the maximum period in which any shock is 
    # active. 
    T_ <- purrr::map(shock_timing, function(x) max(unlist(x))) |>
        unlist() |>
        max()
    # Initialise the array 
    out_dims <- c(dim(base_irf), T_)
    out_names <- append(dimnames(base_irf), list("shock_period_i" = 1:T_))
    out <- array(dim=out_dims, dimnames = out_names)
    # Fill the array with the IRFs
    for (shock_var in shock_names) { # Shock variable
        for (i in shock_timing[[shock_var]]) { # Shock timing
            out[,,shock_var, i] <- irf_for_shock(shock_var, i)
        }
    }
    out
    })
}

# Read a cached IRF result from `oo_$.irf_cache`, or compute and store it there.
# `oo_$.irf_cache` is an environment (reference semantics), initialised once
# when a MOO pair is built (see `custom_moo()`/`read_dynare()`), so writes made
# here remain visible to every holder of that `oo_`, even though `oo_` itself
# is an ordinary (copy-on-modify) list.
ezdyn_irf_cache <- function(oo_, cache, key, compute) {
    if (!cache) return(compute())
    if (is.null(oo_$.irf_cache)) oo_$.irf_cache <- new.env(parent = emptyenv())
    if (is.null(oo_$.irf_cache[[key]])) oo_$.irf_cache[[key]] <- compute()
    oo_$.irf_cache[[key]]
}

# A variable that is natively available in the IRF always wins over any
# `derived_from` fallback declared for it in metadata (e.g. a variable that is
# a genuine model equation can still carry `derived_from` for baseline
# purposes without affecting its IRF, which is resolved natively here). For a
# derived variable, the source's own `scale_factor` is applied to its raw
# values before any transform; the derived variable's own `scale_factor` is
# applied afterwards, downstream, by the caller's own `rescale()` call (e.g.
# `build_policy_irfs()`) - never here, to avoid applying it twice.
ezdyn_subset_irf_variables <- function(irf, variables, varmeta = NULL) {
    available <- dimnames(irf)[[1]]
    lookup <- ezdyn_derived_variable_lookup(varmeta)
    source_variables <- vapply(variables, function(variable) {
        base_variable <- sub("_gap_loss$", "", variable)
        if (base_variable %in% available) return(base_variable)
        derived <- lookup[[base_variable]]
        if (!is.null(derived) && derived$source %in% available) return(derived$source)
        base_variable
    }, character(1))
    missing <- unique(source_variables[!(source_variables %in% available)])
    if (length(missing) > 0) {
        stop(sprintf("Requested IRF variables are not available: %s.", paste(missing, collapse = ", ")), call. = FALSE)
    }

    out <- array(0, c(length(variables), dim(irf)[2], dim(irf)[3]), list(variables, dimnames(irf)[[2]], dimnames(irf)[[3]]))
    for (i in seq_along(variables)) {
        values <- irf[source_variables[[i]], , , drop = FALSE]
        variable <- sub("_gap_loss$", "", variables[[i]])
        derived <- lookup[[variable]]
        is_derived <- !is.null(derived) && source_variables[[i]] == derived$source
        for (shock in seq_len(dim(irf)[3])) {
            x <- as.numeric(values[1, , shock])
            if (is_derived) {
                x <- ezdyn_own_scale(varmeta, derived$source)(x)
                if (!is.na(derived$transform)) {
                    x <- ezdyn_derived_transform_registry[[derived$transform]](x)
                }
            }
            out[i, , shock] <- x
        }
    }
    out
}




