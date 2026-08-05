#' Create a MxHxSxT matrix where:
#' M: Number of response variables to IRF 
#' H: Number of time periods (i.e. horizon)
#' S: Number of shock variables.
#' T: Maximum period in which a shock occurs.
#' @param shock_timing Named list. Name is the name of the shock, values are a list of periods in which the shock is active. 
#' @param horizon horizon for which the irf is plotted.
get_ir_matrix <- function(M_, oo_, horizon=40, shock_timing) {
    shock_names <- names(shock_timing)
    base_irf <- get_irf(M_, oo_, shock_names, horizon)
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
}




