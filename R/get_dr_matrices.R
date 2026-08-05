#' Get decision rule coefficients for lagged variables and structural shocks. 
#' @param M_ Dynare model structure containing model specifications and parameters. 
#' @param oo_ Dynare output structure containing simulation results. 
#' @param adjusted whether to adjust or otherwise return the raw matrices from dynare. 
#' Adjustment implies reordering A and B, as well as restricting A to only contain state variables.
get_dr_matrices <- function(M_, oo_, adjusted=TRUE) {
    A = oo_$dr$ghx # Response to lagged variables
    B = oo_$dr$ghu # Response to structural shocks
    
    # Adjust coefficients based on the order of variables. 
    A_adj = A[oo_$dr$inv_order_var,]
    B_adj = B[oo_$dr$inv_order_var,]
    # Identify state variables and construct transition matrix At.
    # There are more commments in the MATLAB implementation describing this legacy code. 
    max_lag = as.numeric(M_$maximum_lag)
    endo_n = as.numeric(M_$endo_nbr) # Number of endogenous 
    endo_names = unlist(M_$endo_names) # Endogenous variable names
    exo_names = unlist(M_$exo_names) # Endogenous variable names
    exo_n = length(M_$exo_names) # number of exogenous vars
    k2 = oo_$dr$kstate[oo_$dr$kstate[,2] <= max_lag + 1, c(1, 2)]
    i = 1
    istate = oo_$dr$order_var[k2[,1]] + (min(i, max_lag) + 1 - k2[,2])*endo_n     # List of indices which are state variables (I think?)
    # Construct transition matrix. Zero if not state variable, A_adj if state variable. 
    A_t = matrix(0, nrow=endo_n, ncol=endo_n)
    for (jj in 1:endo_n) {
        if (jj %in% istate) {
            A_t[,jj] = A_adj[,istate == jj]
        }
    }
    rownames(A_t) <- endo_names
    colnames(A_t) <- endo_names
    rownames(B_adj) <- endo_names
    colnames(B_adj) <- exo_names
    # Return either adjusted or unadjusted matrices
    if (adjusted) {
        out <- list(A=A_t, B=B_adj)
    } else {
        out <- list(A=A, B=B)
    }
    out
}
