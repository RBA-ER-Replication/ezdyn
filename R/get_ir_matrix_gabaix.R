#' Get IRF matrix for partially anticipated shock, using cognitive discounting.
#' This is inspired by the method described in Gabaix 2020.
#' The discounting is applied to the shock (not the state vector as in Gabaix 2020).
#' In depth: 
#' We have a partially anticipated shock which is announced in period 1. 
#' This is equivalent to some linear combination of fully anticipated 
#' shocks announced in periods 1 to T_shock (i.e. s_ann=1:T_shock) and 
#' with anticipation horizon h_ant=1 to T_shock.  
#' @param M_ M_ object.
#' @param oo_ oo_ object.
#' @param horizon number of periods to compute IRF for.
#' @param shock_name name of the shock
#' @param T_shock 
#' @param lambda Gabaix discounting parameter. This is the fraction of people who become aware of the shock in each period. 
#' 
#' @examples
#' ir_matrix_discounted <- get_ir_matrix_gabaix(M_, oo_, 40, "eps_r", 5, 0.8)
#' ir_matrix_discounted$Mh_total[1,1,1,1]
#'
get_ir_matrix_gabaix <- function(M_, oo_, horizon, shock_name, T_shock, lambda) {
    # Get effect of fully anticipated shocks in all periods. 
    shock_names <- paste0(shock_name, "_", 1:T_shock)
    shock_timing <- purrr::map(1:T_shock, function(x) 1:T_shock) # eps_r_1 is equivalent to eps_r 
    names(shock_timing) <- shock_names
    Mh_ant <- get_ir_matrix(M_, oo_, horizon, shock_timing)
    
    # Define three things: 
        # s_ann: Shock announcement period
        # s_imp: Shock implementation period
        # h_ant : shock anticipation horizon. How many periods ahead is the 
        # shock anticipated from? 
    # Note: s_imp = s_ann + h_ant; s_imp = 0 means immediate implementation.
    
    # `Mh_ant` has dimensions:
    # 1. response variable;
    # 2. response time;
    # 3. anticipation horizon plus one (`h_ant + 1`), represented by
    #    `shock_name_(h_ant + 1)`;
    # 4. announcement date (`s_ann`), at which that anticipated-shock
    #    innovation is active.
    #
    # Re-index it as `[response, time, implementation date, announcement date]`.
    # Entries with `s_ann > s_imp` remain zero because information cannot arrive
    # after the shock has been implemented.
    
    Mh_temp <- array(0,dim = c(dim(Mh_ant)[[1]], horizon, T_shock, T_shock))
    
    for (s_imp in 1:T_shock) {
        for (s_ann in 1:s_imp) {
            h_ant <- s_imp - s_ann
            Mh_temp[,,s_imp, s_ann] <- Mh_ant[,,h_ant+1,s_ann]
        }
    }
    
    dimnames(Mh_temp) <- append(dimnames(Mh_ant)[c(1,2)], list("s_imp" = 1:T_shock,
                                                               "s_ann" = 1:T_shock))
    
    # Get Gabaix discounting matrix applied to each shock. 
    # i,j shows what fraction of people *become* aware of the partially ant. shock 
    # implemented in period i, during period j. 
    # This is a marginal effect, showing the change in the fraction which 
    # anticipate the shock, rather than the total fraction who anticipate the shock. 
    # For example:
    # Suppose we are in period 3, and we consider a partially anticipated shock 
    # to be implemented in period 4. The total fraction of people aware of the 
    # shock is lambda. Hence, M(4,3) + M(4,2) + M(4,1) = lambda. 
    # In period 2, lambda^2 people will be aware of the shock. 
    #   Hence, M(4,2) + M(4,1) = lambda^2. 
    #   Which implies M(4,3) = lambda - lambda^2.
    # In period 1 (announcement period), lambda^3 people will be aware of the shock. 
    #   Hence, M(4,1) = lambda^3
    #          M(4,2) = lambda^2 - lambda^3
    discount_matrix <- matrix(0,nrow =T_shock, ncol=T_shock)
    for (i in 1:T_shock) {
        for (j in 1:T_shock) {
            if (j == 1) {
                discount_matrix[i,j] <- lambda^(i-j)
            } else {
                discount_matrix[i,j] <- (lambda^(i-j))*(1-lambda)
            }
        }
    }
    # Multiply IRF matrix for anticipated shocks by Gabaix discounting matrix
    # for each shock (rows), the columns give the effect of more agents becoming 
    # aware of it in each period. 
    # This matrix is useful for seeing how agents' expectations evolve over time.
    Mh_marginal <- Mh_temp
    for (ii in 1:dim(Mh_temp)[[1]]) {
        for (jj in 1:dim(Mh_temp)[[2]]) {
            Mh_marginal[ii,jj,,] <- Mh_marginal[ii,jj,,]*discount_matrix        
        }
    }
    # Sum across the columns to get the total effect of a unit partially anticipated 
    # shock in each period.
    Mh_total <- apply(Mh_marginal, c(1,2,3), sum)
    names(dimnames(Mh_total))[[3]] <- "shock_period_i"
    # For consistency with get_ir_matrix, slot in third dimension with shock name. 
    Mh_total <- array(Mh_total, dim=c(dim(Mh_total)[1:2], 1, dim(Mh_total)[[3]]),
                      dimnames = append(dimnames(Mh_total)[1:2], append(list("shock_var"=shock_name), dimnames(Mh_total)[3])))
    list(Mh_total=Mh_total, Mh_marginal=Mh_marginal)
}