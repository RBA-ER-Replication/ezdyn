# Hamish Sullivan, 2025.

#' Given an IR matrix from get_ir_matrix(), flatten the matrix so that each row is a response variable for a particular time period, and each 
#' column is a shock that occurred in a particular time period. 
#' This is useful for backing out the scale of shocks required to 
#' achieve a given impulse response. 
#' @param ir_matrix result from get_ir_matrix()
#' @param selected_resp_vars if you want to subset the variables before flattening. Default
#'   selects all response variables, in the IRF's native order. When explicitly supplied,
#'   the flattened rows follow `selected_resp_vars`'s order (not the IRF's native order) -
#'   callers that need a specific row order (e.g. to match a target vector built from a
#'   named list) should rely on this.
#' @param selected_periods Subset the number of periods before flattening. Default selects all periods in ir_matrix.
#'  
#' @examples 
#' ir_matrix <- get_ir_matrix(M_, oo_, 40, list("eps_r"=1:3, "eps_psi"=1:3))
#' flat_matrix <- ir_matrix[rownames(ir_matrix) %in% c("r_obs", "ntwi_growth"),,,] |>
#'     flatten_ir_matrix() 
#' 
flatten_ir_matrix <- function(ir_matrix, selected_resp_vars=dimnames(ir_matrix)[[1]], selected_periods=1:dim(ir_matrix)[[2]]) {
    periods_ix <- 1:dim(ir_matrix)[[2]] %in% selected_periods
    # match() (rather than %in%) preserves selected_resp_vars's own order in the
    # flattened output, instead of silently reverting to the IR matrix's native
    # variable order - callers that build a target vector in a particular
    # variable order (e.g. get_irf_target()'s selected_resp_vars=names(target))
    # need the flattened rows to follow that same order.
    respvar_ix <- match(selected_resp_vars, dimnames(ir_matrix)[[1]])
    # drop=FALSE ensures that if one of the dimensions is of length 1, R doesn't 
    # drop that dimension and its labels. 
    ir_matrix <- ir_matrix[drop=FALSE, respvar_ix,periods_ix,,]
    
    # Get the labels for each dimension. 
    shock_vars <- dimnames(ir_matrix)[[3]]
    response_vars <- dimnames(ir_matrix)[[1]]
    timesteps <- 1:dim(ir_matrix)[[2]]
    shock_periods <- dimnames(ir_matrix)[[4]]
    
    # Construct the new labels (row/column names).
    row_names <- outer(response_vars, timesteps, paste, sep=".") |>
        as.vector()
    col_names <- outer(shock_vars, shock_periods, paste, sep = ".") |> 
        as.vector()
    dim_names <- list("response_var.t"=row_names, 
                      "shock_var.shock_period"=col_names)
    
    # Reshape the matrix and add the new names.  
    flat <- matrix(ir_matrix, nrow = length(row_names), 
                   ncol=length(col_names),
                   dimnames=dim_names)
    
    flat
}

#' Given a target matrix, flatten it so that each row is a response variable for a particular time period.
#' Entries of `target` may have different lengths - each is padded with trailing `NA`
#' (unconstrained periods) up to the longest entry's length before flattening.
#' @examples 
#' target <- matrix(0, nrow=2, ncol=40) 
#' rownames(target) = c("r_obs", "ntwi_growth")
#' target
flatten_target <- function(target) {
    # Pad every entry to the longest entry's length with trailing NA
    # (unconstrained periods), rather than relying on as.data.frame()'s
    # recycling - recycling only "works" when lengths happen to divide evenly,
    # and even then silently reshapes using length(target[[1]]) alone, which is
    # wrong whenever a later entry is longer than the first.
    max_len <- max(lengths(target))
    target <- lapply(target, function(x) {
        length(x) <- max_len
        x
    })
    flat_target_row_names <- outer(names(target), 1:max_len, paste, sep=".") |>
        as.vector()
    flat_target <- target |>
        as.data.frame() |>
        t() |>
        matrix(nrow=length(target)*max_len, dimnames = list("response_var.t"=flat_target_row_names))
    flat_target
}




