# get_hd.R
# Reimplementation of MATLAB code which pulls a historical shock decomposition from a Dynare M_oo_ object in R.

#' Convert Dynare-style name containers to a clean character vector.
#' TODO: deprecate.
extract_dynare_names <- function(x) {
	if (is.null(x)) {
		return(character())
	}

	if (is.matrix(x)) {
		if (is.character(x)) {
			return(trimws(apply(x, 1, paste0, collapse = "")))
		}
		return(trimws(as.character(x[, 1])))
	}

	if (is.data.frame(x)) {
		return(trimws(as.character(x[[1]])))
	}

	if (is.list(x)) {
		out <- vapply(
			x,
			FUN.VALUE = character(1),
			FUN = function(el) {
				if (length(el) == 0) {
					return("")
				}
				if (is.character(el) && length(el) > 1) {
					return(paste0(el, collapse = ""))
				}
				as.character(el[[1]])
			}
		)
		return(trimws(out))
	}

	trimws(as.character(x))
}


# TODO: Don't think this is necessary. 
#' Resolve the first available field from a list/object.
#'
#' @param x A list-like object.
#' @param candidates Character vector of candidate field names.
#' @return The first non-null field value, else NULL.
resolve_first_field <- function(x, candidates) {
	for (nm in candidates) {
		if (!is.null(x[[nm]])) {
			return(x[[nm]])
		}
	}
	NULL
}


#' Historical shock decomposition from Dynare smoothed objects.
#'
#' Reimplementation of MATLAB `getHD.m`.
#'
#' @param M_ Dynare model structure.
#' @param oo_ Dynare output structure.
#' @param varlist Optional variable list (kept for API compatibility).
#' @param nvar_unobs Number of unobserved variables.
#' @param i_var_unobs Integer indices of unobserved variables in `M_$endo_names`.
#'
#' @return A list with:
#'   - `shock_decomposition`: 3D array
#'     `[endo_nbr x (nshocks + 3) x gend]`
#'   - `z_unobs`: matrix `[nvar_unobs x gend]`
#'
#'   @examples {
#'   #hd <- get_hd(dyn$M_, dyn$oo_, c("r_obs", "pi_obs"))
#'   }
#' @export
get_hd <- function(M_, oo_, varlist = NULL, nvar_unobs = 0L, i_var_unobs = integer()) {
	# `varlist` is currently unused in the MATLAB source and retained for compatibility.
	force(varlist)

	# Useful definitions
	endo_nbr <- M_$endo_nbr
	nshocks <- M_$exo_nbr
	dr <- oo_$dr
	maximum_lag <- M_$maximum_lag

	# Data ordering
	order_var <- dr$order_var
	inv_order_var <- oo_$dr$inv_order_var

	# Reduced form coefficients
	A <- dr$ghx
	B <- dr$ghu
	es <- oo_

	exo_names <- extract_dynare_names(resolve_first_field(M_, c("exo_names", "exo.names")))
	endo_names <- extract_dynare_names(resolve_first_field(M_, c("endo_names", "endo.names")))

	if (length(exo_names) != nshocks) {
		stop("Length of exogenous names does not match M_$exo_nbr.")
	}
	if (length(endo_names) != endo_nbr) {
		stop("Length of endogenous names does not match M_$endo_nbr.")
	}

	# 1. Recover smoothed shocks: epsilon [nshocks x gend]
	epsilon <- do.call(
		rbind,
		lapply(exo_names, function(shk) {
			as.numeric(es$SmoothedShocks[[shk]])
		})
	)

	gend <- ncol(epsilon)
	time_names <- as.character(seq_len(gend))

	dimnames(epsilon) <- list(exo_names, time_names)

	# Main decomposition array:
	# [endo_nbr x (nshocks+3) x gend]
	shock_col_names <- c(exo_names, "total", "smoothed", "residual")
	z <- array(
		0,
		dim = c(endo_nbr, nshocks + 3L, gend),
		dimnames = list(endo_names, shock_col_names, time_names)
	)

	# Fill smoothed endogenous series in column (nshocks + 2)
	for (ii in seq_len(endo_nbr)) {
		z[ii, nshocks + 2L, ] <- as.numeric(es$SmoothedVariables[[endo_names[ii]]])
	}

	# Recover unobserved variables matrix [nvar_unobs x gend]
	if (length(i_var_unobs) > 0L) {
		i_var_unobs <- as.integer(i_var_unobs)
		z_unobs <- do.call(
			rbind,
			lapply(i_var_unobs, function(idx) {
				as.numeric(es$SmoothedVariables[[endo_names[idx]]])
			})
		)
		if (is.null(dim(z_unobs))) {
			z_unobs <- matrix(z_unobs, nrow = 1L)
		}
		rownames(z_unobs) <- endo_names[i_var_unobs]
		colnames(z_unobs) <- time_names
	} else {
		z_unobs <- matrix(
			numeric(0),
			nrow = as.integer(nvar_unobs),
			ncol = gend,
			dimnames = list(character(as.integer(nvar_unobs)), time_names)
		)
	}

	# State indexing used in recursive contribution propagation
	kstate <- dr$kstate
	k2 <- kstate[kstate[, 2] <= (maximum_lag + 1L), c(1, 2), drop = FALSE]
	# Match MATLAB effective formula (given prior loop scope):
	# (maximum_lag + 1 - k2(:,2)) * endo_nbr
	i_state <- order_var[k2[, 1]] + (maximum_lag + 1L - k2[, 2]) * endo_nbr

	lags <- integer(0)
	for (tt in seq_len(gend)) {
		if (tt > 1L && tt <= (maximum_lag + 1L)) {
			lags <- seq.int(from = min(tt - 1L, maximum_lag), to = 1L, by = -1L)
		}

		if (tt > 1L) {
			m <- min(tt - 1L, maximum_lag)

			# Equivalent to MATLAB: permute(z(:,1:nshocks,lags), [1 3 2])
			tempx <- aperm(z[, seq_len(nshocks), lags, drop = FALSE], c(1, 3, 2))
			tempx <- matrix(tempx, nrow = endo_nbr * m, ncol = nshocks)

			# Pad missing lag blocks with zeros when tt <= maximum_lag + 1
			n_pad <- endo_nbr * max(maximum_lag - tt + 1L, 0L)
			if (n_pad > 0L) {
				tempx <- rbind(tempx, matrix(0, nrow = n_pad, ncol = nshocks))
			}

			z[, seq_len(nshocks), tt] <- A[inv_order_var, , drop = FALSE] %*%
				tempx[i_state, , drop = FALSE]

			lags <- lags + 1L
		}

		# Add contemporaneous shock contribution:
		# B(inv_order_var,:) .* repmat(epsilon(:,tt)', endo_nbr, 1)
		z[, seq_len(nshocks), tt] <- z[, seq_len(nshocks), tt] +
			sweep(B[inv_order_var, , drop = FALSE], 2, epsilon[, tt], `*`)

		# Total contribution of all shocks
		z[, nshocks + 1L, tt] <- rowSums(z[, seq_len(nshocks), tt, drop = FALSE])

		# Residual = smoothed series - total shock contribution
		z[, nshocks + 3L, tt] <- z[, nshocks + 2L, tt] - z[, nshocks + 1L, tt]
	}

	list(
		shock_decomposition = z,
		z_unobs = z_unobs
	)
}