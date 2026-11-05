# remove_endo_vars.R
# Overlay for pruning unnecessary endogenous variables out of an already-built
# MOO object (`dynare` or `custom_moo`), by regex pattern match against
# variable codes. Large Dynare models can carry hundreds of endogenous
# variables that are never used downstream (e.g. reporting-only aliases),
# which slows shock decompositions and alternative-path calculations; pruning
# them shrinks the decision-rule matrices/IRF tables those calculations
# actually operate on.

#' Remove endogenous variables from a MOO object.
#'
#' A safe overlay on top of an already-built `dynare`/`custom_moo` pair (see
#' [read_dynare()]/[custom_moo()]) - removes matched endogenous variables from
#' the model's metadata and (for `dynare` models) the precomputed
#' decision-rule matrices (`oo_$dr$A`/`B`, see [get_dr_matrices()]).
#'
#' @param model A MOO pair of class `dynare` or `custom_moo`.
#' @param patterns Character vector of regular expressions (matched via
#'   [grepl()]). Any endogenous variable code matching *any* pattern is a
#'   candidate for removal.
#'
#' @details
#' Two safety rules prevent silently breaking the model:
#' - Dynare state variables (`M_$state_var`) are never removed, even if
#'   matched - dropping one would break the decision-rule recursion for every
#'   other variable, since every period's values are computed from the
#'   previous period's state. Matched state variables are skipped with a
#'   `warning()` listing them.
#' - If a matched variable is the `derived_from` source of another
#'   `M_$varmeta` row that is *not* itself matched, `remove_endo_vars()`
#'   stops with an error listing the dependent row(s) - that row's IRF/
#'   baseline values can no longer be computed once its source disappears.
#'   Include the dependent variable(s) explicitly in `patterns` to remove
#'   both together.
#'
#' `custom_moo` models have no state-space/recursion concept (every row is
#' already a precomputed IRF value), so only the second rule applies there.
#'
#' @return The updated MOO pair.
#' @export
#' @examples
#' \dontrun{
#' model <- read_dynare("model.json", "varmeta.xlsx")
#' model <- remove_endo_vars(model, c("^aux_", "_lag[0-9]+$"))
#' }
remove_endo_vars <- function(model, patterns) {
    if (!is.character(patterns) || length(patterns) == 0) {
        stop("`patterns` must be a non-empty character vector of regular expressions.", call. = FALSE)
    }
    if (inherits(model, "dynare")) {
        ezdyn_remove_endo_vars_dynare(model, patterns)
    } else if (inherits(model, "custom_moo")) {
        ezdyn_remove_endo_vars_custom(model, patterns)
    } else {
        stop("`model` must be a `dynare` or `custom_moo` MOO pair.", call. = FALSE)
    }
}

# Union of `grepl(pattern, codes)` across `patterns` -> matched codes, in
# `codes`' original order (deduplicated).
ezdyn_resolve_removal_matches <- function(codes, patterns) {
    matched <- rep(FALSE, length(codes))
    for (pattern in patterns) {
        matched <- matched | grepl(pattern, codes)
    }
    unique(codes[matched])
}

# Protect Dynare state variables from removal - see `remove_endo_vars()`'s
# `@details`. Returns the subset of `matched` codes that are safe to remove
# (never a state variable), warning about any that were protected.
ezdyn_protect_state_vars <- function(M_, matched) {
    if (is.null(M_$state_var)) {
        return(matched)
    }
    endo_names <- unlist(M_$endo_names)
    state_codes <- endo_names[M_$state_var]
    protected <- intersect(matched, state_codes)
    if (length(protected) > 0) {
        warning(
            sprintf(
                "Not removing %s: Dynare state variable(s) cannot be removed without breaking the model's decision-rule recursion.",
                paste(sprintf("`%s`", protected), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    setdiff(matched, protected)
}

# Stop if a removed variable is the `derived_from` source of another varmeta
# row that isn't itself being removed - see `remove_endo_vars()`'s `@details`.
ezdyn_cascade_derived_removal <- function(varmeta, to_remove) {
    if (is.null(varmeta) || !("derived_from" %in% names(varmeta)) || length(to_remove) == 0) {
        return(invisible(NULL))
    }
    dependent <- varmeta |>
        dplyr::filter(derived_from %in% to_remove, !(dynare_name %in% to_remove))
    if (nrow(dependent) > 0) {
        stop(
            sprintf(
                "Cannot remove %s: derived variable(s) %s depend on them via `derived_from` but were not themselves matched. Include the derived variable(s) explicitly in `patterns` to remove both.",
                paste(sprintf("`%s`", unique(dependent$derived_from)), collapse = ", "),
                paste(sprintf("`%s`", dependent$dynare_name), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    invisible(NULL)
}

# Dimension/name consistency assertions after pruning - catches bugs rather
# than silently returning a corrupted model.
ezdyn_validate_pruned_model <- function(M_, oo_) {
    endo_names <- unlist(M_$endo_names)
    n <- length(endo_names)
    if (!identical(as.numeric(M_$endo_nbr), as.numeric(n))) {
        stop("Internal error: `M_$endo_nbr` does not match `length(M_$endo_names)` after pruning.", call. = FALSE)
    }
    if (!is.null(oo_$dr$A) && (nrow(oo_$dr$A) != n || ncol(oo_$dr$A) != n)) {
        stop("Internal error: `oo_$dr$A` dimensions do not match the pruned endogenous variable set.", call. = FALSE)
    }
    if (!is.null(oo_$dr$B) && nrow(oo_$dr$B) != n) {
        stop("Internal error: `oo_$dr$B` dimensions do not match the pruned endogenous variable set.", call. = FALSE)
    }
    if (!is.null(M_$state_var) && (anyNA(M_$state_var) || any(M_$state_var < 1) || any(M_$state_var > n))) {
        stop("Internal error: `M_$state_var` indices are invalid after pruning.", call. = FALSE)
    }
    invisible(NULL)
}

ezdyn_remove_endo_vars_dynare <- function(model, patterns) {
    M_ <- model$M_
    oo_ <- model$oo_

    endo_names <- unlist(M_$endo_names)
    matched <- ezdyn_resolve_removal_matches(endo_names, patterns)
    to_remove <- ezdyn_protect_state_vars(M_, matched)
    ezdyn_cascade_derived_removal(M_$varmeta, to_remove)

    if (length(to_remove) == 0) {
        message("remove_endo_vars(): no endogenous variables removed (no safe matches).")
        return(model)
    }

    # Obtain the full (pre-removal) decision-rule matrices before mutating
    # `M_` - `get_dr_matrices()` sizes/derives them from the CURRENT
    # `M_$endo_nbr`/`endo_names`, so this must happen first.
    dr_matrices <- get_dr_matrices(M_, oo_, adjusted = TRUE)

    keep <- !(endo_names %in% to_remove)
    new_endo_names <- endo_names[keep]

    M_$endo_names <- M_$endo_names[keep]
    M_$endo.names <- new_endo_names
    M_$endo_names_tex <- M_$endo_names_tex[keep]
    M_$endo.names.tex <- unlist(M_$endo_names_tex)
    M_$endo_nbr <- sum(keep)
    if (!is.null(M_$state_var)) {
        M_$state_var <- match(endo_names[M_$state_var], new_endo_names)
    }
    if (!is.null(M_$varmeta)) {
        M_$varmeta <- dplyr::filter(M_$varmeta, !(dynare_name %in% to_remove))
    }

    oo_$dr$A <- dr_matrices$A[keep, keep, drop = FALSE]
    oo_$dr$B <- dr_matrices$B[keep, , drop = FALSE]
    oo_$.irf_cache <- new.env(parent = emptyenv()) # Invalidate: cached IRFs were sized for the old variable set.

    model$M_ <- M_
    model$oo_ <- oo_
    ezdyn_validate_pruned_model(model$M_, model$oo_)
    model
}

ezdyn_remove_endo_vars_custom <- function(model, patterns) {
    M_ <- model$M_
    oo_ <- model$oo_

    codes <- unique(M_$endo.vars)
    to_remove <- ezdyn_resolve_removal_matches(codes, patterns)
    ezdyn_cascade_derived_removal(M_$varmeta, to_remove)

    if (length(to_remove) == 0) {
        message("remove_endo_vars(): no endogenous variables removed (no matches).")
        return(model)
    }

    oo_$irf <- dplyr::filter(oo_$irf, !(resp_var %in% to_remove))
    M_$endo.vars <- setdiff(M_$endo.vars, to_remove)
    if (!is.null(M_$varmeta)) {
        M_$varmeta <- dplyr::filter(M_$varmeta, !(dynare_name %in% to_remove))
    }
    oo_$.irf_cache <- new.env(parent = emptyenv())

    model$M_ <- M_
    model$oo_ <- oo_
    model
}
