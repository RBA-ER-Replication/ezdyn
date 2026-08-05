
rename_irf <- function(irf, varmeta) {
  if (!"rename_to" %in% names(varmeta)) return(irf)
  irf |>
    dplyr::left_join(
      varmeta |> dplyr::select(dynare_name, rename_to),
      by = c("resp_var" = "dynare_name")
    ) |>
    dplyr::mutate(resp_var = dplyr::coalesce(rename_to, resp_var)) |>
    dplyr::select(-rename_to)
}

rename_meta <- function(varmeta) {
  if (!"rename_to" %in% names(varmeta)) return(varmeta)

  varmeta |>
    dplyr::mutate(
      original_name = dynare_name,
      # Takes the rename_to value if it exists, otherwise keeps the original dynare_name.
      dynare_name = dplyr::coalesce(rename_to, dynare_name)
    )
}
#' Build Moo objects for a non-Dynare model, by supplying IRFs and any applicable metadata.
#'
#' @param irf A long data frame with columns `t` (time), `resp_var` (response variable), `shock` (shock variable), and `value` (IRF value).
#' @param model_name Optional model name (used in pretty IRF outputs).
#' @param meta optional path to variable metadata (excel file with columns
#' dynare_name, display_name, units, unit_symbol, scale_formula [a formula for rescaling the IRF].
#' Additional metadata columns can be provided (for example, I add default [whether this variable is shown by default],
#' alt_paths [T/F whether to  include on alt paths module]).
#' @param param_df One-row data frame of model parameters used to
#'   evaluate scaling formulas. Default is an empty dataframe.
#' @param subset_vars Boolean. if TRUE, clear the IRF dataframe of all variables not in subset_vars.
#'
#' @return A full MOO pair of class `custom_moo`, containing `M_` and `oo_`.
#' @export
#' @examples
#' \dontrun{
#' moo <- custom_moo(irf_df, model_name = "MyModel", meta = "meta.xlsx")
#' }
custom_moo <- function(irf, model_name=NULL, meta=NULL, param_df=data.frame(), subset_vars=FALSE) {
    M_ <- list()
    if (!is.null(meta)) {
        sheets <- readxl::excel_sheets(meta)
        varmeta_sheet <- if ("variables" %in% sheets) "variables" else sheets[[1]]
        varmeta <- readxl::read_excel(meta, sheet = varmeta_sheet) |>
          dplyr::mutate(scale_formula = as.character(scale_formula))
        # Renames the endogenous variable names in the IRF with values from 'rename_to' column (if it exists).
        irf <- rename_irf(irf, varmeta)
        # Update metadata so renamed variables go into dynare_name column,
        # and the original names archived in original_names column.
        varmeta <- rename_meta(varmeta)
    }
    M_$model_name <- model_name
    M_$endo.vars <- unique(irf$resp_var)
    M_$exo.vars <- unique(irf$shock)
    M_$param_df <- param_df
    # param_df/param_table implementaiton needs to be reworked for custom_moos. 
    # M_$param_table <- ezdyn_attach_param_table(M_, path_meta = meta)
    if (!is.null(meta)) {
      M_$varmeta <- varmeta |>
          #Calculate scaling factors for each variable using latest parameters.
          calc_scale_factors(M_)
      ezdyn_validate_var_alias_metadata(M_$varmeta)
      if ("shocks" %in% sheets) {
        shock_meta <- readxl::read_excel(meta, sheet = "shocks")
        ezdyn_validate_shock_alias_metadata(shock_meta)
        M_$shock_meta <- shock_meta
      }
    }
    oo_ <- list()
    if (subset_vars) {
        irf <- irf |>
        dplyr::filter(resp_var %in% M_$varmeta$dynare_name)
    }

    oo_$irf <- irf

    class(M_) <- "custom_moo"
    class(oo_) <- "custom_moo"
    model <- list(M_ = M_, oo_ = oo_)
    class(model) <- "custom_moo"
    model
}

#' Read/build a Moo object
#'
#' Convenience wrapper around [custom_moo()].
#'
#' @inheritParams custom_moo
#' @return A full MOO pair of class `custom_moo`, containing `M_` and `oo_`.
#' @export
read_moo <- function(irf, model_name=NULL, meta=NULL, param_df=data.frame(), subset_vars=FALSE) {
  custom_moo(
    irf = irf,
    model_name = model_name,
    meta = meta,
    param_df = param_df,
    subset_vars = subset_vars
  )
}
