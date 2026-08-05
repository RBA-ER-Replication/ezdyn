# read_dynare.R 
# Hamish Sullivan, 2025
# Utilities for reading in a .mat (or .json) file containing the dynare M_ and oo_ objects. 
# MATLAB helpers ----------------------------------------------------------
#' Unpack oo_ object from .mat file.
#'
#' @param oo_ Dynare `oo_` object loaded from MATLAB.
#'
#' @return Unpacked `oo_` list with restored names.
#' @export
unpack_oo <- function(oo_) {
    names(oo_) <- attr(oo_, "dimnames")[[1]]
    oo_
}

#' Repack MATLAB-loaded objects with updated names.
#'
#' @param obj Object loaded from MATLAB.
#'
#' @return Repacked object with restored names.
#' @export
repack <- function(obj) {
    obj_dimnames <- dimnames(obj)
    # Repack sub-components
    if (typeof(obj) == "list") {
        obj <- purrr::map(obj, repack)
        if (!is.null(obj_dimnames)) {
            names(obj) <- obj_dimnames[[1]]
        }
    }
    obj
}

#' Read Dynare output from JSON or MAT files.
#'
#' @param path Path to Dynare output (`.json` preferred; `.mat` quasi-supported).
#' @param path_meta Optional path to variable metadata (Excel) with columns
#'   `dynare_name`, `display_name`, `units`, `unit_symbol`, and `scale_formula`.
#' Can include additional columns if you like as well.
#' @param model_name Optional model name.
#'
#' @return A full MOO pair of class `dynare`, containing `oo_` and `M_`.
#' @export
#' @examples
#' \dontrun{
#' read_dynare("solved_model.json", "varmeta.xlsx", model_name = "DINGO")
#' }
read_dynare <- function(path, path_meta=NULL, model_name=NULL) {
    file_ext <- tools::file_ext(path)
    if (file_ext == "json") {
        solved_model <- readLines(path) |>
            jsonlite::fromJSON()
        M_ <- solved_model$M_
        oo_ <- solved_model$oo_

        # Make some definitions for legacy reasons. TODO: Clean up later.
        M_$endo.names <- M_$endo_names
        M_$exo.names <- M_$exo_names
        M_$param.names <- M_$param_names
        M_$param.names.tex <- M_$param_names_tex
        M_$endo.names.tex <- M_$endo_names_tex
        M_$exo.names.tex <- M_$exo_names_tex

    } else if (file_ext == "mat") {
        warning("The readMat package appears to have issues with reading in structs; data can become out of order. Recommended to use JSON instead.")
        message("The readMat package appears to have issues with reading in structs; data can become out of order. Recommended to use JSON instead.")
        # I tried reading in with hdf5 (which is the same as.mat v7.3) but this
        # was excessively complicated.
        mat <- R.matlab::readMat(path)
        oo_ <- repack(mat$oo.)
        M_ <- repack(mat$M.)
    }

    # Fix formatting of a couple of specific objects
    M_$endo.names <- unlist(M_$endo.names)
    M_$exo.names <- unlist(M_$exo.names)
    M_$param.names <- unlist(M_$param.names)
    M_$param.names.tex <- unlist(M_$param.names.tex)
    M_$endo.names.tex <- unlist(M_$endo.names.tex)
    M_$exo.names.tex <- unlist(M_$exo.names.tex)
    # Attach parameter values table and optional parameter metadata.
    M_$param_table <- ezdyn_attach_param_table(M_, path_meta = path_meta)
    M_$param_df <- ezdyn_param_values_from_M(M_) |>
        dplyr::select(Name, Value) |>
        tidyr::pivot_wider(names_from = "Name", values_from = "Value")

    if (!is.null(path_meta)) {
        sheets <- readxl::excel_sheets(path_meta)
        varmeta_sheet <- if ("variables" %in% sheets) "variables" else sheets[[1]]
        # Load Variable Name Metadata. Calculate scaling factors and bind into M_
        varmeta <- readxl::read_excel(path_meta, sheet = varmeta_sheet) |>
                dplyr::mutate(scale_formula = as.character(scale_formula)) |>
                #Calculate scaling factors for each variable using latest parameters.
                calc_scale_factors(M_)
        ezdyn_validate_var_alias_metadata(varmeta)
        M_$varmeta <- varmeta
        # If shock metadata is provided, load this into the object too.
        if ("shocks" %in% sheets) {
            shock_meta <- readxl::read_excel(path_meta, sheet = "shocks")
            ezdyn_validate_shock_alias_metadata(shock_meta)
            M_$shock_meta <- shock_meta
        }
    }

    M_$model_name <- model_name
    class(M_) <-"dynare" # Specify the type of the M_ object as dynare, as opposed to
    class(oo_) <-"dynare"

    model <- list(oo_ = oo_, M_ = M_)
    class(model) <- "dynare"
    model
}
