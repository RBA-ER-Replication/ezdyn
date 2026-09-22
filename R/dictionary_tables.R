# dictionary_tables.R
# Variable and shock "dictionary" tables, analogous to get_param_table() in
# param_tables.R. Both list what a model's metadata defines (display names,
# units, descriptions) rather than computed values.

#' Build shared variable-table data for [get_variable_table()].
#'
#' @param M_ Model object containing `varmeta`.
#' @param show_codes Whether to keep the model's own raw variable code
#'   (`Code`) as a column.
#'
#' @return Data frame with columns `Variable`, `Units`, and optionally `Code`.
ezdyn_variable_table_data <- function(M_, show_codes = FALSE) {
    if (is.null(M_$varmeta)) {
        stop("`M_$varmeta` not found. Load metadata with read_dynare/custom_moo first.", call. = FALSE)
    }

    data <- M_$varmeta |>
        dplyr::filter(default == TRUE) |>
        dplyr::arrange(display_name) |>
        dplyr::transmute(
            Variable = display_name,
            Units = units,
            Code = dynare_name
        )

    if (!show_codes) {
        data <- dplyr::select(data, -Code)
    }

    data
}

#' Build a variable table from a model object.
#'
#' Lists the display names and units a model's metadata defines for its
#' variables (the same information used to label plots and IRF selectors),
#' analogous to [get_param_table()] for parameters.
#'
#' @param M_ Model object containing `varmeta` (populated by [read_dynare()]/
#'   [custom_moo()] when a metadata workbook is supplied).
#' @param mode Table format: `"katex"` (default) returns a static, styled
#'   `kableExtra` HTML table; `"dt"` returns an interactive, sortable/
#'   searchable [DT::datatable()].
#' @param show_codes Whether to include the model's own raw variable code
#'   (`Code`) alongside the display name - intended for advanced users.
#'
#' @return HTML `kableExtra` table (`mode = "katex"`) or a `DT::datatable`
#'   htmlwidget (`mode = "dt"`).
#' @export
get_variable_table <- function(M_, mode = c("katex", "dt"), show_codes = FALSE) {
    mode <- match.arg(mode)
    data <- ezdyn_variable_table_data(M_, show_codes = show_codes)
    if (mode == "dt") ezdyn_datatable(data) else ezdyn_kable_striped(data)
}

#' Build shared shock-table data for [get_shock_table()].
#'
#' @param M_ Model object containing `shock_meta`.
#' @param show_codes Whether to keep the model's own raw shock code (`Code`)
#'   as a column.
#'
#' @return Data frame with columns `Shock` and optionally `Code`.
ezdyn_shock_table_data <- function(M_, show_codes = FALSE) {
    if (is.null(M_$shock_meta)) {
        stop("`M_$shock_meta` not found. Load metadata with read_dynare/custom_moo first.", call. = FALSE)
    }

    data <- M_$shock_meta |>
        dplyr::transmute(
            Shock = dplyr::coalesce(description, shock),
            Code = shock
        ) |>
        dplyr::arrange(Shock)

    if (!show_codes) {
        data <- dplyr::select(data, -Code)
    }

    data
}

#' Build a shock table from a model object.
#'
#' Lists the descriptions a model's metadata defines for its shocks,
#' analogous to [get_param_table()] for parameters. Shocks without a defined
#' description fall back to showing their model code as `Shock`.
#'
#' @param M_ Model object containing `shock_meta` (populated by
#'   [read_dynare()]/[custom_moo()] when a metadata workbook's optional
#'   `shocks` sheet is supplied).
#' @param mode Table format: `"katex"` (default) returns a static, styled
#'   `kableExtra` HTML table; `"dt"` returns an interactive, sortable/
#'   searchable [DT::datatable()].
#' @param show_codes Whether to include the model's own raw shock code
#'   (`Code`) alongside the description - intended for advanced users.
#'
#' @return HTML `kableExtra` table (`mode = "katex"`) or a `DT::datatable`
#'   htmlwidget (`mode = "dt"`).
#' @export
get_shock_table <- function(M_, mode = c("katex", "dt"), show_codes = FALSE) {
    mode <- match.arg(mode)
    data <- ezdyn_shock_table_data(M_, show_codes = show_codes)
    if (mode == "dt") ezdyn_datatable(data) else ezdyn_kable_striped(data)
}
