# param_tables.R
# Hamish Sullivan, 2025
# Functions which help 
# given a model object, build a pretty table showing the parameter names and values. 

#' Build parameter values table from model object
#'
#' @param M_ Model object (Dynare or custom moo).
#'
#' @return Tibble with parameter values (`Name`, `Value`, `TexName`).
ezdyn_param_values_from_M <- function(M_) {
    if (!is.null(M_$param.names) && !is.null(M_$params)) {
        tex_names <- if (!is.null(M_$param.names.tex)) unlist(M_$param.names.tex) else NA_character_
        return(
            tibble::tibble(
                Name = as.character(unlist(M_$param.names)),
                TexName = as.character(tex_names),
                Value = as.numeric(as.vector(M_$params))
            )
        )
    }

    tibble::tibble(Name = character(), TexName = character(), Value = numeric())
}

ezdyn_load_parameter_metadata <- function(path_meta) {
    if (is.null(path_meta) || !file.exists(path_meta)) {
        return(NULL)
    }

    sheets <- readxl::excel_sheets(path_meta)
    if (!"parameters" %in% sheets) {
        return(NULL)
    }

    readxl::read_excel(path_meta, sheet = "parameters")
}

#' Build enriched parameter table for model object
#'
#' @param M_ Model object.
#' @param path_meta Optional path to metadata workbook.
#'
#' @return Tibble containing enriched parameter metadata and values.
ezdyn_attach_param_table <- function(M_, path_meta = NULL) {
    param_values <- ezdyn_param_values_from_M(M_)
    param_meta <- ezdyn_load_parameter_metadata(path_meta)

    if (!is.null(param_meta) && nrow(param_meta) > 0) {
        param_meta$Name <- as.character(param_meta$dynare_name)

        out <- param_meta |>
            dplyr::left_join(param_values, by = "Name")

        if (!"Dyn_Symbol" %in% names(out) && "Name" %in% names(out)) {
            out <- out |>
                dplyr::mutate(Dyn_Symbol = Name)
        }
    } else {
        out <- param_values |>
            dplyr::mutate(
                Dyn_Symbol = Name,
                Symbol = dplyr::if_else(!is.na(TexName), paste0("$", TexName, "$"), Name),
                Description = Name,
                Sector = NA_character_,
                Type = "All"
            )
    }

    out
}

#' Render LaTeX (as authored in a `LaTeX_Symbol` metadata column) to inline
#' KaTeX HTML/MathML for table display.
#'
#' @param x Character vector of LaTeX symbol strings.
#'
#' @return Character vector of rendered HTML/MathML strings.
ezdyn_render_katex_symbols <- function(x) {
    x <- as.character(x)
    x <- stringr::str_replace_all(x, "^\\$|\\$$", "")
    x <- stringr::str_replace_all(x, "\\\\\\\\", "\\\\")
    vapply(
        x,
        function(xx) as.character(katex::katex_html(xx, displayMode = FALSE, output = "mathml")),
        character(1)
    )
}

#' Build shared parameter-table data for [get_param_table()].
#'
#' Validates `M_$param_table`, applies the optional `Type` filter, renders
#' KaTeX symbols, and rounds parameter values. Both rendering modes of
#' [get_param_table()] are built on this shared, mode-agnostic tibble.
#'
#' @param M_ Model object containing `param_table`.
#' @param type Optional value to filter by `Type` column.
#' @param dp Number of decimal places for rounding numeric values.
#' @param show_codes Whether to keep the model's own raw parameter code
#'   (`Dyn_Symbol`) as a column.
#'
#' @return Tibble/data frame with columns among `Sector`, `Symbol`,
#'   `Dyn_Symbol`, `Description`, `Notes`, `Value`.
ezdyn_param_table_data <- function(M_, type = NULL, dp = 3, show_codes = FALSE) {
    if (is.null(M_$param_table)) {
        stop("`M_$param_table` not found. Load metadata with read_dynare/read_moo first.", call. = FALSE)
    }

    data <- M_$param_table
    if (!is.null(type) && "Type" %in% names(data)) {
        data <- data |>
            dplyr::filter(tolower(Type) == tolower(type))
    }

    if (nrow(data) == 0) {
        return(data.frame(Message = "No matching parameters."))
    }

    if (!"Value" %in% names(data)) {
        data <- data |>
            dplyr::mutate(Value = suppressWarnings(as.numeric(Value_Original)))
    }

    out <- data |>
        dplyr::mutate(
            Symbol = ezdyn_render_katex_symbols(LaTeX_Symbol),
            Value = round(as.numeric(Value), digits = dp)
        )

    selected_cols <- c("Sector", "Symbol", "Dyn_Symbol", "Description", "Notes", "Value")
    if (!show_codes) {
        selected_cols <- setdiff(selected_cols, "Dyn_Symbol")
    }
    selected_cols <- selected_cols[selected_cols %in% names(out)]
    out |>
        dplyr::select(dplyr::all_of(selected_cols))
}

#' Build a parameter table from a model object.
#'
#' @param M_ Model object containing `param_table` (populated by
#'   [read_dynare()]/[read_moo()]; not currently populated by [custom_moo()]).
#' @param type Optional value to filter by `Type` column.
#' @param mode Table format: `"katex"` (default) returns a static, styled
#'   `kableExtra` HTML table (KaTeX-rendered symbols, optional footnote;
#'   suited to reports/Rmd); `"dt"` returns an interactive, sortable/
#'   searchable [DT::datatable()] (suited to a live Shiny dashboard).
#' @param dp Number of decimal places for rounding numeric values.
#' @param as_of Optional timestamp text appended as a table footnote
#'   (`mode = "katex"` only).
#' @param show_codes Whether to include the model's own raw parameter code
#'   (`Dyn_Symbol`) alongside the rendered symbol - intended for advanced
#'   users who want to match displayed parameters back to the underlying
#'   model code.
#'
#' @return HTML `kableExtra` table (`mode = "katex"`) or a `DT::datatable`
#'   htmlwidget (`mode = "dt"`).
#' @export
get_param_table <- function(M_, type = NULL, mode = c("katex", "dt"), dp = 3, as_of = NULL, show_codes = FALSE) {
    mode <- match.arg(mode)
    data <- ezdyn_param_table_data(M_, type = type, dp = dp, show_codes = show_codes)

    if (mode == "dt") {
        return(ezdyn_datatable(data))
    }

    if ("Notes" %in% names(data) && "Description" %in% names(data)) {
        data <- data |>
            dplyr::mutate(
                Notes = zoo::na.fill(Notes, ""),
                Description = kableExtra::text_spec(Description, format = "html", tooltip = Notes)
            ) |>
            dplyr::select(-Notes)
    }

    tbl <- ezdyn_kable_striped(data, bold_col = "Symbol")
    if (!is.null(as_of) && "Value" %in% names(data)) {
        tbl <- tbl |>
            kableExtra::footnote(sprintf("As of %s. Values are rounded to %s decimal places.", as_of, dp))
    }

    tbl
}

#' Render KaTeX parameter table from model object
#'
#' Deprecated: use [get_param_table()] instead. This wrapper simply calls
#' `get_param_table(M_, type, mode = "katex", dp, as_of)` and is kept only so
#' existing callers (e.g. Rmd reports) keep working unchanged.
#'
#' @param M_ Model object containing `param_table`.
#' @param type Optional value to filter by `Type` column.
#' @param dp Number of decimal places for rounding numeric values.
#' @param as_of Optional timestamp text appended as a table footnote.
#'
#' @return HTML `kableExtra` table.
#' @export
#' @keywords internal
param_table_katex <- function(M_, type = NULL, dp = 3, as_of = NULL) {
    .Deprecated("get_param_table")
    get_param_table(M_, type = type, mode = "katex", dp = dp, as_of = as_of)
}
