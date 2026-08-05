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

#' Render KaTeX parameter table from model object
#'
#' @param M_ Model object containing `param_table`.
#' @param type Optional value to filter by `Type` column.
#' @param dp Number of decimal places for rounding numeric values.
#' @param as_of Optional timestamp text appended as a table footnote.
#'
#' @return HTML `kableExtra` table.
#' @export
param_table_katex <- function(M_, type = NULL, dp = 3, as_of = NULL) {
    if (is.null(M_$param_table)) {
        stop("`M_$param_table` not found. Load metadata with read_dynare/read_moo first.")
    }

    data <- M_$param_table
    if (!is.null(type) && "Type" %in% names(data)) {
        data <- data |>
            dplyr::filter(tolower(Type) == tolower(type))
    }

    if (nrow(data) == 0) {
        return(knitr::kable(data.frame(Message = "No matching parameters."), format = "html", escape = FALSE))
    }

    symbol_col <- "Symbol"
    description_col <- "Description"

    if (!"Value" %in% names(data)) {
        data <- data |>
            dplyr::mutate(Value = suppressWarnings(as.numeric(Value_Original)))
    }

    render_latex <- function(x) {
        x <- as.character(x)
        x <- stringr::str_replace_all(x, "^\\$|\\$$", "")
        x <- stringr::str_replace_all(x, "\\\\\\\\", "\\\\")
        vapply(
            x,
            function(xx) as.character(katex::katex_html(xx, displayMode = FALSE, output = "mathml")),
            character(1)
        )
    }

    out <- data |>
        dplyr::mutate(
            Symbol = render_latex(LaTeX_Symbol),
            Description = Description,
            Value = round(as.numeric(Value), digits = dp)
        )

    if ("Notes" %in% names(out)) {
        out <- out |>
            dplyr::mutate(
                Notes = zoo::na.fill(Notes, ""),
                Description = kableExtra::text_spec(Description, format = "html", tooltip = Notes)
            )
    }

    selected_cols <- c("Sector", "Symbol", "Description", "Value")
    selected_cols <- selected_cols[selected_cols %in% names(out)]
    out <- out |>
        dplyr::select(dplyr::all_of(selected_cols))

    tbl <- out |>
        knitr::kable("html", escape = FALSE) |>
        kableExtra::column_spec(which(names(out) == "Symbol"), bold = TRUE) |>
        kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)

    if ("Sector" %in% names(out)) {
        tbl <- tbl |>
            kableExtra::collapse_rows(columns = which(names(out) == "Sector"), valign = "top")
    }

    if (!is.null(as_of)) {
        tbl <- tbl |>
            kableExtra::footnote(sprintf("As of %s. Values are rounded to %s decimal places.", as_of, dp))
    }

    tbl
}
