# table_helpers.R
# Shared rendering helpers used by every "dictionary" table function
# (get_param_table(), get_variable_table(), get_shock_table()). Each of
# those functions builds its own mode-agnostic tibble, then hands it to one
# of these two renderers depending on the requested `mode`.

#' Render a data frame as a static, styled HTML table.
#'
#' Shared "katex"-mode renderer: a striped Bootstrap table, an optional bold
#' column, and Sector-collapsed row grouping when a `Sector` column exists.
#'
#' @param data Data frame to render. Values are shown as supplied; HTML
#'   (e.g. KaTeX-rendered symbols) is not escaped.
#' @param bold_col Optional column name to render in bold.
#'
#' @return HTML `kableExtra` table.
ezdyn_kable_striped <- function(data, bold_col = NULL) {
    tbl <- data |>
        knitr::kable("html", escape = FALSE) |>
        kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)

    if (!is.null(bold_col) && bold_col %in% names(data)) {
        tbl <- tbl |>
            kableExtra::column_spec(which(names(data) == bold_col), bold = TRUE)
    }
    if ("Sector" %in% names(data)) {
        tbl <- tbl |>
            kableExtra::collapse_rows(columns = which(names(data) == "Sector"), valign = "top")
    }

    tbl
}

#' Render a data frame as an interactive, searchable/sortable table.
#'
#' Shared "dt"-mode renderer. `escape = FALSE` so KaTeX-rendered HTML
#' symbols still display correctly.
#'
#' @param data Data frame to render.
#' @param ... Additional arguments passed to [DT::datatable()].
#'
#' @return A `DT::datatable` htmlwidget.
ezdyn_datatable <- function(data, ...) {
    DT::datatable(
        data,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 25, autoWidth = TRUE),
        escape = FALSE,
        ...
    )
}
