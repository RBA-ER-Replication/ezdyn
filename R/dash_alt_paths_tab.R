# Alternative policy paths -------------------------------------------------
#
# This module owns Alternative Paths UI, reactive state, and session-local
# uploaded-baseline state. Public ezdyn APIs own baseline import, path
# calculation, year-ended transformations, plotting, and exports.

# 1. Small UI and display-data helpers -------------------------------------

#' Return model response display names with baseline columns.
#'
#' @param models Named list of selected model pairs.
#' @param baseline Canonical baseline.
#'
#' @return Named character vector of common display names.
alt_paths_response_choices <- function(models, baseline) {
  choices <- irf_common_choices(models, irf_model_display_names)
  irf_choices(intersect(unname(choices), setdiff(names(baseline), "date")))
}

#' Return the initial Alternative Paths plot range.
#'
#' @param timeline Validated dashboard timeline.
#'
#' @return Two `Date` values for the plot start and end.
alt_paths_default_plot_range <- function(timeline) {
  # Keep the default visible history bounded by data availability.
  two_years_before_forecast <- seq(
    timeline$forecast_start,
    by = "-1 year",
    length.out = 3
  )[[3]]
  c(max(timeline$data_start, two_years_before_forecast), timeline$forecast_end)
}

#' Return an Alternative Paths graph title from the selected responses.
#'
#' @param display_names Display names present in the plot data.
#'
#' @return One plot title.
alt_paths_plot_title <- function(display_names) {
  # A single response is clearer when named in the graph title; multi-panel
  # plots retain the generic title.
  display_names <- unique(display_names)
  if (length(display_names) == 1) {
    sprintf("%s — Response to Alternative Policy Path", display_names)
  } else {
    "Responses to Alternative Policy Path"
  }
}

#' Prepare Alternative Paths export data for the on-screen table.
#'
#' @param export_data Wide data returned by [format_alt_paths_long()].
#' @param forecast_start First forecast quarter.
#'
#' @return Data frame without history, mnemonic, or baseline-name columns.
alt_paths_table_data <- function(export_data, forecast_start) {
  # Export data retains history and all identifiers. The visible table is a
  # compact forecast-only view, so filter date columns here rather than alter
  # the export contract.
  date_columns <- names(export_data)[grepl("^\\d{4}-\\d{2}-\\d{2}$", names(export_data))]
  forecast_columns <- date_columns[as.Date(date_columns) >= forecast_start]
  identifier_columns <- setdiff(names(export_data), c("Mnemonic", "Baseline Name", date_columns))

  export_data[, c(identifier_columns, forecast_columns), drop = FALSE]
}

#' Validate and normalize one uploaded baseline name.
#'
#' @param name User-supplied baseline name.
#' @param existing_names Names already registered in the current session.
#'
#' @return One trimmed, unique baseline name.
alt_paths_upload_name <- function(name, existing_names) {
  name <- trimws(name)
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    stop("Provide a name for the uploaded baseline.", call. = FALSE)
  }
  if (name %in% existing_names) {
    stop(sprintf("A baseline named `%s` already exists.", name), call. = FALSE)
  }

  name
}

#' Build the configured-baseline upload trigger.
#'
#' @param ns A Shiny namespace function.
#' @param button_label Upload trigger label.
#'
#' @return Shiny UI.
alt_paths_upload_ui <- function(ns, button_label = "Provide Custom Baseline") {
  shiny::actionButton(
    ns("open_upload"),
    button_label,
    icon = shiny::icon("file-upload")
  )
}

#' Build an upload template from a canonical baseline.
#'
#' @param file Destination `.xlsx` file.
#' @param baseline Canonical `ezdyn_baseline` object.
#'
#' @return Invisibly, `NULL`.
alt_paths_baseline_template <- function(file, baseline) {
  if (!inherits(baseline, "ezdyn_baseline")) {
    stop("`baseline` must be an `ezdyn_baseline` object.", call. = FALSE)
  }

  writexl::write_xlsx(baseline, file)
}

# 2. Alternative Paths user interface --------------------------------------

#' Build Alternative Policy Paths UI from configuration.
#'
#' @param id Module namespace.
#' @param config Validated dashboard configuration.
#'
#' @return Shiny UI.
UITab_AltPaths <- function(id, config) {
  # 2.1. Derive all initial values from the explicit dashboard configuration.
  ns <- shiny::NS(id)
  baseline <- config$baselines[[config$default_baseline]]
  forecast_dates <- alt_paths_forecast_dates(config)
  instrument_values <- alt_paths_instrument_values(
    baseline,
    forecast_dates,
    config$instrument_display_name
  )
  model_choices <- irf_choices(config$alt_paths$models)
  selected_models <- irf_preserve_selection(config$alt_paths$defaults$models, model_choices)
  responses <- alt_paths_response_choices(config$models[selected_models], baseline)
  plot_range <- alt_paths_default_plot_range(config$timeline)

  shiny::sidebarLayout(
    # 2.2. The sidebar owns model, policy-path, and display selections.
    shiny::sidebarPanel(
      width = 5,
      shiny::selectizeInput(
        ns("models"),
        "Model(s):",
        choices = model_choices,
        selected = selected_models,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        ns("display_names"),
        "Response variable:",
        choices = responses,
        selected = irf_preserve_selection(config$irf$defaults$display_names, responses),
        multiple = TRUE
      ),
      shiny::checkboxInput(
        ns("use_cd"),
        shiny::tagList(
          "Use anticipated shocks when supported ",
          help_button(ns("help_anticipated"))
        ),
        value = TRUE
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === true", ns("use_cd")),
        shiny::numericInput(
          ns("lambda"),
          shiny::tagList(
            "Cognitive discounting parameter: ",
            help_button(ns("help_cognitive_discounting"))
          ),
          value = 0.8,
          min = 0,
          max = 1,
          step = 0.05
        )
      ),
      shiny::tags$label(class = "control-label", "Cash Rate path:"),
      shiny::fluidRow(
        class = "slider-container",
        shiny::uiOutput(ns("cash_rate_sliders"))
      ),
      dashboard_plot_mode_controls(
        ns,
        advanced_input = "show_advanced",
        advanced_controls = shiny::tagList(
          shiny::selectInput(
            ns("baseline_name"),
            "Baseline:",
            choices = names(config$baselines),
            selected = config$default_baseline
          ),
          shiny::tags$p(shiny::strong("Active baseline: "), shiny::textOutput(ns("active_baseline"), inline = TRUE)),
          shiny::sliderInput(
            ns("date_range"),
            "Plot range:",
            min = config$timeline$data_start,
            max = config$timeline$forecast_end,
            value = plot_range
          ),
          shinyWidgets::prettySwitch(ns("show_table"), "Show table", value = FALSE)
        )
      )
    ),
    # 2.3. The main panel owns rendered output, action controls, and the
    # optional compact forecast table.
    shiny::mainPanel(
      width = 7,
      style = "padding: 0 24px;",
      shiny::uiOutput(ns("message")),
      shiny::fluidRow(
        shiny::column(
          width = 8,
          shiny::tags$div(
            class = "alt-path-plot",
            dashboard_plot_mode_ui(ns, height = 400)
            )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 8,
          shiny::wellPanel(
            class = "alt-path-actions",
            shiny::downloadButton(ns("download_data"), "Download Data"),
            shiny::downloadButton(ns("download_graph"), "Download Graph"),
            alt_paths_upload_ui(ns)
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] === true", ns("show_table")),
            shiny::tableOutput(ns("table"))
          )
        )
      )
    )
  )
}

# 3. Alternative Paths server ----------------------------------------------

#' Serve Alternative Policy Paths using configuration and public ezdyn APIs.
#'
#' @param id Module namespace.
#' @param config Validated dashboard configuration.
#'
#' @return Invisibly, `NULL`.
ServerTab_AltPaths <- function(id, config) {
  shiny::moduleServer(id, function(input, output, session) {
    dashboard_sync_compatibility_mode(input, session)
    # 3.1. Keep configured and uploaded baselines in a module-local registry.
    # An upload must never mutate the configuration shared by other sessions.
    ns <- session$ns
    forecast_dates <- alt_paths_forecast_dates(config)
    baseline_registry <- shiny::reactiveVal(config$baselines)
    active_baseline_name <- shiny::reactive({
      choices <- names(baseline_registry())
      shiny::req(length(choices) > 0)
      if (input$baseline_name %in% choices) input$baseline_name else config$default_baseline
    })
    active_baseline <- shiny::reactive(baseline_registry()[[active_baseline_name()]])
    selected_models <- shiny::reactive({
      shiny::req(input$models)
      config$models[input$models]
    })
    responses <- shiny::reactive(alt_paths_response_choices(selected_models(), active_baseline()))
    instrument_values <- shiny::reactive(alt_paths_instrument_values(
      active_baseline(),
      forecast_dates,
      config$instrument_display_name
    ))

    # 3.2. Render state derived from the active baseline and show contextual
    # help for the anticipated-shock/cognitive-discounting controls.
    output$active_baseline <- shiny::renderText(active_baseline_name())
    output$cash_rate_sliders <- shiny::renderUI(cash_rate_sliders(
      session$ns,
      forecast_dates,
      instrument_values(),
      config$slider_preferences
    ))
    shiny::observeEvent(input$help_anticipated, {
      shiny::showModal(dash_help_modal(config$help$anticipated_shocks, "Anticipated shocks"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$help_cognitive_discounting, {
      shiny::showModal(dash_help_modal(config$help$anticipated_shocks, "Cognitive discounting parameter"))
    }, ignoreInit = TRUE)

    # 3.3. Refresh compatible response selections whenever the selected
    # model(s) change - a response common to the previous selection may no
    # longer be common to the new one, and vice versa.
    shiny::observeEvent(input$models, {
      shiny::updateSelectizeInput(
        session,
        "display_names",
        choices = responses(),
        selected = irf_preserve_selection(input$display_names, responses()),
        server = TRUE
      )
    }, ignoreInit = TRUE)

    # 3.4. Reset all cash-rate controls and compatible response selections
    # whenever the active baseline changes.
    shiny::observeEvent(active_baseline_name(), {
      values <- instrument_values()
      for (index in seq_along(values)) {
        shinyWidgets::updateNoUiSliderInput(
          session,
          alt_paths_slider_id(index),
          range = values[[1]] + c(
            -config$slider_preferences$r_slider_range,
            config$slider_preferences$r_slider_range
          ),
          value = values[[index]]
        )
      }
      shiny::updateSelectizeInput(
        session,
        "display_names",
        choices = responses(),
        selected = irf_preserve_selection(input$display_names, responses()),
        server = TRUE
      )
    }, ignoreInit = TRUE)

    # 3.5. Register +/- actions for every configured forecast quarter. `local()`
    # freezes the loop index for the observer closures.
    for (index in seq_along(forecast_dates)) {
      local({
        slider_index <- index
        shiny::observeEvent(input[[alt_paths_slider_increment_id(slider_index)]], {
          shinyWidgets::updateNoUiSliderInput(
            session,
            alt_paths_slider_id(slider_index),
            value = input[[alt_paths_slider_id(slider_index)]] + config$slider_preferences$cr_button_step
          )
        }, ignoreInit = TRUE)
        shiny::observeEvent(input[[alt_paths_slider_decrement_id(slider_index)]], {
          shinyWidgets::updateNoUiSliderInput(
            session,
            alt_paths_slider_id(slider_index),
            value = input[[alt_paths_slider_id(slider_index)]] - config$slider_preferences$cr_button_step
          )
        }, ignoreInit = TRUE)
      })
    }

    # 3.6. Convert dynamic slider input to one named wide path only after every
    # browser input has initialized. Pass that path and explicit config values
    # to ezdyn, which owns the model calculation.
    alt_paths <- shiny::reactive({
      slider_values <- lapply(seq_along(forecast_dates), function(index) {
        input[[alt_paths_slider_id(index)]]
      })
      shiny::req(all(vapply(
        slider_values,
        function(value) {
          is.numeric(value) && length(value) == 1 && !is.na(value) && is.finite(value)
        },
        logical(1)
      )))
      values <- vapply(slider_values, function(value) value[[1]], numeric(1))
      alt_paths_slider_path(forecast_dates, values)
    })
    calculated_paths <- shiny::reactive({
      shiny::validate(shiny::need(length(input$models) > 0, "Select at least one model."))
      result <- tryCatch(
        get_alt_paths(
          alt_paths = alt_paths(),
          models = selected_models(),
          baseline = active_baseline(),
          use_cd = isTRUE(input$use_cd),
          lambda = if (isTRUE(input$use_cd)) input$lambda else 0,
          instrument_display_name = config$instrument_display_name,
          instrument_shock_name = config$instrument_shock_name,
          forecast_start = config$timeline$forecast_start,
          forecast_end = config$timeline$forecast_end,
          data_start = config$timeline$data_start
        ),
        error = function(e) e
      )
      failed <- inherits(result, "error")
      shiny::validate(shiny::need(!failed, if (failed) conditionMessage(result)))
      result
    })

    # 3.7. Restrict outputs to selected display names.
    selected_paths <- shiny::reactive({
      paths <- calculated_paths()
      selected_display_names <- intersect(input$display_names, unique(paths$display_name))
      paths |>
        dplyr::filter(
          display_name %in% selected_display_names
        )
    })
    displayed_paths <- shiny::reactive({
      selected_paths() |>
        dplyr::filter(
          date >= input$date_range[[1]],
          date <= input$date_range[[2]]
        )
    })

    # 3.8. Keep plotting, downloads, and the compact table as distinct views:
    # the graph obeys the selected plot range, exports retain history, and the
    # on-screen table keeps forecast quarters only.
    plotted_paths <- shiny::reactive({
      data <- displayed_paths()
      shiny::validate(shiny::need(nrow(data) > 0, "No alternative-path output matches the selected variables and dates."))
      plot_pretty(
        data,
        display_names = unique(data$display_name),
        title = alt_paths_plot_title(data$display_name),
        baseline_name = "Baseline",
        baseline_colour = "royalblue",
        plotter = dashboard_selected_plotter(input)
      )
    })
    export_paths <- shiny::reactive({
      output_vars <- selected_paths() |>
        dplyr::filter(path_name != "Baseline") |>
        dplyr::pull(dynare_name) |>
        unique()
      format_alt_paths_long(
        pretty_df = selected_paths(),
        output_vars = output_vars,
        baseline_name_value = active_baseline_name(),
        forecasts_start_date = config$timeline$forecast_start,
        output_start_date = config$timeline$data_start,
        output_end_date = config$timeline$forecast_end
      )
    })
    table_paths <- shiny::reactive({
      output_vars <- selected_paths() |>
        dplyr::filter(path_name != "Baseline") |>
        dplyr::pull(dynare_name) |>
        unique()
      table_export <- format_alt_paths_long(
        pretty_df = selected_paths(),
        output_vars = output_vars,
        baseline_name_value = active_baseline_name(),
        forecasts_start_date = config$timeline$forecast_start,
        output_start_date = config$timeline$data_start,
        output_end_date = config$timeline$forecast_end,
        include_baseline_path = TRUE
      )
      alt_paths_table_data(table_export, config$timeline$forecast_start)
    })

    # 3.9. Bind the shared plot renderer and download endpoints.
    output$message <- shiny::renderUI(NULL)
    dashboard_bind_plot_mode(
      output = output,
      graph = shiny::reactive(plotted_paths()$graph),
      compatibility_mode = shiny::reactive(isTRUE(input$use_compatibility_plot)),
      plotter = shiny::reactive(dashboard_selected_plotter(input))
    )
    output$table <- shiny::renderTable(table_paths(), rownames = FALSE)
    output$download_data <- shiny::downloadHandler(
      filename = function() paste0(plotted_paths()$graph_fname, ".xlsx"),
      content = function(file) writexl::write_xlsx(export_paths(), file)
    )
    output$download_graph <- shiny::downloadHandler(
      filename = function() paste0(plotted_paths()$graph_fname, ".svg"),
      content = function(file) ezdyn_ggsave(file, plotted_paths()$graph, plotter = dashboard_selected_plotter(input))
    )

    # 3.10. Build the upload modal from allowed configuration values. Model-input
    # workbooks are translated/rescaled by their selected model; default-template
    # workbooks use Display Names and are already in final units.
    shiny::observeEvent(input$open_upload, {
      source_choices <- config$baseline_upload$source_models
      format_choice_names <- list(
        shiny::tagList(
          "Default template ",
          shiny::tags$span(
            class = "fa fa-info-circle",
            title = paste(
              "This spreadsheet is in the same format as the template:",
              "it uses Display Names (e.g. 'Inflation' as opposed to 'pi_obs')",
              "and all variables are in the units",
              "used by Alternative Paths exercises."
            ),
            `aria-label` = "Default template format help"
          )
        ),
        shiny::tagList(
          "Model input ",
          shiny::tags$span(
            class = "fa fa-info-circle",
            title = paste(
              "This spreadsheet is in the format imported into the underlying model.",
              "Its column names use variable codes (e.g. 'pi_obs'); they will be transformed",
              "and any configured scaling needed",
              "to convert the spreadsheet to the default template format will be applied."
            ),
            `aria-label` = "Model input format help"
          )
        )
      )
      format_choice_values <- c("display", "source")
      if (!isTRUE(config$baseline_upload$allow_display_name_upload)) {
        format_choice_names <- format_choice_names[2]
        format_choice_values <- "source"
      }
      shiny::showModal(shiny::modalDialog(
        title = "Upload Custom Baseline Paths",
        shiny::fileInput(
          ns("uploaded_baseline"),
          "Upload a .xls or .xlsx file",
          accept = c(".xls", ".xlsx")
        ),
        shiny::textInput(ns("uploaded_baseline_name"), "Baseline name:"),
        shiny::radioButtons(
          ns("upload_interpretation"),
          "Spreadsheet format:",
          choiceNames = format_choice_names,
          choiceValues = format_choice_values,
          selected = format_choice_values[[1]]
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'source'", ns("upload_interpretation")),
          shiny::selectInput(
            ns("upload_source_model"),
            "Source model:",
            choices = source_choices,
            selected = config$baseline_upload$default_source_model
          )
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'display'", ns("upload_interpretation")),
          shiny::p("Values are retained in the default template units without rescaling.")
        ),
        shiny::downloadButton(ns("download_template"), "Download active baseline template"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("import_baseline"), "Import baseline", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    # The template always reflects the active canonical baseline.
    output$download_template <- shiny::downloadHandler(
      filename = function() "baseline_template.xlsx",
      content = function(file) alt_paths_baseline_template(file, active_baseline())
    )

    # 3.10. Import, validate coverage, and register a named uploaded baseline
    # within this session. Import or naming failures leave active state intact.
    shiny::observeEvent(input$import_baseline, {
      shiny::req(input$uploaded_baseline)
      source_label <- if (identical(input$upload_interpretation, "display")) {
        config$baseline_upload$default_source_model
      } else {
        input$upload_source_model
      }
      shiny::req(source_label)
      tryCatch({
        baseline_name <- alt_paths_upload_name(
          input$uploaded_baseline_name,
          names(baseline_registry())
        )
        imported <- import_baseline(
          input = input$uploaded_baseline$datapath,
          source_model = config$models[[source_label]],
          models = config$models
        )
        dashboard_validate_baseline_coverage(imported, config$timeline, baseline_name)
        updated_registry <- baseline_registry()
        updated_registry[[baseline_name]] <- imported
        baseline_registry(updated_registry)
        shiny::updateSelectInput(
          session,
          "baseline_name",
          choices = names(updated_registry),
          selected = baseline_name
        )
        shiny::removeModal()
      }, error = function(condition) {
        shiny::showNotification(condition$message, type = "error", duration = NULL)
      })
    })
  })
}
