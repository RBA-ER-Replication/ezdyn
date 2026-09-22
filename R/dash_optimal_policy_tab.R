# Optimal policy ------------------------------------------------------------
#
# This module owns optimal-policy inputs and session-local IRF caching. ezdyn
# owns the optimisation and returns labelled, rescaled paths.

#' Return available policy response choices from model metadata.
#'
#' @param model Full policy-model MOO pair.
#' @return Named character vector from display names to model codes.
optimal_policy_response_choices <- function(model) {
  metadata <- model$M_$varmeta
  keep <- !is.na(metadata$dynare_name) & metadata$dynare_name != "" &
    !is.na(metadata$display_name) & metadata$display_name != ""
  stats::setNames(metadata$dynare_name[keep], metadata$display_name[keep])
}

# Return display names for selected policy-model response codes.
optimal_policy_response_display_names <- function(model, variables) {
  choices <- optimal_policy_response_choices(model)
  unname(names(choices)[match(variables, choices)])
}

#' Return the configured loss variables offered by the Optimal Policy tab.
#'
#' @param config Validated dashboard configuration.
#' @return Named character vector.
optimal_policy_loss_choices <- function(config) {
  config$optimal_policy$loss_variable_choices
}

# Return the initial policy plot range as two years before and after the forecast start.
optimal_policy_default_plot_range <- function(timeline, policy_end) {
  c(
    max(timeline$data_start, seq(timeline$forecast_start, by = "-1 year", length.out = 3)[[3]]),
    min(policy_end, seq(timeline$forecast_start, by = "1 year", length.out = 5)[[5]])
  )
}

# Return a generic Optimal Policy description shown when no
# `optimal_policy$description_ui` configuration hook is supplied.
optimal_policy_generic_description <- function() {
  shiny::tags$p("This tab lets you generate optimal policy paths under a range of assumptions.")
}

# Build the selected loss-variable weights and targets in two columns.
optimal_policy_loss_inputs <- function(ns, config) {
  choices <- optimal_policy_loss_choices(config)
  target_defaults <- config$optimal_policy$loss_variable_defaults
  weight_defaults <- config$optimal_policy$loss_variable_weight_defaults
  lapply(names(choices), function(label) {
    variable <- choices[[label]]
    target_default <- if (variable %in% names(target_defaults)) target_defaults[[variable]] else 0
    weight_default <- if (variable %in% names(weight_defaults)) weight_defaults[[variable]] else 1
    shiny::conditionalPanel(
      condition = sprintf("input.loss_variables && input.loss_variables.indexOf('%s') !== -1", variable),
      ns = ns,
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns(paste0("weight_", optimal_policy_widget_id(variable))), paste(label, "weight"), weight_default, min = 0)),
        shiny::column(6, shiny::numericInput(ns(paste0("target_", optimal_policy_widget_id(variable))), paste(label, "target"), target_default))
      )
    )
  })
}

# Return the configured level variables available for policy constraints.
optimal_policy_constraint_choices <- function(config) {
  config$optimal_policy$constraint_variable_choices
}

# Sanitize a variable identifier (which may now be a display name containing
# spaces/parentheses, e.g. "Inflation (year-ended)") into a safe Shiny widget
# id fragment. Used consistently at both id-construction and id-lookup sites
# so display names, not just dynare codes, can be used for loss/constraint
# choices.
optimal_policy_widget_id <- function(variable) {
  gsub("[^A-Za-z0-9]+", "_", variable)
}

# Return a namespaced input identifier for one constraint-table field.
optimal_policy_constraint_id <- function(variable, direction, field) {
  paste("constraint", optimal_policy_widget_id(variable), direction, field, sep = "_")
}

# Build the reviewed multi-variable policy-constraint table.
optimal_policy_constraint_table <- function(ns, policy_dates, config) {
  choices <- optimal_policy_constraint_choices(config)
  periods <- stats::setNames(seq_along(policy_dates), format(policy_dates, "%b %Y"))
  rows <- unlist(lapply(names(choices), function(label) {
    variable <- choices[[label]]
    lapply(c("lower", "upper"), function(direction) {
      prefix <- function(field) ns(optimal_policy_constraint_id(variable, direction, field))
      shiny::tags$tr(
        shiny::tags$td(label),
        shiny::tags$td(tools::toTitleCase(direction)),
        shiny::tags$td(shiny::numericInput(prefix("value"), NULL, 0, width = "85px")),
        shiny::tags$td(shiny::selectInput(prefix("start"), NULL, periods, width = "105px")),
        shiny::tags$td(shiny::selectInput(prefix("end"), NULL, periods, width = "105px")),
        shiny::tags$td(shiny::checkboxInput(prefix("enabled"), NULL, FALSE))
      )
    })
  }), recursive = FALSE)
  shiny::tags$table(
    class = "table table-condensed",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th("Variable"), shiny::tags$th("Bound"), shiny::tags$th("Value"),
      shiny::tags$th("Start"), shiny::tags$th("End"), shiny::tags$th("On/off")
    )),
    shiny::tags$tbody(rows)
  )
}

#' Construct selected policy strategies from module inputs.
#'
#' @param input Shiny input values.
#' @param constraints Policy constraint list.
#' @param config Validated dashboard configuration.
#' @return List of strategy specifications for [get_policy_scenario()].
optimal_policy_strategies <- function(input, constraints, config) {
  modes <- c("commit", "timeless", "disc")
  modes <- modes[modes %in% input$strategy_modes]
  if ("disc" %in% modes && input$loss_horizon != input$instrument_horizon) {
    stop("Discretion requires equal loss and optimisation horizons.", call. = FALSE)
  }
  lapply(modes, function(mode) {
    list(
      output_name = switch(
        mode,
        timeless = "Optimal (DINGO, commitment)",
        disc = "Optimal (DINGO, discretion)"
      ),
      output_variables = input$response_variables,
      loss_variables = paste0(input$loss_variables, "_gap_loss"),
      loss_weights = vapply(input$loss_variables, function(variable) input[[paste0("weight_", optimal_policy_widget_id(variable))]], numeric(1)),
      discount_factor = input$discount_factor,
      T_loss = input$loss_horizon,
      T_instrument = input$instrument_horizon,
      commit = mode,
      instrument_variable = config$optimal_policy$instrument_variable,
      instrument_shock = config$optimal_policy$instrument_shock,
      constraints = constraints
    )
  })
}

#' Build enabled ELB constraints from module inputs.
#'
#' @param input Shiny input values.
#' @param config Validated dashboard configuration.
#' @return List of policy constraints.
optimal_policy_constraints <- function(input, config) {
  constraints <- list()
  if (isTRUE(input$elb)) {
    constraints[[length(constraints) + 1]] <- list(
      direction = "lower",
      variable = config$optimal_policy$elb_variable,
      period = seq_len(min(input$loss_horizon, input$instrument_horizon)),
      value = 0.1,
      historical = TRUE
    )
  }
  for (variable in unname(optimal_policy_constraint_choices(config))) {
    for (direction in c("lower", "upper")) {
      id <- function(field) optimal_policy_constraint_id(variable, direction, field)
      if (!isTRUE(input[[id("enabled")]])) next
      start <- as.integer(input[[id("start")]])
      end <- as.integer(input[[id("end")]])
      if (start > end || end > input$loss_horizon) {
        stop("Constraint periods must be ordered and within the loss evaluation horizon.", call. = FALSE)
      }
      constraints[[length(constraints) + 1]] <- list(
        direction = direction,
        variable = variable,
        period = seq.int(start, end),
        value = input[[id("value")]],
        historical = FALSE
      )
    }
  }
  constraints
}

#' Add selected target-gap variables to an augmented policy baseline.
#'
#' @param baseline Model-code baseline.
#' @param variables Selected level loss variables.
#' @param targets Named numeric targets.
#' @return Baseline with `_gap_loss` columns.
optimal_policy_add_gaps <- function(baseline, variables, targets) {
  for (variable in variables) {
    if (!(variable %in% names(baseline))) {
      stop(sprintf("Policy baseline is missing `%s`.", variable), call. = FALSE)
    }
    baseline[[paste0(variable, "_gap_loss")]] <- baseline[[variable]] - targets[[variable]]
  }
  baseline
}

#' Build the Optimal Policy tab UI.
#'
#' @param id Module namespace.
#' @param config Validated dashboard configuration.
#' @return Shiny UI.
UITab_OptimalPolicy <- function(id, config) {
  ns <- shiny::NS(id)
  policy <- config$optimal_policy
  model <- config$models[[policy$policy_model]]
  choices <- optimal_policy_response_choices(model)
  loss_choices <- optimal_policy_loss_choices(config)
  policy_dates <- seq(policy$forecast_start, policy$forecast_end, by = "quarter")
  plot_range <- optimal_policy_default_plot_range(config$timeline, policy$forecast_end)
  description <- if (is.function(policy$description_ui)) policy$description_ui() else optimal_policy_generic_description()

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 5,
      description,
      shiny::h4("Graph details"),
      shiny::selectizeInput(ns("response_variables"), "Response variables", choices, multiple = TRUE, selected = intersect(policy$default_response_variables, choices)),
      shiny::h4("Model details"),
      shiny::numericInput(ns("lambda"), shiny::tagList("Anticipation Parameter", help_button(ns("help_cd"))), policy$lambda, min = 0, max = 1, step = 0.1),
      shiny::h4("Strategy details"),
      shiny::checkboxGroupInput(
        ns("strategy_modes"),
        shiny::tagList("Strategy", help_button(ns("help_strategy"))),
        c("Commitment" = "timeless", "Discretion" = "disc"),
        selected = c("timeless", "disc")
      ),
      shiny::selectizeInput(ns("loss_variables"), shiny::tagList("Loss function variables", help_button(ns("help_loss_variables"))), loss_choices, multiple = TRUE, selected = intersect(policy$default_loss_variables, loss_choices)),
      optimal_policy_loss_inputs(ns, config),
      dashboard_plot_mode_controls(ns, advanced_controls = shiny::tagList(
        shiny::selectInput(
          ns("baseline_name"),
          "Baseline:",
          choices = names(config$baselines),
          selected = config$default_baseline
        ),
        shiny::tags$p(shiny::strong("Active baseline: "), shiny::textOutput(ns("active_baseline"), inline = TRUE)),
        shiny::numericInput(ns("discount_factor"), "Discount factor", 0.99, min = 0, max = 1),
        shiny::numericInput(ns("loss_horizon"), shiny::tagList("Loss evaluation horizon", help_button(ns("help_loss_horizon"))), min(40L, length(policy_dates)), min = 1, max = length(policy_dates)),
        shiny::numericInput(ns("instrument_horizon"), shiny::tagList("Optimisation horizon", help_button(ns("help_instrument_horizon"))), min(40L, length(policy_dates)), min = 1, max = length(policy_dates)),
        shiny::checkboxInput(ns("elb"), "ELB", FALSE),
        shiny::tags$label(shiny::tagList("Constraints", help_button(ns("help_constraints"))), class = "control-label"),
        optimal_policy_constraint_table(ns, policy_dates, config),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] && input['%s'].indexOf('timeless') !== -1", ns("strategy_modes"), ns("strategy_modes")),
          shiny::sliderInput(ns("preloss_start"), shiny::tagList("Commitment start", help_button(ns("help_commitment_start"))), min = policy$preloss_start, max = policy$forecast_start, value = policy$preloss_start, timeFormat = "%Y-%m-%d")
        ),
        shiny::sliderInput(ns("date_range"), "Plot range", min = config$timeline$data_start, max = policy$forecast_end, value = plot_range),
        shinyWidgets::prettySwitch(ns("show_table"), "Show table", FALSE)
      ))
    ),
    shiny::mainPanel(
      width = 7,
      style = "padding: 0 24px;",
      shiny::uiOutput(ns("message")),
      shiny::fluidRow(
        shiny::column(
          width = 8,
          shiny::actionButton(ns("run"), "Generate graph", class = "btn-primary"),
          shiny::tags$div(
            class = "alt-path-plot optimal-policy-plot",
            shiny::tags$div(
              class = "optimal-policy-loading",
              shiny::tags$div(class = "optimal-policy-spinner"),
              shiny::tags$span("Generating optimal policy graph…")
            ),
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
          shiny::conditionalPanel(sprintf("input['%s'] === true", ns("show_table")), shiny::tableOutput(ns("table")))
        )
      ),
      shiny::tags$style(shiny::HTML(".optimal-policy-plot { position: relative; } .optimal-policy-loading { display: none; position: absolute; top: 0; right: 0; bottom: 0; left: 0; z-index: 10; align-items: center; justify-content: center; gap: 10px; background: rgba(255, 255, 255, 0.8); font-weight: 600; } .shiny-busy .optimal-policy-loading { display: flex; } .optimal-policy-spinner { width: 22px; height: 22px; border: 3px solid #d9d9d9; border-top-color: #005a9c; border-radius: 50%; animation: optimal-policy-spin 0.8s linear infinite; } @keyframes optimal-policy-spin { to { transform: rotate(360deg); } }"))
    )
  )
}

#' Serve the Optimal Policy tab.
#'
#' @param id Module namespace.
#' @param config Validated dashboard configuration.
#' @return Invisibly, `NULL`.
ServerTab_OptimalPolicy <- function(id, config) {
  shiny::moduleServer(id, function(input, output, session) {
    dashboard_sync_compatibility_mode(input, session)
    ns <- session$ns
    policy <- config$optimal_policy
    policy_model <- config$models[[policy$policy_model]]
    cache <- new.env(parent = emptyenv())
    baseline_registry <- shiny::reactiveVal(config$baselines)
    active_baseline_name <- shiny::reactive({
      choices <- names(baseline_registry())
      shiny::req(length(choices) > 0)
      if (input$baseline_name %in% choices) input$baseline_name else config$default_baseline
    })
    active_baseline <- shiny::reactive(baseline_registry()[[active_baseline_name()]])
    prepared_baseline <- shiny::reactive({
      baseline <- active_baseline()
      baseline <- policy$prepare_baseline(baseline, policy_model, policy)
      policy$extend_baseline(baseline, policy)
    })
    output$message <- shiny::renderUI(NULL)
    output$active_baseline <- shiny::renderText(active_baseline_name())
    shiny::observeEvent(input$help_cd, {
      shiny::showModal(dash_help_modal(policy$help$cd, "Anticipation parameter"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$help_strategy, {
      shiny::showModal(dash_help_modal(policy$help$strategy, "Strategy"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$help_loss_variables, {
      shiny::showModal(dash_help_modal(policy$help$loss_variables, "Loss function variables"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$help_loss_horizon, {
      shiny::showModal(dash_help_modal(policy$help$loss_horizon, "Loss evaluation horizon"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$help_instrument_horizon, {
      shiny::showModal(dash_help_modal(policy$help$instrument_horizon, "Optimisation horizon"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$help_constraints, {
      shiny::showModal(dash_help_modal(policy$help$constraints, "Constraints"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$help_commitment_start, {
      shiny::showModal(dash_help_modal(policy$help$commitment_start, "Commitment start"))
    }, ignoreInit = TRUE)

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

    output$download_template <- shiny::downloadHandler(
      filename = function() "baseline_template.xlsx",
      content = function(file) alt_paths_baseline_template(file, active_baseline())
    )

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

    result <- shiny::eventReactive(input$run, {
      shiny::validate(shiny::need(length(input$response_variables) > 0, "Select at least one response variable."), shiny::need(length(input$strategy_modes) > 0, "Select at least one strategy."))
      loss_variables <- input$loss_variables
      targets <- stats::setNames(vapply(loss_variables, function(variable) input[[paste0("target_", optimal_policy_widget_id(variable))]], numeric(1)), loss_variables)
      baseline <- optimal_policy_add_gaps(prepared_baseline(), loss_variables, targets)
      constraints <- optimal_policy_constraints(input, config)
      strategies <- optimal_policy_strategies(input, constraints, config)
      run_one <- function(model, strategy, use_cd, lambda) {
        variables <- unique(c(strategy$output_variables, strategy$loss_variables, vapply(strategy$constraints, `[[`, character(1), "variable"), strategy$instrument_variable))
        preloss <- if (strategy$commit == "timeless") length(seq(input$preloss_start, policy$forecast_start, by = "quarter")) - 1L else 0L
        horizon <- max(length(seq(policy$forecast_start, policy$forecast_end, by = "quarter")), strategy$T_loss + preloss)
        instrument_horizon <- strategy$T_instrument + preloss
        key <- paste(model$M_$model_name, paste(variables, collapse = "|"), horizon, instrument_horizon, use_cd, lambda, sep = "::")
        irfs <- if (exists(key, envir = cache, inherits = FALSE)) get(key, envir = cache) else {
          value <- build_policy_irfs(model, variables, horizon, instrument_horizon, use_cd, lambda, strategy$instrument_shock)
          assign(key, value, envir = cache)
          value
        }
        get_policy_scenario(strategy, baseline, model, use_cd, lambda, policy$forecast_start, policy$forecast_end, policy$loss_variable_vintages, input$preloss_start, irfs)
      }
      scenarios <- lapply(strategies, run_one, model = policy_model, use_cd = policy$use_cd, lambda = input$lambda)
      data <- dplyr::bind_rows(scenarios)
      if (!is.null(policy$comparison_model)) {
        comparison <- config$models[[policy$comparison_model]]
        reference <- strategies[[1]]
        reference$commit <- "commit"
        reference$output_name <- sprintf("Optimal (%s)", policy$comparison_model)
        data <- dplyr::bind_rows(data, run_one(comparison, reference, FALSE, 0))
      }
      list(
        data = data,
        baseline = baseline,
        response_variables = input$response_variables,
        date_range = input$date_range
      )
    }, ignoreInit = TRUE)
    selected <- shiny::reactive({
      data <- result()$data
      date_range <- result()$date_range
      data[data$date >= date_range[[1]] & data$date <= date_range[[2]], , drop = FALSE]
    })
    plotted <- shiny::reactive({
      data <- selected()
      shiny::validate(shiny::need(nrow(data) > 0, "No policy output matches the selected plot range."))
      plot_pretty(
        data,
        display_names = optimal_policy_response_display_names(policy_model, result()$response_variables),
        title = "Optimal Policy",
        baseline_colour = "royalblue",
        date_range = result()$date_range,
        plotter = dashboard_selected_plotter(input)
      )
    })
    dashboard_bind_plot_mode(
      output,
      shiny::reactive(plotted()$graph),
      shiny::reactive(isTRUE(input$use_compatibility_plot)),
      plotter = shiny::reactive(dashboard_selected_plotter(input))
    )
    output$table <- shiny::renderTable(plotted()$graph_data, rownames = FALSE)
    output$download_data <- shiny::downloadHandler(filename = function() paste0(plotted()$graph_fname, ".xlsx"), content = function(file) writexl::write_xlsx(plotted()$graph_data, file))
    output$download_graph <- shiny::downloadHandler(filename = function() paste0(plotted()$graph_fname, ".svg"), content = function(file) ezdyn_ggsave(file, plotted()$graph, plotter = dashboard_selected_plotter(input)))
  })
}
