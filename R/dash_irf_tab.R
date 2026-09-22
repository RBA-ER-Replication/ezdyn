# Impulse response library -------------------------------------------------
#
# This module owns mode-specific Shiny state only. Generic selectors and solver
# adapters live in `dash_irf_helpers.R`/`dash_irf_custom_helpers.R`; curated
# scenario recipes live in the dashboard composition root.

#' Build the namespaced three-mode IRF tab UI.
#'
#' @param id Module namespace.
#' @param config Validated dashboard configuration.
#'
#' @return Shiny UI.
UITab_IRF <- function(id, config) {
  ns <- shiny::NS(id)
  modes <- irf_mode_choices(config)
  if (length(modes) == 0) {
    return(shiny::div(
      class = "alert alert-info",
      "No IRF modes are configured for this dashboard."
    ))
  }

  defaults <- config$irf$defaults
  scenarios <- config$irf$scenarios
  model_choices <- irf_choices(names(config$models))
  selected_models <- irf_preserve_selection(defaults$models, model_choices)
  selected_model_objects <- config$models[selected_models]
  shock_selector <- irf_shock_selector(selected_model_objects)
  response_choices <- irf_response_choices(selected_model_objects)
  common_display_choices <- response_choices$choices
  selected_shocks <- irf_default_shock_selection(shock_selector)
  selected_responses <- irf_preserve_selection(defaults$display_names, response_choices$values)
  scenario_choices <- if (!is.null(scenarios)) {
    irf_choices(scenarios$scenario[scenarios$model_name %in% selected_models])
  } else {
    character()
  }
  selected_scenario <- irf_preserve_selection(NULL, scenario_choices)
  scenario_subset <- if (is.null(scenarios)) {
    data.frame()
  } else {
    scenarios[
      scenarios$model_name %in% selected_models & scenarios$scenario %in% selected_scenario,
      ,
      drop = FALSE
    ]
  }
  scenario_response_choices <- irf_scenario_response_choices(scenario_subset)
  scenario_controls <- if ("scenario" %in% unname(modes)) {
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'scenario'", ns("mode")),
      shiny::selectInput(
        ns("scenario"),
        "Scenario:",
        choices = scenario_choices,
        selected = selected_scenario
      ),
      shiny::selectizeInput(
        ns("scenario_shocks"),
        "Scenario variant:",
        choices = irf_choices(scenario_subset$shock),
        selected = utils::head(irf_choices(scenario_subset$shock), 1),
        multiple = TRUE
      ),
      shiny::selectizeInput(
        ns("scenario_display_names"),
        "Response variable:",
        choices = scenario_response_choices$choices,
        selected = irf_preserve_selection(
          defaults$display_names,
          scenario_response_choices$values
        ),
        multiple = TRUE
      )
    )
  }

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 4,
      shiny::selectInput(ns("mode"), "Mode:", choices = modes, selected = unname(modes[[1]])),
      shiny::selectizeInput(
        ns("models"),
        "Model(s):",
        choices = model_choices,
        selected = selected_models,
        multiple = TRUE
      ),
      scenario_controls,
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === 'targeted'", ns("mode")),
        shiny::selectizeInput(
          ns("target_display_names"),
          "Response variables to plot:",
          choices = common_display_choices,
          selected = selected_responses,
          multiple = TRUE
        ),
        shiny::numericInput(
          ns("target_horizon"),
          "Horizon (quarters):",
          value = defaults$horizon,
          min = 1,
          step = 1
        ),
        shiny::tags$hr(),
        shiny::tags$div(class = "irf-subsection-title", "Shock and target"),
        shiny::selectizeInput(
          ns("target_shock"),
          "Shock(s), one per model:",
          choices = shock_selector$choices,
          selected = selected_shocks,
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("target_display_name"),
          "Target variable:",
          choices = common_display_choices,
          selected = utils::head(selected_responses, 1)
        ),
        shiny::numericInput(
          ns("target_value"),
          "Shock size (impact on first-quarter of target variable):",
          value = 1
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === 'direct'", ns("mode")),
        shiny::selectizeInput(
          ns("direct_shocks"),
          "Shock(s), one per model:",
          choices = shock_selector$choices,
          selected = selected_shocks,
          multiple = TRUE
        ),
        shiny::numericInput(
          ns("direct_horizon"),
          "Horizon (quarters):",
          value = defaults$horizon,
          min = 1,
          step = 1
        ),
        shiny::selectizeInput(
          ns("direct_display_names"),
          "Response variable:",
          choices = common_display_choices,
          selected = selected_responses,
          multiple = TRUE
        )
      ),
      dashboard_plot_mode_controls(
        ns,
        advanced_controls = shiny::tagList(
          shiny::textInput(ns("custom_title"), "Custom graph title:"),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] === 'targeted'", ns("mode")),
            shiny::checkboxInput(
              ns("target_use_cd"),
              "Use cognitive discounting when supported",
              defaults$use_cd
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] === true", ns("target_use_cd")),
              shiny::sliderInput(
                ns("target_lambda"),
                "Cognitive discounting parameter:",
                0,
                1,
                defaults$lambda,
                0.05
              )
            )
          )
        )
      )
    ),
    shiny::mainPanel(
      width = 8,
      shiny::uiOutput(ns("message")),
      shiny::fluidRow(
        shiny::column(width = 6, dashboard_plot_mode_ui(ns)),
        shiny::column(
          width = 1,
          shiny::h5("Downloads", style = "b"),
          shiny::downloadButton(ns("download_data"), "Download Data"),
          shiny::downloadButton(ns("download_graph"), "Download Graph")
        )
      )
    )
  )
}

#' Return a custom graph title when supplied, otherwise retain the generated title.
#'
#' @param custom_title User-supplied graph title.
#' @param generated_title Mode-specific graph title.
#'
#' @return One character string.
irf_title_or_default <- function(custom_title, generated_title) {
  if (is.null(custom_title) || !nzchar(trimws(custom_title))) {
    generated_title
  } else {
    trimws(custom_title)
  }
}

#' Serve the namespaced three-mode IRF tab.
#'
#' @param id Module namespace.
#' @param config Validated dashboard configuration.
#'
#' @return Invisibly, `NULL`.
ServerTab_IRF <- function(id, config) {
  shiny::moduleServer(id, function(input, output, session) {
    dashboard_sync_compatibility_mode(input, session)
    scenarios <- config$irf$scenarios
    defaults <- config$irf$defaults
    has_scenarios <- !is.null(scenarios) && nrow(scenarios) > 0

    selected_models <- shiny::reactive({
      shiny::req(input$models)
      config$models[input$models]
    })
    shock_selector <- shiny::reactive(irf_shock_selector(selected_models()))
    response_choices <- shiny::reactive(irf_response_choices(selected_models()))
    scenario_selector_data <- shiny::reactive({
      if (!has_scenarios) {
        return(NULL)
      }
      scenarios |>
        dplyr::filter(model_name %in% input$models)
    })
    scenario_data <- shiny::reactive({
      if (!has_scenarios) {
        return(NULL)
      }
      scenario_selector_data() |>
        dplyr::filter(scenario %in% input$scenario)
    })

    # 1. Preserve compatible selections when a model or scenario changes.
    shiny::observeEvent(input$models, {
      if (has_scenarios) {
        scenario_choices <- irf_choices(scenario_selector_data()$scenario)
        shiny::updateSelectInput(
          session,
          "scenario",
          choices = scenario_choices,
          selected = irf_preserve_selection(input$scenario, scenario_choices)
        )
      }
      shocks <- shock_selector()
      display_choices <- response_choices()
      shiny::updateSelectizeInput(
        session,
        "target_shock",
        choices = shocks$choices,
        selected = irf_preserve_shock_selection(input$target_shock, shocks),
        server = TRUE
      )
      shiny::updateSelectInput(
        session,
        "target_display_name",
        choices = display_choices$choices,
        selected = irf_preserve_selection(
          input$target_display_name,
          display_choices$values,
          defaults$display_names
        )
      )
      shiny::updateSelectizeInput(
        session,
        "target_display_names",
        choices = display_choices$choices,
        selected = irf_preserve_selection(input$target_display_names, display_choices$values, defaults$display_names),
        server = TRUE
      )
      shiny::updateSelectizeInput(
        session,
        "direct_shocks",
        choices = shocks$choices,
        selected = irf_preserve_shock_selection(input$direct_shocks, shocks),
        server = TRUE
      )
      shiny::updateSelectizeInput(
        session,
        "direct_display_names",
        choices = display_choices$choices,
        selected = irf_preserve_selection(input$direct_display_names, display_choices$values, defaults$display_names),
        server = TRUE
      )
    }, ignoreInit = TRUE)
    if (has_scenarios) {
      shiny::observeEvent(scenario_data(), {
        data <- scenario_data()
        shock_choices <- irf_choices(data$shock)
        scenario_display_choices <- irf_scenario_response_choices(data)
        shiny::updateSelectizeInput(
          session,
          "scenario_shocks",
          choices = shock_choices,
          selected = irf_preserve_selection(input$scenario_shocks, shock_choices),
          server = TRUE
        )
        shiny::updateSelectizeInput(
          session,
          "scenario_display_names",
          choices = scenario_display_choices$choices,
          selected = irf_preserve_selection(input$scenario_display_names, scenario_display_choices$values, defaults$display_names),
          server = TRUE
        )
      }, ignoreInit = TRUE)
    }

    # 2. Each mode produces one common pretty-IRF table and selector metadata.
    mode_result <- shiny::reactive({
      shiny::req(input$mode)
      if (identical(input$mode, "scenario")) {
        shiny::validate(shiny::need(has_scenarios, "Scenario Mode is not configured."))
        shiny::req(input$scenario, input$scenario_shocks, input$scenario_display_names)
        data <- irf_filter_scenarios(
          scenarios,
          models = input$models,
          scenario_names = input$scenario,
          shocks = input$scenario_shocks,
          display_names = input$scenario_display_names
        )
        message <- irf_scenario_message(selected_models(), data)
        shiny::validate(shiny::need(nrow(data) > 0, message))
        return(list(
          data = data,
          display_names = input$scenario_display_names,
          shocks = input$scenario_shocks,
          footnote = irf_scenario_footnote(data),
          message = message,
          title = irf_scenario_plot_title(input$scenario_display_names, input$scenario)
        ))
      }

      message <- irf_custom_message(input$mode, selected_models())
      shiny::validate(shiny::need(is.null(message), message))
      if (identical(input$mode, "targeted")) {
        shiny::req(input$target_display_name, input$target_shock, input$target_display_names)
        resolved_shocks <- irf_resolve_model_shocks(shock_selector(), input$target_shock)
        shiny::validate(shiny::need(resolved_shocks$valid, resolved_shocks$message))
        target_data <- irf_run_targeted(
          selected_models(),
          target_display_name = input$target_display_name,
          target_value = input$target_value,
          model_shocks = resolved_shocks$shocks,
          horizon = input$target_horizon,
          use_cd = isTRUE(input$target_use_cd),
          lambda = input$target_lambda
        )
        data <- target_data |>
          dplyr::filter(display_name %in% input$target_display_names)
        data <- irf_label_selected_shocks(
          data,
          resolved_shocks$shocks,
          resolved_shocks$display_names
        )
        return(list(
          data = data,
          display_names = input$target_display_names,
          shocks = unique(data$shock),
          message = NULL,
          footnote = irf_custom_footnote(
            "targeted",
            target_data,
            use_cd = input$target_use_cd,
            lambda = input$target_lambda,
            shock_display_names = resolved_shocks$display_names,
            target_display_name = input$target_display_name,
            target_value = input$target_value
          ),
          title = irf_custom_plot_title(
            "targeted",
            input$target_display_names,
            shock_display_names = resolved_shocks$display_names
          )
        ))
      }

      shiny::req(input$direct_shocks, input$direct_display_names)
      resolved_shocks <- irf_resolve_model_shocks(shock_selector(), input$direct_shocks)
      shiny::validate(shiny::need(resolved_shocks$valid, resolved_shocks$message))
      data <- irf_run_direct(
        selected_models(),
        model_shocks = resolved_shocks$shocks,
        horizon = input$direct_horizon
      ) |>
        dplyr::filter(display_name %in% input$direct_display_names)
      data <- irf_label_selected_shocks(
        data,
        resolved_shocks$shocks,
        resolved_shocks$display_names
      )
      list(
        data = data,
        display_names = input$direct_display_names,
        shocks = unique(data$shock),
        message = NULL,
        footnote = irf_custom_footnote(
          "direct",
          data,
          shock_display_names = resolved_shocks$display_names
        ),
        title = irf_custom_plot_title(
          "direct",
          input$direct_display_names,
          shock_display_names = resolved_shocks$display_names
        )
      )
    })
    plot <- shiny::reactive({
      result <- mode_result()
      shiny::validate(shiny::need(nrow(result$data) > 0, "No IRFs match the current selection."))
      title <- irf_title_or_default(input$custom_title, result$title)
      plotter <- dashboard_selected_plotter(input)
      out <- plot_pretty(
        result$data,
        display_names = result$display_names,
        shocks = result$shocks,
        title = title,
        plotter = plotter
      )
      if (!is.null(result$footnote)) {
        out$graph <- out$graph + (
          if (plotter == "ggrba") ggrba::footnote_rba(result$footnote) else ggplot2::labs(caption = result$footnote)
        )
      }
      out
    })

    # 3. Plot rendering and downloads stay independent of the active IRF mode.
    output$message <- shiny::renderUI({
      message <- mode_result()$message
      if (is.null(message)) {
        return(NULL)
      }
      shiny::div(class = "alert alert-info", message)
    })
    dashboard_bind_plot_mode(
      output = output,
      graph = shiny::reactive(plot()$graph),
      compatibility_mode = shiny::reactive(isTRUE(input$use_compatibility_plot)),
      plotter = shiny::reactive(dashboard_selected_plotter(input))
    )
    output$download_data <- shiny::downloadHandler(
      filename = function() paste0(plot()$graph_fname, ".xlsx"),
      content = function(file) writexl::write_xlsx(plot()$graph_data, file)
    )
    output$download_graph <- shiny::downloadHandler(
      filename = function() paste0(plot()$graph_fname, ".svg"),
      content = function(file) ezdyn_ggsave(file, plot()$graph, plotter = dashboard_selected_plotter(input))
    )
  })
}
