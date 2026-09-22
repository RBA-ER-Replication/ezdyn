# Historical shock-decomposition tab --------------------------------------

#' Return valid named Shiny choices.
#'
#' @param values Character values.
#'
#' @return A named character vector.
hsd_choices <- function(values) {
  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  stats::setNames(values, values)
}

#' Return the preferred selected values that remain available.
#'
#' @param preferred Configured preferred values.
#' @param choices Available values.
#'
#' @return Character vector.
hsd_selected_values <- function(preferred, choices) {
  selected <- intersect(preferred, choices)
  if (length(selected) > 0) selected else utils::head(choices, 1)
}

#' Build the namespaced historical shock-decomposition tab UI.
#'
#' @param id Module namespace.
#' @param config Validated dashboard configuration.
#'
#' @return Shiny UI.
UITab_HistShockDecomp <- function(id, config) {
  ns <- shiny::NS(id)
  initial_model <- config$hsd$models[[1]]
  initial_variables <- hsd_choices(defined_display_names(
    config$models[[initial_model]]$M_,
    hsd_only = TRUE
  ))
  selected_variables <- hsd_selected_values(config$hsd$display_names, initial_variables)
  initial_groups <- ez_hd.available_shock_groupings(config$models[[initial_model]]$M_)
  initial_groups <- c("None" = "", hsd_choices(initial_groups))
  selected_group <- if (!is.null(config$hsd$shock_group) &&
                        config$hsd$shock_group %in% unname(initial_groups)) {
    config$hsd$shock_group
  } else {
    "All shocks"
  }
  preamble <- if (is.function(config$hsd$preamble_ui)) config$hsd$preamble_ui() else NULL

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::selectInput(ns("model"), "Model:", choices = config$hsd$models, selected = initial_model),
      shiny::selectizeInput(
        ns("display_names"),
        "Variables:",
        choices = initial_variables,
        selected = selected_variables,
        multiple = TRUE
      ),
      shiny::selectInput(ns("shock_group"), "Shock grouping:", choices = initial_groups, selected = selected_group),
      shiny::checkboxInput(ns("interactive"), "Interactive chart (slower)", value = FALSE),
      shiny::downloadButton(ns("download_data"), "Download Data")
    ),
    shiny::mainPanel(
      width = 9,
      style = "padding-left: 0; padding-right: 0;",
      preamble,
      shiny::div(
        style = "width: 100%;",
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === true", ns("interactive")),
          plotly::plotlyOutput(ns("plot"), width = "100%", height = 700)
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] !== true", ns("interactive")),
          shiny::plotOutput(ns("plot_static"), width = "100%", height = 700)
        )
      ),
      shiny::div(
        style = "padding: 0 24px;",
        shiny::tags$small(
          class = "text-muted",
          "'Other' includes any shocks not part of the grouping; 'Residual' contains approximation error from initial conditions."
        )
      )
    )
  )
}

#' Serve the namespaced historical shock-decomposition tab.
#'
#' @param id Module namespace.
#' @param config Validated dashboard configuration.
#'
#' @return Invisibly, `NULL`.
ServerTab_HistShockDecomp <- function(id, config) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_model <- shiny::reactive({
      shiny::req(input$model)
      config$models[[input$model]]
    })

    variable_choices <- shiny::reactive({
      hsd_choices(defined_display_names(selected_model()$M_, hsd_only = TRUE))
    })
    shock_group_choices <- shiny::reactive({
      groups <- ez_hd.available_shock_groupings(selected_model()$M_)
      c("None" = "", hsd_choices(groups))
    })

    shiny::observeEvent(variable_choices(), {
      choices <- variable_choices()
      shiny::updateSelectizeInput(
        session,
        "display_names",
        choices = choices,
        selected = hsd_selected_values(input$display_names, choices),
        server = TRUE
      )
    }, ignoreInit = TRUE)
    shiny::observeEvent(shock_group_choices(), {
      choices <- shock_group_choices()
      selected <- if (input$shock_group %in% unname(choices)) input$shock_group else ""
      shiny::updateSelectInput(session, "shock_group", choices = choices, selected = selected)
    }, ignoreInit = TRUE)

    hsd <- shiny::reactive({
      model <- selected_model()
      ez_hd(model$M_, model$oo_, model_name = input$model)
    })
    plot <- shiny::reactive({
      shiny::req(input$display_names)
      shock_group <- input$shock_group
      if (identical(shock_group, "")) shock_group <- NULL

      plot_pretty_hd(
        hsd(),
        display_names = input$display_names,
        shock_group = shock_group,
        interactive = isTRUE(input$interactive)
      )
    })

    output$plot <- plotly::renderPlotly({
      shiny::req(isTRUE(input$interactive))
      shiny::req(plot()$graph)
      plot()$graph
    })
    output$plot_static <- shiny::renderPlot({
      shiny::req(!isTRUE(input$interactive))
      shiny::req(plot()$graph)
      plot()$graph
    })
    output$download_data <- shiny::downloadHandler(
      filename = function() paste0(plot()$graph_fname, ".xlsx"),
      content = function(file) writexl::write_xlsx(plot()$graph_data, file)
    )
  })
}
