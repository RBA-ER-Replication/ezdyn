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

#' Return the shock-grouping value to select for the "Shock grouping:" input.
#'
#' Prefers `current` (the existing selection, if still valid for `choices` -
#' used when the available models/groupings change mid-session), then
#' `configured_default` (`config$hsd$shock_group`), then the first real
#' (non-"None") available grouping, so a dashboard that leaves `shock_group`
#' unconfigured still starts on a meaningful grouping rather than silently
#' falling back to "None".
#'
#' @param current Current selection, or `NULL`/`NA` if there isn't one yet.
#' @param configured_default Configured preferred grouping, or `NULL`.
#' @param choices Named vector of available grouping choices, including a
#'   leading `"None" = ""` entry.
#'
#' @return One grouping value from `choices` (possibly `""` for "None").
hsd_selected_shock_group <- function(current, configured_default, choices) {
  values <- unname(choices)
  if (!is.null(current) && !is.na(current) && current %in% values) {
    return(current)
  }
  if (!is.null(configured_default) && configured_default %in% values) {
    return(configured_default)
  }
  non_none <- setdiff(values, "")
  if (length(non_none) > 0) non_none[[1]] else ""
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
  selected_group <- hsd_selected_shock_group(NULL, config$hsd$shock_group, initial_groups)
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
      selected <- hsd_selected_shock_group(input$shock_group, config$hsd$shock_group, choices)
      shiny::updateSelectInput(session, "shock_group", choices = choices, selected = selected)
    }, ignoreInit = TRUE)

    hsd <- shiny::reactive({
      model <- selected_model()
      ez_hd(model$M_, model$oo_, model_name = input$model, data_start = config$timeline$hsd_data_start)
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
