# Variable Dictionary tab ---------------------------------------------------
#
# Lets a dashboard user pick one or more configured models and browse their
# parameters, variables, and shocks side by side. This module owns only
# Shiny UI/state (model selection, the "show codes" toggle, and per-model
# dynamic `DT` output wiring); the actual tables come from ezdyn's public
# `get_param_table()`/`get_variable_table()`/`get_shock_table()`.

# 1. Small helpers ----------------------------------------------------------

# Whether a model has the metadata a given dictionary section needs.
dictionary_has_param_table <- function(model) !is.null(model$M_$param_table)
dictionary_has_varmeta <- function(model) !is.null(model$M_$varmeta)
dictionary_has_shock_meta <- function(model) !is.null(model$M_$shock_meta)

# Build a Shiny-output-safe id for one selected model's dictionary section.
dictionary_output_id <- function(section, model_name) {
  paste0(section, "_", gsub("[^A-Za-z0-9]", "_", model_name))
}

# Build the stacked per-model UI for one dictionary section: one heading
# plus either a `DT` placeholder (when the model has the needed metadata)
# or an inline fallback note (e.g. MARTIN currently has no `param_table`).
dictionary_section_ui <- function(ns, section, models, has_capability, unavailable_message) {
  if (length(models) == 0) {
    return(shiny::div(class = "alert alert-info", "Select at least one model."))
  }

  sections <- lapply(names(models), function(model_name) {
    body <- if (has_capability(models[[model_name]])) {
      DT::DTOutput(ns(dictionary_output_id(section, model_name)))
    } else {
      shiny::div(class = "alert alert-info", unavailable_message)
    }
    shiny::tagList(shiny::h4(model_name), body)
  })

  do.call(shiny::tagList, sections)
}

# Populate the `DT` output for every selected model that has the needed
# metadata for one dictionary section.
dictionary_render_section <- function(output, section, models, has_capability, build_table) {
  for (model_name in names(models)) {
    model <- models[[model_name]]
    if (!has_capability(model)) next
    local({
      model_ <- model
      id <- dictionary_output_id(section, model_name)
      output[[id]] <- DT::renderDT(build_table(model_))
    })
  }
}

# 2. Variable Dictionary user interface -------------------------------------

#' Build the namespaced Variable Dictionary tab UI.
#'
#' @param id Module namespace.
#' @param config Validated dashboard configuration.
#'
#' @return Shiny UI.
UITab_Dictionary <- function(id, config) {
  ns <- shiny::NS(id)
  model_choices <- irf_choices(config$dictionary$models)
  selected_models <- irf_preserve_selection(config$dictionary$defaults$models, model_choices)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::selectizeInput(
        ns("models"),
        "Model(s):",
        choices = model_choices,
        selected = selected_models,
        multiple = TRUE
      ),
      shiny::checkboxInput(
        ns("show_codes"),
        "Show underlying codes",
        value = FALSE
      )
    ),
    shiny::mainPanel(
      width = 9,
      shiny::tabsetPanel(
        shiny::tabPanel("Parameters", value = "parameters", shiny::uiOutput(ns("parameters_content"))),
        shiny::tabPanel("Variables", value = "variables", shiny::uiOutput(ns("variables_content"))),
        shiny::tabPanel("Shocks", value = "shocks", shiny::uiOutput(ns("shocks_content")))
      )
    )
  )
}

# 3. Variable Dictionary server ---------------------------------------------

#' Serve the namespaced Variable Dictionary tab.
#'
#' @param id Module namespace.
#' @param config Validated dashboard configuration.
#'
#' @return Invisibly, `NULL`.
ServerTab_Dictionary <- function(id, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_models <- shiny::reactive({
      shiny::req(input$models)
      config$models[input$models]
    })

    output$parameters_content <- shiny::renderUI({
      dictionary_section_ui(
        ns, "parameters", selected_models(), dictionary_has_param_table,
        "No parameter metadata available for this model."
      )
    })
    shiny::observe({
      dictionary_render_section(
        output, "parameters", selected_models(), dictionary_has_param_table,
        function(model) get_param_table(model$M_, mode = "dt", show_codes = input$show_codes)
      )
    })

    output$variables_content <- shiny::renderUI({
      dictionary_section_ui(
        ns, "variables", selected_models(), dictionary_has_varmeta,
        "No variable metadata available for this model."
      )
    })
    shiny::observe({
      dictionary_render_section(
        output, "variables", selected_models(), dictionary_has_varmeta,
        function(model) get_variable_table(model$M_, mode = "dt", show_codes = input$show_codes)
      )
    })

    output$shocks_content <- shiny::renderUI({
      dictionary_section_ui(
        ns, "shocks", selected_models(), dictionary_has_shock_meta,
        "No shock metadata available for this model."
      )
    })
    shiny::observe({
      dictionary_render_section(
        output, "shocks", selected_models(), dictionary_has_shock_meta,
        function(model) get_shock_table(model$M_, mode = "dt", show_codes = input$show_codes)
      )
    })

    invisible(NULL)
  })
}
