testthat::test_that("dashboard_selected_plotter resolves the plot-style input with an automatic fallback", {
  testthat::expect_equal(dashboard_selected_plotter(list(plotter = "ggplot")), "ggplot")
  testthat::expect_equal(dashboard_selected_plotter(list(plotter = "ggrba")), ezdyn_resolve_plotter("ggrba"))
  testthat::expect_equal(dashboard_selected_plotter(list(plotter = NULL)), ezdyn_resolve_plotter(NULL))
})

testthat::test_that("dashboard_sync_compatibility_mode toggles compatibility mode with the selected plotter", {
  # shiny::testServer()'s mock session does not simulate update*Input()
  # actually changing `input$...` (there is no real client to echo the
  # update back), so this asserts on the update call itself.
  recorded_values <- logical(0)
  testthat::local_mocked_bindings(
    updateCheckboxInput = function(session, inputId, value, ...) {
      recorded_values[length(recorded_values) + 1] <<- value
    },
    .package = "shiny"
  )

  fake_server <- function(input, output, session) {
    dashboard_sync_compatibility_mode(input, session)
  }

  shiny::testServer(fake_server, {
    session$setInputs(plotter = "ggrba")
    session$setInputs(plotter = "ggplot")
    session$setInputs(plotter = "ggrba")
  })

  testthat::expect_equal(recorded_values, c(TRUE, FALSE))
})

testthat::test_that("dashboard_plot_mode_controls exposes a plot-style control only when ggrba is installed", {
  ui_html <- htmltools::renderTags(dashboard_plot_mode_controls(shiny::NS("mod")))$html

  if (requireNamespace("ggrba", quietly = TRUE)) {
    testthat::expect_match(ui_html, "mod-plotter")
    testthat::expect_match(ui_html, "Plot style:")
    testthat::expect_match(ui_html, "RBA style")
    testthat::expect_match(ui_html, "ggplot2")
  } else {
    testthat::expect_false(grepl("mod-plotter", ui_html, fixed = TRUE))
  }
})

testthat::test_that("ezdyn_ggsave saves a file using the requested backend", {
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()

  out_ggplot <- tempfile(fileext = ".png")
  ezdyn_ggsave(out_ggplot, p, plotter = "ggplot")
  testthat::expect_true(file.exists(out_ggplot))

  if (requireNamespace("ggrba", quietly = TRUE)) {
    out_ggrba <- tempfile(fileext = ".png")
    ezdyn_ggsave(out_ggrba, p, plotter = "ggrba")
    testthat::expect_true(file.exists(out_ggrba))
  }
})

testthat::test_that("render_rbaplot writes a temporary image using the resolved backend", {
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()

  result <- render_rbaplot(p, plotter = "ggplot")
  testthat::expect_true(file.exists(result$src))
  testthat::expect_equal(result$width, 800)
})
