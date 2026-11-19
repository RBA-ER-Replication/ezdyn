testthat::test_that("dashboard_selected_plotter uses the plot-style input, else the configured backend", {
  # A selected plot style wins.
  testthat::expect_equal(dashboard_selected_plotter(list(plotter = "ggplot"), plotters = c("ggrba", "ggplot")), "ggplot")
  testthat::expect_equal(dashboard_selected_plotter(list(plotter = "ggrba"), plotters = c("ggrba", "ggplot")), "ggrba")
  # Without the control (one configured backend), use the configured backend,
  # preferring ggrba.
  testthat::expect_equal(dashboard_selected_plotter(list(plotter = NULL)), "ggplot")
  testthat::expect_equal(dashboard_selected_plotter(list(), plotters = "ggrba"), "ggrba")
  testthat::expect_equal(dashboard_selected_plotter(list(), plotters = c("ggplot", "ggrba")), "ggrba")
  # A custom input identifier.
  testthat::expect_equal(dashboard_selected_plotter(list(style = "ggplot"), plotters = "ggrba", plotter_input = "style"), "ggplot")
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

testthat::test_that("dashboard_plot_mode_controls starts compatibility mode on only for a default ggplot backend", {
  compatibility_checked <- function(plotters) {
    ui_html <- htmltools::renderTags(dashboard_plot_mode_controls(shiny::NS("mod"), plotters = plotters))$html
    grepl("id=\"mod-use_compatibility_plot\"[^>]*checked", ui_html)
  }

  testthat::expect_true(compatibility_checked("ggplot"))
  testthat::expect_false(compatibility_checked("ggrba"))
  testthat::expect_false(compatibility_checked(c("ggplot", "ggrba")))
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

testthat::test_that("dashboard_ggplot_caption wraps each footnote onto its own lines", {
  note <- paste(rep("A long caveat about how these model responses were computed.", 5), collapse = " ")
  caption <- dashboard_ggplot_caption(c("Fixture scenario.", note))$caption
  lines <- strsplit(caption, "\n", fixed = TRUE)[[1]]
  testthat::expect_identical(lines[[1]], "Fixture scenario.")
  testthat::expect_gt(length(lines), 3)
  testthat::expect_true(all(nchar(lines) <= ezdyn_ggplot_wrap_widths()$caption))
  testthat::expect_identical(dashboard_ggplot_caption("Fixture scenario.")$caption, "Fixture scenario.")
})

ezdyn_test_png_size <- function(file) {
  con <- file(file, "rb")
  on.exit(close(con))
  b <- readBin(con, "raw", 24)
  be <- function(x) sum(as.integer(x) * 256^(3:0))
  c(width = be(b[17:20]), height = be(b[21:24]))
}

testthat::test_that("ezdyn_ggsave writes ggplot2 charts at a fixed size whatever device is open", {
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()
  out <- tempfile(fileext = ".png")
  grDevices::pdf(NULL, width = 9, height = 3.5)
  on.exit(grDevices::dev.off(), add = TRUE)
  ezdyn_ggsave(out, p, plotter = "ggplot", dpi = 50)
  # 7 x 6 in at 50 dpi, not the open 9 x 3.5 in device's size.
  testthat::expect_equal(unname(ezdyn_test_png_size(out)), c(350, 300))
})

testthat::test_that("ezdyn_ggsave grows a ggplot2 chart's height when its caption needs the room", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::labs(caption = paste(rep("A footnote line.", 40), collapse = "\n"))
  out <- tempfile(fileext = ".png")
  devices_before <- grDevices::dev.list()
  size <- ezdyn_ggsave(out, p, plotter = "ggplot", dpi = 50)
  testthat::expect_equal(size$width, 7)
  testthat::expect_gt(size$height, 6)
  testthat::expect_equal(unname(ezdyn_test_png_size(out)), c(350, round(size$height * 50)))
  # The measurement leaves the open graphics devices as they were.
  testthat::expect_identical(grDevices::dev.list(), devices_before)
})

testthat::test_that("render_rbaplot draws ggplot2 charts at the output's width and pixel ratio", {
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()

  result <- render_rbaplot(p, plotter = "ggplot", width = 450, height = 400, pixelratio = 2)

  testthat::expect_equal(result[c("width", "height", "class")], list(width = 450, height = 400, class = "ezdyn-fitted-plot"))
  testthat::expect_equal(unname(ezdyn_test_png_size(result$src)), c(900, 800))
})

testthat::test_that("dashboard_plot_mode_ui keeps the minimum height and lets the outputs grow", {
  ui_html <- htmltools::renderTags(dashboard_plot_mode_ui(shiny::NS("mod"), height = 400))$html

  testthat::expect_match(ui_html, "min-height: 400px", fixed = TRUE)
  testthat::expect_match(ui_html, "id=\"mod-plot_direct\"[^>]*height:auto")
  testthat::expect_match(ui_html, "id=\"mod-plot_image\"[^>]*height:auto")
})

testthat::test_that("dashboard_bind_plot_mode sizes both renderers to the output and grows them for long captions", {
  base <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c")), ggplot2::aes(x, y, colour = g)) +
    ggplot2::geom_line()
  tall <- base + ggplot2::labs(caption = paste(rep("A footnote line.", 30), collapse = "\n"))
  current <- NULL

  server <- function(input, output, session) {
    dashboard_bind_plot_mode(
      output,
      graph = shiny::reactive(current),
      compatibility_mode = shiny::reactive(isTRUE(input$compat)),
      plotter = shiny::reactive("ggplot")
    )
  }

  current <- base
  shiny::testServer(server, {
    session$setInputs(compat = FALSE)
    # MockShinySession reports every output as 600px wide.
    testthat::expect_equal(output$plot_image$width, 600)
    testthat::expect_equal(output$plot_image$height, 400)
    testthat::expect_equal(output$plot_image$class, "ezdyn-fitted-plot")
    session$setInputs(compat = TRUE)
    testthat::expect_equal(output$plot_direct$width, 600)
    testthat::expect_equal(output$plot_direct$height, 400)
  })

  current <- tall
  shiny::testServer(server, {
    session$setInputs(compat = TRUE)
    testthat::expect_gt(output$plot_direct$height, 400)
    session$setInputs(compat = FALSE)
    testthat::expect_gt(output$plot_image$height, 400)
  })
})
