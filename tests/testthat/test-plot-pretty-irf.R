plot_pretty_irf_fixture <- function() {
	tibble::tibble(
		date = rep(seq(as.Date("2024-03-01"), as.Date("2024-09-01"), by = "quarter"), 4),
		dynare_name = c(
			rep(NA_character_, 3),
			rep("r_obs", 3),
			rep("r_obs", 3),
			rep("r_obs", 3)
		),
		display_name = "Cash Rate",
		display_unit = "Per cent",
		unit_symbol_irf = "ppt",
		unit_symbol_baseline = "%",
		path_name = c(
			rep("Baseline", 3),
			rep("Lower rates", 3),
			rep("Lower rates", 3),
			rep("Higher rates", 3)
		),
		model_name = c(
			rep(NA_character_, 3),
			rep("DINGO", 3),
			rep("MARTIN", 3),
			rep("DINGO", 3)
		),
		# MARTIN is left NA to exercise the dynamic-palette fallback alongside DINGO's override.
		model_colour = c(
			rep(NA_character_, 3),
			rep("firebrick", 3),
			rep(NA_character_, 3),
			rep("firebrick", 3)
		),
		value = c(4.1, 4.2, 4.3, 4.0, 3.8, 3.7, 4.0, 3.9, 3.8, 4.3, 4.5, 4.6)
	)
}

testthat::test_that("plot_pretty retains ordinary IRF graph-data structure", {
	dynare <- load_dynare_irf_fixture()
	pretty <- get_irf(dynare$M_, dynare$oo_, "eps_r", horizon = 2, pretty = TRUE)
	# The default plotter is ggrba when it is installed, otherwise ggplot2.
	actual <- plot_pretty(pretty)
	if (!requireNamespace("ggrba", quietly = TRUE)) {
		# Only an explicit ggrba request needs the optional package.
		testthat::expect_error(plot_pretty(pretty, plotter = "ggrba"), "optional ggrba package")
		testthat::expect_identical(plot_pretty(pretty, plotter = "ggplot")$graph_data, actual$graph_data)
	}
	legacy <- plot_pretty_irf(pretty, plotter = "ggplot")

	testthat::expect_s3_class(actual$graph, "ggplot")
	testthat::expect_true(all(c("t", "display_name", "shock", "model_name") %in% names(actual$graph_data)))
	testthat::expect_false("date" %in% names(actual$graph_data))
	testthat::expect_identical(legacy$graph_data, actual$graph_data)
})

testthat::test_that("plot_pretty plots alternative paths with dates and level units", {
	actual <- plot_pretty(
		plot_pretty_irf_fixture(),
		display_names = "Cash Rate",
		shocks = "ignored",
		plotter = "ggplot"
	)

	testthat::expect_s3_class(actual$graph, "ggplot")
	testthat::expect_true(all(c("date", "display_name", "path_name", "model_name") %in% names(actual$graph_data)))
	testthat::expect_setequal(
		setdiff(names(actual$graph_data), c("date", "display_name", "path_name", "model_name")),
		c("Baseline", "Lower rates (DINGO)", "Lower rates (MARTIN)", "Higher rates")
	)
	testthat::expect_equal(sum(!is.na(actual$graph_data$Baseline)), 3)
	testthat::expect_match(actual$graph_fname, "Alternative policy path")

	colour_scale <- actual$graph$scales$get_scales("colour")
	testthat::expect_identical(colour_scale$palette(1)[["Baseline"]], "royalblue")
	testthat::expect_identical(
		colour_scale$breaks,
		c("Baseline", "Lower rates (DINGO)", "Lower rates (MARTIN)", "Higher rates")
	)
	# Baseline is drawn last (highest factor level) so it renders on top of
	# overlapping alternative-path lines, even though it stays first in the legend.
	testthat::expect_identical(utils::tail(levels(actual$graph$data$line_label), 1), "Baseline")
	testthat::expect_true(grepl("%", actual$graph$labels$y, fixed = TRUE))
	date_scale <- actual$graph$scales$get_scales("x")
	testthat::expect_s3_class(date_scale, "ScaleContinuousDate")
	testthat::expect_true(date_scale$guide$params$check.overlap)
	if (requireNamespace("ggrba", quietly = TRUE)) {
		testthat::expect_s3_class(
			plot_pretty(plot_pretty_irf_fixture(), plotter = "ggrba")$graph,
			"ggplot"
		)
	} else {
		testthat::expect_error(
			plot_pretty(plot_pretty_irf_fixture(), plotter = "ggrba"),
			"optional ggrba package"
		)
	}
})

testthat::test_that("plot_pretty honours a model's configured line colour, falling back to the dynamic palette for models without one", {
	actual <- plot_pretty(
		plot_pretty_irf_fixture(),
		display_names = "Cash Rate",
		plotter = "ggplot"
	)

	colour_scale <- actual$graph$scales$get_scales("colour")
	palette <- colour_scale$palette(1)
	testthat::expect_identical(palette[["Baseline"]], "royalblue")
	testthat::expect_identical(palette[["Lower rates (DINGO)"]], "firebrick")
	testthat::expect_identical(palette[["Higher rates"]], "firebrick")
	testthat::expect_false(identical(palette[["Lower rates (MARTIN)"]], "firebrick"))
})

testthat::test_that("plot_pretty labels ggplot2 facets with only the non-empty units", {
	irf <- tibble::tibble(
		t = rep(1:2, 3),
		shock = "Monetary policy shock",
		dynare_name = rep(c("pinfobs", "pi", "y"), each = 2),
		display_name = rep(c("Inflation", "Inflation", "Output"), each = 2),
		display_unit = rep(c("Per cent (quarterly)", "Per cent (quarterly)", "Per cent deviation from steady state"), each = 2),
		# One Inflation model has no unit symbol; Output has none at all.
		unit_symbol_irf = rep(c("ppt", "", ""), each = 2),
		unit_symbol_baseline = "",
		model_name = rep(c("SW", "Gali", "SW"), each = 2),
		value = c(0.1, 0.2, 0.1, 0.1, -0.3, -0.2)
	)

	actual <- plot_pretty(irf, plotter = "ggplot")

	testthat::expect_setequal(
		unique(actual$graph$data$display_name_unit),
		c("Inflation\nPer cent (quarterly), ppt", "Output\nPer cent deviation from steady state")
	)
})

testthat::test_that("plot_pretty validates Dynare-name selection and duplicate Baselines", {
	pretty <- plot_pretty_irf_fixture()
	duplicate <- dplyr::bind_rows(
		pretty,
		pretty |>
			dplyr::filter(path_name == "Baseline") |>
			dplyr::mutate(model_name = "Duplicated")
	)
	actual <- plot_pretty(duplicate, plotter = "ggplot", collapse_baseline = TRUE)
	testthat::expect_equal(sum(!is.na(actual$graph_data$Baseline)), 3)

	testthat::expect_error(
		plot_pretty(duplicate, plotter = "ggplot", collapse_baseline = FALSE),
		"Duplicate Baseline rows require"
	)

	conflicting <- duplicate |>
		dplyr::mutate(
			value = dplyr::if_else(
				model_name == "Duplicated" & date == as.Date("2024-09-01"),
				value + 1,
				value
			)
		)
	testthat::expect_error(
		plot_pretty(conflicting, plotter = "ggplot"),
		"must have identical values"
	)

	different_names <- pretty |>
		dplyr::mutate(
			dynare_name = dplyr::if_else(model_name == "MARTIN", "martin_r", dynare_name)
		)
	testthat::expect_error(
		plot_pretty(different_names, dynare_names = "r_obs", plotter = "ggplot"),
		"use `display_names` instead"
	)
})

testthat::test_that("plot_alt_paths delegates legacy selections to plot_pretty", {
	actual <- plot_alt_paths(
		df_raw = plot_pretty_irf_fixture(),
		plot_variables = "r_obs",
		plot_model = "DINGO",
		plot_start_date = as.Date("2024-03-01"),
		plot_end_date = as.Date("2024-09-01")
	)

	testthat::expect_s3_class(actual$graph, "ggplot")
	testthat::expect_true("Baseline" %in% names(actual$graph_data))
})

testthat::test_that("plot_pretty honours an alternative-path date range", {
	date_range <- as.Date(c("2024-03-01", "2025-03-01"))
	actual <- plot_pretty(
		plot_pretty_irf_fixture(),
		plotter = "ggplot",
		date_range = date_range
	)

	testthat::expect_equal(actual$graph$coordinates$limits$x, date_range)
})



ggplot_layout_irf_fixture <- function() {
	models <- c("Smets Wouters (2007)", "Gali NK (Ch.3)", "Gali Unemployment (2010)", "Gertler-Karadi (2015)")
	tibble::tibble(
		t = rep(1:3, length(models)),
		shock = "Monetary Policy Shock",
		dynare_name = "pi",
		display_name = "Inflation",
		display_unit = "Per cent (quarterly)",
		unit_symbol_irf = "ppt",
		unit_symbol_baseline = "",
		model_name = rep(models, each = 3),
		value = seq(-0.5, 0.6, length.out = 3 * length(models))
	)
}

# Widest visible title/legend/caption grob when drawn on a 392 x 400 px
# renderPlot canvas (the narrowest dashboard plot column) at 72 dpi.
ggplot_layout_overflow <- function(graph, width_px = 392, height_px = 400) {
	width_in <- width_px / 72
	grDevices::pdf(NULL, width = width_in, height = height_px / 72)
	on.exit(grDevices::dev.off(), add = TRUE)
	gt <- ggplot2::ggplotGrob(graph)
	parts <- c("title", "subtitle", "caption", "guide-box-bottom", "guide-box-right")
	widths <- vapply(parts, function(name) {
		idx <- which(gt$layout$name == name)
		if (length(idx) == 0 || inherits(gt$grobs[[idx]], "zeroGrob")) return(0)
		grid::convertWidth(grid::grobWidth(gt$grobs[[idx]]), "in", valueOnly = TRUE)
	}, numeric(1))
	list(widths = widths, width_in = width_in, legend_right = widths[["guide-box-right"]] > 0)
}

testthat::test_that("ggplot2 IRF charts fit their title, legend and caption in a narrow dashboard column", {
	out <- plot_pretty(
		ggplot_layout_irf_fixture(),
		title = "Response of Inflation to Monetary Policy Shock",
		plotter = "ggplot"
	)
	graph <- out$graph + dashboard_ggplot_caption(c(
		"Monetary Policy Shock is calibrated to a 1 ppt impact on Nominal Interest Rate in the first quarter.",
		paste(rep("Smets Wouters (2007) responses are a first-order approximation around the steady state.", 3), collapse = " ")
	))

	layout <- ggplot_layout_overflow(graph)
	testthat::expect_false(layout$legend_right)
	testthat::expect_true(all(layout$widths <= layout$width_in))
	# One shock across several models: the shock becomes the legend title and
	# each key shows only its model; the data keeps its full line labels.
	colour_scale <- graph$scales$get_scales("colour")
	testthat::expect_identical(colour_scale$breaks, unique(out$graph$data$line_label))
	testthat::expect_identical(colour_scale$labels, unique(ggplot_layout_irf_fixture()$model_name))
	testthat::expect_true("Monetary Policy Shock (Smets Wouters (2007))" %in% names(out$graph_data))
})

testthat::test_that("ggplot2 alternative-path legends wrap long multi-model labels and stay merged", {
	pretty <- plot_pretty_irf_fixture()
	out <- plot_pretty(pretty, display_names = "Cash Rate", plotter = "ggplot")
	layout <- ggplot_layout_overflow(out$graph)
	testthat::expect_false(layout$legend_right)
	testthat::expect_true(all(layout$widths <= layout$width_in))
	grDevices::pdf(NULL, width = 392 / 72, height = 400 / 72)
	on.exit(grDevices::dev.off(), add = TRUE)
	gt <- ggplot2::ggplotGrob(out$graph)
	guide_box <- gt$grobs[[which(gt$layout$name == "guide-box-bottom")]]
	# Colour and linetype guides share labels, so they still merge into one legend.
	testthat::expect_equal(sum(grepl("^guides", guide_box$layout$name)), 1)
})

testthat::test_that("building a ggplot2 chart does not open a graphics device", {
	# Compare with the devices already open rather than closing them. In a
	# fresh session (none open) this catches a newly opened default device.
	devices_before <- grDevices::dev.list()
	plot_pretty(ggplot_layout_irf_fixture(), plotter = "ggplot")
	testthat::expect_identical(grDevices::dev.list(), devices_before)
})
