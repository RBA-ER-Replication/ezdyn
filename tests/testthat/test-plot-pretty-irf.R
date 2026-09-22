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
	if (requireNamespace("ggrba", quietly = TRUE)) {
		actual <- plot_pretty(pretty)
	} else {
		testthat::expect_error(plot_pretty(pretty), "optional ggrba package")
		actual <- plot_pretty(pretty, plotter = "ggplot")
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

