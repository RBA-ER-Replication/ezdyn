testthat::test_that("ezdyn_resolve_plotter defaults to ggrba only when it is installed", {
	testthat::expect_equal(ezdyn_resolve_plotter(NULL, ggrba_available = TRUE), "ggrba")
	testthat::expect_equal(ezdyn_resolve_plotter(NULL, ggrba_available = FALSE), "ggplot")
})

testthat::test_that("ezdyn_resolve_plotter honours an explicit ggplot request regardless of ggrba availability", {
	testthat::expect_equal(ezdyn_resolve_plotter("ggplot", ggrba_available = TRUE), "ggplot")
	testthat::expect_equal(ezdyn_resolve_plotter("ggplot", ggrba_available = FALSE), "ggplot")
})

testthat::test_that("ezdyn_resolve_plotter errors on an explicit ggrba request only when unavailable", {
	testthat::expect_equal(ezdyn_resolve_plotter("ggrba", ggrba_available = TRUE), "ggrba")
	testthat::expect_error(
		ezdyn_resolve_plotter("ggrba", ggrba_available = FALSE),
		"optional ggrba package"
	)
})

testthat::test_that("ezdyn_resolve_plotter validates the plotter argument", {
	testthat::expect_error(ezdyn_resolve_plotter("invalid"))
})

testthat::test_that("plot_pretty_graph, plot_pretty, and plot_pretty_irf default to NULL (auto-resolved) plotters", {
	testthat::expect_null(formals(plot_pretty_graph)$plotter)
	testthat::expect_null(formals(plot_pretty)$plotter)
	testthat::expect_null(formals(plot_pretty_irf)$plotter)
	testthat::expect_null(formals(plot_alt_paths)$plotter)
})
