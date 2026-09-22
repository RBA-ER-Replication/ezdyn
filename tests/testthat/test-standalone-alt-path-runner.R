# `run_alt_paths/run_alt_paths.R` lives outside this package, in the
# surrounding monorepo, so it is not available in a standalone checkout of
# this package (e.g. a GitHub clone of just `ezdyn`/`ezdyn-publish`). Skip
# rather than fail when it can't be found.
standalone_runner_path <- normalizePath(
  testthat::test_path("..", "..", "..", "run_alt_paths", "run_alt_paths.R"),
  winslash = "/",
  mustWork = FALSE
)

testthat::test_that("standalone runner uses only public ezdyn alternative-path APIs", {
  testthat::skip_if_not(
    file.exists(standalone_runner_path),
    "run_alt_paths/run_alt_paths.R is outside this package and not available in this checkout"
  )

  script <- paste(readLines(standalone_runner_path, warn = FALSE), collapse = "\n")

  testthat::expect_match(script, "import_baseline\\(")
  testthat::expect_match(script, "get_alt_paths\\(")
  testthat::expect_match(script, "plot_pretty\\(")
  testthat::expect_match(script, "forecast_start\\s*(?:<-|=)\\s*as.Date\\(\"2026-09-01\"\\)")
  testthat::expect_false(grepl("Sys.getenv", script))
  testthat::expect_match(script, "format\\(eom_dates, \"%d/%m/%Y\"\\)")
  testthat::expect_false(grepl("ezdyn:::|source\\(|read_baseline_excel", script))
})
