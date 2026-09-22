testthat::test_that("defined_display_names respects the hsd_only filter", {
  M_ <- list(
    varmeta = data.frame(
      dynare_name = c("r_obs", "gdp_growth", "consumption"),
      display_name = c("Cash Rate", "GDP Growth", "Consumption"),
      alt_paths = c(TRUE, FALSE, TRUE),
      hsd = c(TRUE, TRUE, FALSE),
      stringsAsFactors = FALSE
    )
  )

  testthat::expect_equal(
    defined_display_names(M_, hsd_only = TRUE),
    c("Cash Rate", "GDP Growth")
  )
})