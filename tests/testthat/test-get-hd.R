testthat::test_that("hsd_data_available detects missing or empty Smoothed* fields", {
  testthat::expect_false(hsd_data_available(list()))
  testthat::expect_false(hsd_data_available(list(SmoothedShocks = list())))
  testthat::expect_false(hsd_data_available(list(SmoothedVariables = list())))
  testthat::expect_false(hsd_data_available(list(SmoothedShocks = list(), SmoothedVariables = list())))
  testthat::expect_false(hsd_data_available(list(SmoothedShocks = list(eps_r = 1), SmoothedVariables = list())))
})

testthat::test_that("hsd_data_available is TRUE when both fields are populated", {
  oo_ <- list(
    SmoothedShocks = list(eps_r = c(0.1, 0.2)),
    SmoothedVariables = list(r_obs = c(4.1, 4.2))
  )
  testthat::expect_true(hsd_data_available(oo_))
})

testthat::test_that("get_hd errors clearly when a model has no historical shock decomposition data", {
  M_ <- list(model_name = "NoHSD")
  oo_ <- list()

  testthat::expect_error(get_hd(M_, oo_), "NoHSD.*historical shock decomposition")
})

testthat::test_that("get_hd falls back to 'unnamed' when M_$model_name is missing", {
  testthat::expect_error(get_hd(list(), list()), "unnamed.*historical shock decomposition")
})
