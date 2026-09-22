testthat::test_that("get_anticipated_shocks resolves shock descriptions and codes", {
  dynare <- load_dynare_irf_fixture()
  dynare$M_$exo.names <- c("eps_r", "eps_r_1", "eps_r_2")
  dynare$M_$shock_meta <- data.frame(
    shock = "eps_r",
    description = "Monetary policy shock",
    stringsAsFactors = FALSE
  )

  expected <- c(`1` = "eps_r_1", `2` = "eps_r_2")
  testthat::expect_equal(
    get_anticipated_shocks(dynare$M_, "Monetary policy shock", c(2, 1)),
    expected
  )
  testthat::expect_equal(
    get_anticipated_shocks(dynare$M_, "eps_r", 1:2),
    expected
  )
})

testthat::test_that("get_anticipated_shocks supports custom MOO shock metadata", {
  moo <- load_custom_moo_irf_fixture()
  moo$M_$exo.vars <- c("eps_r", "eps_r_1", "eps_r_2")
  moo$M_$shock_meta <- data.frame(
    shock = "eps_r",
    description = "Monetary policy shock",
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(
    get_anticipated_shocks(moo$M_, "Monetary policy shock", 1:2),
    c(`1` = "eps_r_1", `2` = "eps_r_2")
  )
})

testthat::test_that("get_anticipated_shocks rejects invalid periods and unavailable codes", {
  dynare <- load_dynare_irf_fixture()
  dynare$M_$exo.names <- c("eps_r", "eps_r_1")
  dynare$M_$shock_meta <- data.frame(
    shock = "eps_r",
    description = "Monetary policy shock",
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    get_anticipated_shocks(dynare$M_, "Monetary policy shock", c(1, 1)),
    "unique positive integers"
  )
  testthat::expect_error(
    get_anticipated_shocks(dynare$M_, "Monetary policy shock", c(1, 2)),
    "eps_r_2"
  )
  testthat::expect_error(
    get_anticipated_shocks(dynare$M_, "Unknown shock", 1),
    "Unknown shock"
  )
})

testthat::test_that("get_anticipated_shocks rejects ambiguous shock descriptions", {
  dynare <- load_dynare_irf_fixture()
  dynare$M_$exo.names <- c("eps_r", "eps_x", "eps_r_1")
  dynare$M_$shock_meta <- data.frame(
    shock = c("eps_r", "eps_x"),
    description = c("Policy shock", "Policy shock"),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    get_anticipated_shocks(dynare$M_, "Policy shock", 1),
    "Duplicate shock aliases"
  )
})