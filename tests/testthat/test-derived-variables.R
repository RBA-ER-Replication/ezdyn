test_that("augment_derived_variables computes registered transforms from raw columns", {
  model <- list(M_ = list(varmeta = data.frame(
    dynare_name = c("infl_obs_ye", "gdp_growth_ye", "dr", "ddr"),
    derived_from = c("infl_obs", "gdp_growth", "r_obs", "r_obs"),
    derived_transform = c("year_ended_sum", "year_ended_sum", "diff", "diff2"),
    stringsAsFactors = FALSE
  )))
  baseline <- data.frame(
    date = seq(as.Date("2023-03-01"), by = "quarter", length.out = 4),
    infl_obs = c(0.5, 0.6, 0.7, 0.8),
    r_obs = c(3.8, 4.0, 4.1, 4.2),
    gdp_growth = c(0.2, 0.3, 0.4, 0.5)
  )

  actual <- augment_derived_variables(baseline, model)

  expect_equal(actual$infl_obs_ye[[4]], sum(baseline$infl_obs))
  expect_equal(actual$gdp_growth_ye[[4]], sum(baseline$gdp_growth))
  expect_equal(actual$dr, c(3.8, 0.2, 0.1, 0.1))
  expect_equal(actual$ddr, c(3.8, -3.6, -0.1, 0.0))
})

test_that("augment_derived_variables treats a blank derived_transform as a pure alias", {
  model <- list(M_ = list(varmeta = data.frame(
    dynare_name = "unemp",
    derived_from = "unemp1",
    derived_transform = NA_character_,
    stringsAsFactors = FALSE
  )))
  baseline <- data.frame(date = as.Date("2023-03-01"), unemp1 = 0.041)

  actual <- augment_derived_variables(baseline, model)

  expect_equal(actual$unemp, 0.041)
})

test_that("augment_derived_variables applies the derived row's own scale_factor on top of its transform", {
  model <- list(M_ = list(varmeta = data.frame(
    dynare_name = c("dr", "unemp"),
    derived_from = c("r_obs", "unemp1"),
    derived_transform = c("diff", NA_character_),
    scale_factor = I(list(2, 100)),
    stringsAsFactors = FALSE
  )))
  baseline <- data.frame(
    date = seq(as.Date("2023-03-01"), by = "quarter", length.out = 3),
    r_obs = c(3.8, 4.0, 4.1),
    unemp1 = c(0.041, 0.042, 0.043)
  )

  actual <- augment_derived_variables(baseline, model)

  expect_equal(actual$dr, 2 * c(3.8, 0.2, 0.1))
  expect_equal(actual$unemp, 100 * baseline$unemp1)
})

test_that("augment_derived_variables leaves a missing source as NA and an existing column untouched", {
  model <- list(M_ = list(varmeta = data.frame(
    dynare_name = c("dr", "unemp"),
    derived_from = c("r_obs", "unemp1"),
    derived_transform = c("diff", NA_character_),
    stringsAsFactors = FALSE
  )))
  baseline <- data.frame(date = as.Date("2023-03-01"), unemp = 4.5)

  actual <- augment_derived_variables(baseline, model)

  expect_true(is.na(actual$dr))
  expect_equal(actual$unemp, 4.5)
})

test_that("derived-variable metadata rejects an unknown transform", {
  model <- list(M_ = list(varmeta = data.frame(
    dynare_name = "dr",
    derived_from = "r_obs",
    derived_transform = "log",
    stringsAsFactors = FALSE
  )))
  baseline <- data.frame(date = as.Date("2023-03-01"), r_obs = 4)

  expect_error(augment_derived_variables(baseline, model), "Unknown `derived_transform`")
})

test_that("derived-variable metadata rejects chaining through another derived row", {
  model <- list(M_ = list(varmeta = data.frame(
    dynare_name = c("dr", "ddr"),
    derived_from = c("r_obs", "dr"),
    derived_transform = c("diff", "diff"),
    stringsAsFactors = FALSE
  )))
  baseline <- data.frame(date = as.Date("2023-03-01"), r_obs = 4)

  expect_error(augment_derived_variables(baseline, model), "not another derived row")
})

test_that("derived-variable metadata rejects a row that references itself", {
  model <- list(M_ = list(varmeta = data.frame(
    dynare_name = "dr",
    derived_from = "dr",
    derived_transform = "diff",
    stringsAsFactors = FALSE
  )))
  baseline <- data.frame(date = as.Date("2023-03-01"), dr = 4)

  expect_error(augment_derived_variables(baseline, model), "cannot reference its own row")
})

test_that("augment_derived_variables is a no-op when varmeta has no derived_from column", {
  model <- list(M_ = list(varmeta = data.frame(dynare_name = "r_obs", stringsAsFactors = FALSE)))
  baseline <- data.frame(date = as.Date("2023-03-01"), r_obs = 4)

  expect_identical(augment_derived_variables(baseline, model), baseline)
})

test_that("ezdyn_own_scale defaults to identity when scaling metadata is unavailable", {
  expect_equal(ezdyn_own_scale(NULL, "r_obs")(4), 4)
  expect_equal(ezdyn_own_scale(data.frame(dynare_name = "r_obs"), "r_obs")(4), 4)
  expect_equal(ezdyn_own_scale(data.frame(dynare_name = "r_obs", scale_factor = I(list(2))), "unemp")(4), 4)
  expect_equal(
    ezdyn_own_scale(data.frame(dynare_name = "r_obs", scale_factor = I(list(NA_real_))), "r_obs")(4),
    4
  )
})

test_that("ezdyn_own_scale applies a numeric or function scale_factor", {
  numeric_meta <- data.frame(dynare_name = "unemp", scale_factor = I(list(100)))
  expect_equal(ezdyn_own_scale(numeric_meta, "unemp")(0.041), 4.1)

  function_meta <- data.frame(dynare_name = "unemp", scale_factor = I(list(function(x) x + 1)))
  expect_equal(ezdyn_own_scale(function_meta, "unemp")(0.041), 1.041)
})

test_that("ezdyn_available_response_names adds derived names whose source is available", {
  varmeta <- data.frame(
    dynare_name = c("dr", "ddr", "infl_obs_ye"),
    derived_from = c("r_obs", "r_obs", "infl_obs"),
    derived_transform = c("diff", "diff2", "year_ended_sum"),
    stringsAsFactors = FALSE
  )

  actual <- ezdyn_available_response_names(c("r_obs", "unemp"), varmeta)

  expect_setequal(actual, c("r_obs", "unemp", "dr", "ddr"))
})

test_that("ezdyn_available_response_names omits derived names with an unavailable source", {
  varmeta <- data.frame(
    dynare_name = "infl_obs_ye",
    derived_from = "infl_obs",
    derived_transform = "year_ended_sum",
    stringsAsFactors = FALSE
  )

  actual <- ezdyn_available_response_names("r_obs", varmeta)

  expect_setequal(actual, "r_obs")
})

test_that("ezdyn_available_response_names is a no-op when varmeta has no derived_from column", {
  varmeta <- data.frame(dynare_name = "r_obs", stringsAsFactors = FALSE)

  actual <- ezdyn_available_response_names(c("r_obs", "unemp"), varmeta)

  expect_setequal(actual, c("r_obs", "unemp"))
})

test_that("ezdyn_augment_derived_irf_rows adds a transformed row per shock, ordered by t", {
  varmeta <- data.frame(
    dynare_name = "dr",
    derived_from = "r_obs",
    derived_transform = "diff",
    stringsAsFactors = FALSE
  )
  irf_data <- tibble::tibble(
    t = rep(1:3, 2),
    shock = rep(c("eps_r", "eps_y"), each = 3),
    dynare_name = "r_obs",
    value = c(1, 2, 4, 10, 10, 10)
  )

  actual <- ezdyn_augment_derived_irf_rows(irf_data, varmeta)
  dr_rows <- actual[actual$dynare_name == "dr", ]
  dr_rows <- dr_rows[order(dr_rows$shock, dr_rows$t), ]

  expect_equal(dr_rows$value[dr_rows$shock == "eps_r"], c(1, 1, 2))
  expect_equal(dr_rows$value[dr_rows$shock == "eps_y"], c(10, 0, 0))
})

test_that("ezdyn_augment_derived_irf_rows applies the derived row's own scale_factor on top of its transform", {
  varmeta <- data.frame(
    dynare_name = "dr",
    derived_from = "r_obs",
    derived_transform = "diff",
    scale_factor = I(list(2)),
    stringsAsFactors = FALSE
  )
  irf_data <- tibble::tibble(
    t = 1:3,
    shock = "eps_r",
    dynare_name = "r_obs",
    value = c(1, 2, 4)
  )

  actual <- ezdyn_augment_derived_irf_rows(irf_data, varmeta)
  dr_rows <- actual[actual$dynare_name == "dr", ][order(actual$t[actual$dynare_name == "dr"]), ]

  expect_equal(dr_rows$value, 2 * c(1, 1, 2))
})

test_that("ezdyn_augment_derived_irf_rows never overrides a natively present variable", {
  varmeta <- data.frame(
    dynare_name = "dr",
    derived_from = "r_obs",
    derived_transform = "diff",
    stringsAsFactors = FALSE
  )
  irf_data <- tibble::tibble(
    t = 1:2,
    shock = "eps_r",
    dynare_name = c("r_obs", "dr"),
    value = c(1, 99)
  )

  actual <- ezdyn_augment_derived_irf_rows(irf_data, varmeta)

  expect_equal(nrow(actual), 2)
  expect_equal(actual$value[actual$dynare_name == "dr"], 99)
})

test_that("ezdyn_augment_derived_irf_rows is a no-op when the source is unavailable", {
  varmeta <- data.frame(
    dynare_name = "dr",
    derived_from = "r_obs",
    derived_transform = "diff",
    stringsAsFactors = FALSE
  )
  irf_data <- tibble::tibble(t = 1, shock = "eps_r", dynare_name = "unemp", value = 4)

  expect_identical(ezdyn_augment_derived_irf_rows(irf_data, varmeta), irf_data)
})
