testthat::test_that("ezdyn_param_table_data validates, filters, and renders symbols", {
  model <- dashboard_test_model(include_param_table = TRUE)

  data <- ezdyn_param_table_data(model$M_)
  testthat::expect_setequal(names(data), c("Sector", "Symbol", "Description", "Value"))
  testthat::expect_equal(nrow(data), 2)
  testthat::expect_true(all(grepl("<math", data$Symbol))) # KaTeX-rendered MathML

  filtered <- ezdyn_param_table_data(model$M_, type = "Calibrated")
  testthat::expect_equal(nrow(filtered), 2)
  empty <- ezdyn_param_table_data(model$M_, type = "Estimated")
  testthat::expect_equal(names(empty), "Message")
})

testthat::test_that("ezdyn_param_table_data show_codes exposes Dyn_Symbol", {
  model <- dashboard_test_model(include_param_table = TRUE)

  data <- ezdyn_param_table_data(model$M_, show_codes = TRUE)
  testthat::expect_true("Dyn_Symbol" %in% names(data))
  testthat::expect_setequal(data$Dyn_Symbol, c("beta", "sigma"))
})

testthat::test_that("ezdyn_param_table_data errors clearly when param_table is missing", {
  model <- dashboard_test_model(include_param_table = FALSE)
  testthat::expect_error(ezdyn_param_table_data(model$M_), "param_table")
})

testthat::test_that("get_param_table dispatches katex vs dt output classes", {
  model <- dashboard_test_model(include_param_table = TRUE)

  katex_tbl <- get_param_table(model$M_, mode = "katex")
  testthat::expect_s3_class(katex_tbl, "knitr_kable")

  dt_tbl <- get_param_table(model$M_, mode = "dt")
  testthat::expect_s3_class(dt_tbl, "datatables")
  testthat::expect_s3_class(dt_tbl, "htmlwidget")
})

testthat::test_that("param_table_katex is deprecated but matches get_param_table(mode = 'katex')", {
  model <- dashboard_test_model(include_param_table = TRUE)

  testthat::expect_warning(
    legacy <- param_table_katex(model$M_, dp = 2),
    "deprecated"
  )
  current <- get_param_table(model$M_, mode = "katex", dp = 2)
  testthat::expect_identical(as.character(legacy), as.character(current))
})

testthat::test_that("ezdyn_variable_table_data lists default variables with units", {
  model <- dashboard_test_model()

  data <- ezdyn_variable_table_data(model$M_)
  testthat::expect_setequal(names(data), c("Variable", "Units"))
  testthat::expect_setequal(data$Variable, c("Cash Rate", "Inflation"))
})

testthat::test_that("ezdyn_variable_table_data show_codes exposes the raw dynare code", {
  model <- dashboard_test_model()

  data <- ezdyn_variable_table_data(model$M_, show_codes = TRUE)
  testthat::expect_true("Code" %in% names(data))
  testthat::expect_setequal(data$Code, c("r_obs", "infl_obs"))
})

testthat::test_that("ezdyn_variable_table_data errors clearly when varmeta is missing", {
  model <- dashboard_test_model()
  model$M_$varmeta <- NULL
  testthat::expect_error(ezdyn_variable_table_data(model$M_), "varmeta")
})

testthat::test_that("get_variable_table dispatches katex vs dt output classes", {
  model <- dashboard_test_model()

  testthat::expect_s3_class(get_variable_table(model$M_, mode = "katex"), "knitr_kable")
  testthat::expect_s3_class(get_variable_table(model$M_, mode = "dt"), "datatables")
})

testthat::test_that("ezdyn_shock_table_data falls back to the shock code when description is NA", {
  model <- dashboard_test_model(anticipated_shock = TRUE)

  data <- ezdyn_shock_table_data(model$M_, show_codes = TRUE)
  testthat::expect_setequal(data$Shock, c("Monetary policy shock", "eps_r_1"))
  testthat::expect_setequal(data$Code, c("eps_r", "eps_r_1"))
})

testthat::test_that("ezdyn_shock_table_data errors clearly when shock_meta is missing", {
  model <- dashboard_test_model()
  model$M_$shock_meta <- NULL
  testthat::expect_error(ezdyn_shock_table_data(model$M_), "shock_meta")
})

testthat::test_that("get_shock_table dispatches katex vs dt output classes", {
  model <- dashboard_test_model()

  testthat::expect_s3_class(get_shock_table(model$M_, mode = "katex"), "knitr_kable")
  testthat::expect_s3_class(get_shock_table(model$M_, mode = "dt"), "datatables")
})
