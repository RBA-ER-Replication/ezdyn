testthat::test_that("import_baseline returns canonical display-name columns", {
  dynare <- load_dynare_irf_fixture()
  input <- data.frame(
    date = as.Date(c("2024-03-01", "2024-06-01")),
    r_obs = c(1, 2),
    infl_obs = c(0.5, 0.6),
    check.names = FALSE
  )

  actual <- import_baseline(input, dynare, models = list(DINGO = dynare))

  testthat::expect_s3_class(actual, "tbl_df")
  testthat::expect_named(actual, c("date", "Cash Rate", "Inflation"))
  testthat::expect_equal(actual$date, input$date)
  testthat::expect_equal(actual[["Cash Rate"]], input$r_obs)
  testthat::expect_equal(actual[["Inflation"]], input$infl_obs)
})

testthat::test_that("import_baseline adds a metadata-declared derived display-name column", {
  model <- list(
    M_ = list(varmeta = data.frame(
      dynare_name = c("r_obs", "dr"),
      display_name = c("Cash Rate", "Cash Rate Changes"),
      units = c("Level", "Change"),
      derived_from = c(NA_character_, "r_obs"),
      derived_transform = c(NA_character_, "diff"),
      scale_factor = c(1, 1),
      stringsAsFactors = FALSE
    )),
    oo_ = list()
  )
  class(model$M_) <- "dynare"
  class(model$oo_) <- "dynare"
  input <- data.frame(
    date = as.Date(c("2024-03-01", "2024-06-01", "2024-09-01")),
    r_obs = c(3.8, 4.0, 4.1)
  )

  actual <- import_baseline(input, model, models = list(DINGO = model))

  testthat::expect_equal(actual[["Cash Rate Changes"]], c(3.8, 0.2, 0.1))
})

testthat::test_that("import_baseline applies the derived row's own scale_factor on top of its transform", {
  model <- list(
    M_ = list(varmeta = data.frame(
      dynare_name = c("r_obs", "dr"),
      display_name = c("Cash Rate", "Cash Rate Changes"),
      units = c("Level", "Change"),
      derived_from = c(NA_character_, "r_obs"),
      derived_transform = c(NA_character_, "diff"),
      scale_factor = c(1, 2),
      stringsAsFactors = FALSE
    )),
    oo_ = list()
  )
  class(model$M_) <- "dynare"
  class(model$oo_) <- "dynare"
  input <- data.frame(
    date = as.Date(c("2024-03-01", "2024-06-01", "2024-09-01")),
    r_obs = c(3.8, 4.0, 4.1)
  )

  actual <- import_baseline(input, model, models = list(DINGO = model))

  testthat::expect_equal(actual[["Cash Rate Changes"]], 2 * c(3.8, 0.2, 0.1))
})

testthat::test_that("import_baseline leaves a derived column untouched if already supplied", {
  model <- list(
    M_ = list(varmeta = data.frame(
      dynare_name = c("r_obs", "dr"),
      display_name = c("Cash Rate", "Cash Rate Changes"),
      units = c("Level", "Change"),
      derived_from = c(NA_character_, "r_obs"),
      derived_transform = c(NA_character_, "diff"),
      scale_factor = c(1, 1),
      stringsAsFactors = FALSE
    )),
    oo_ = list()
  )
  class(model$M_) <- "dynare"
  class(model$oo_) <- "dynare"
  input <- data.frame(date = as.Date("2024-03-01"), check.names = FALSE)
  input[["Cash Rate"]] <- 3.8
  input[["Cash Rate Changes"]] <- 99

  actual <- import_baseline(input, model, models = list(DINGO = model))

  testthat::expect_equal(actual[["Cash Rate Changes"]], 99)
})

testthat::test_that("import_baseline rescales Dynare columns but not display-name columns", {
  dynare <- load_dynare_irf_fixture()
  scale_row <- match("r_obs", dynare$M_$varmeta$dynare_name)
  dynare$M_$varmeta$scale_factor[[scale_row]] <- 100
  dates <- as.Date(c("2024-03-01", "2024-06-01"))

  dynare_input <- data.frame(date = dates, r_obs = c(1, 2))
  display_input <- data.frame(date = dates, check.names = FALSE)
  display_input[["Cash Rate"]] <- c(1, 2)

  dynare_actual <- import_baseline(dynare_input, dynare, models = list(DINGO = dynare))
  display_actual <- import_baseline(display_input, dynare, models = list(DINGO = dynare))

  testthat::expect_equal(dynare_actual[["Cash Rate"]], c(100, 200))
  testthat::expect_equal(display_actual[["Cash Rate"]], c(1, 2))
})

testthat::test_that("import_baseline supports function-valued scale factors", {
  dynare <- load_dynare_irf_fixture()
  scale_row <- match("r_obs", dynare$M_$varmeta$dynare_name)
  dynare$M_$varmeta$scale_factor[[scale_row]] <- function(values) values * 100
  input <- data.frame(
    date = as.Date(c("2024-03-01", "2024-06-01")),
    r_obs = c(1, 2)
  )

  testthat::expect_silent(
    actual <- import_baseline(input, dynare, models = list(DINGO = dynare))
  )
  testthat::expect_equal(actual[["Cash Rate"]], c(100, 200))
})

testthat::test_that("import_baseline normalizes unnamed YYYYQq and explicit ISO date columns", {
  dynare <- load_dynare_irf_fixture()
  quarterly_input <- data.frame(r_obs = c(1, 2), check.names = FALSE)
  quarterly_input[["date_input"]] <- c("2024Q1", "2024q2")
  quarterly_input <- quarterly_input[c("date_input", "r_obs")]
  names(quarterly_input)[[1]] <- ""

  quarterly_actual <- import_baseline(quarterly_input, dynare, models = list(DINGO = dynare))
  testthat::expect_equal(quarterly_actual$date, as.Date(c("2024-03-01", "2024-06-01")))

  iso_input <- data.frame(when = c("2024-03-01", "2024-06-01"), r_obs = c(1, 2))
  iso_actual <- import_baseline(iso_input, dynare, models = list(DINGO = dynare), date_col = "when")
  testthat::expect_equal(iso_actual$date, as.Date(iso_input$when))

  posix_input <- data.frame(
    date = as.POSIXct(c("2024-03-01", "2024-06-01"), tz = "UTC"),
    r_obs = c(1, 2)
  )
  posix_actual <- import_baseline(posix_input, dynare, models = list(DINGO = dynare))
  testthat::expect_equal(posix_actual$date, as.Date(posix_input$date))
})

testthat::test_that("import_baseline reads existing xlsx baseline input", {
  dynare <- load_dynare_irf_fixture()
  actual <- import_baseline(
    irf_fixture_path("alt_paths", "test_data_matrix.xlsx"),
    dynare,
    models = list(DINGO = dynare)
  )

  testthat::expect_s3_class(actual, "tbl_df")
  testthat::expect_true(inherits(actual$date, "Date"))
  testthat::expect_true("Cash Rate" %in% names(actual))
})

testthat::test_that("baseline input reader uses openxlsx2 and accepts legacy xls workbooks", {
  xlsx_input <- tempfile(fileext = ".xlsx")
  expected_xlsx <- data.frame(
    date = as.Date(c("2024-03-01", "2024-06-01")),
    r_obs = c(1, 2)
  )
  writexl::write_xlsx(expected_xlsx, xlsx_input)

  actual_xlsx <- ezdyn_read_baseline_input(xlsx_input)
  testthat::expect_equal(names(actual_xlsx), names(expected_xlsx))
  testthat::expect_equal(actual_xlsx$r_obs, expected_xlsx$r_obs)

  fixture_xlsx <- irf_fixture_path("alt_paths", "test_data_matrix.xlsx")
  fixture_actual <- ezdyn_read_baseline_input(fixture_xlsx)
  testthat::expect_false(anyNA(fixture_actual$date))

  legacy_xls <- system.file("extdata", "datasets.xls", package = "readxl")

  testthat::expect_true(file.exists(legacy_xls))
  testthat::expect_silent(actual <- ezdyn_read_baseline_input(legacy_xls))
  testthat::expect_true(is.data.frame(actual))
  testthat::expect_gt(nrow(actual), 0)

  unsupported_input <- tempfile(fileext = ".csv")
  writeLines("date,r_obs\n2024-03-01,1", unsupported_input)
  testthat::expect_error(
    ezdyn_read_baseline_input(unsupported_input),
    "\\.xls.*\\.xlsx"
  )
})

testthat::test_that("import_baseline warns for unknown columns and rejects ambiguous input", {
  dynare <- load_dynare_irf_fixture()
  dates <- as.Date(c("2024-03-01", "2024-06-01"))
  input <- data.frame(date = dates, r_obs = c(1, 2), unknown_series = c(3, 4))

  testthat::expect_warning(
    actual <- import_baseline(input, dynare, models = list(DINGO = dynare)),
    "Ignoring unknown baseline column"
  )
  testthat::expect_named(actual, c("date", "Cash Rate"))

  duplicate_input <- data.frame(date = dates, r_obs = c(1, 2), check.names = FALSE)
  duplicate_input[["Cash Rate"]] <- c(1, 2)
  testthat::expect_error(
    import_baseline(duplicate_input, dynare, models = list(DINGO = dynare)),
    "Multiple input columns translate"
  )
})

testthat::test_that("import_baseline rejects invalid dates, models, and unit conflicts", {
  dynare <- load_dynare_irf_fixture()
  bad_dates <- data.frame(date = c("2024-03-01", "not-a-date"), r_obs = c(1, 2))
  duplicate_dates <- data.frame(date = as.Date(c("2024-03-01", "2024-03-01")), r_obs = c(1, 2))

  testthat::expect_error(
    import_baseline(bad_dates, dynare, models = list(DINGO = dynare)),
    "Baseline dates"
  )
  testthat::expect_error(
    import_baseline(duplicate_dates, dynare, models = list(DINGO = dynare)),
    "Baseline dates must be unique"
  )
  testthat::expect_error(
    import_baseline(duplicate_dates, dynare$M_, models = list(DINGO = dynare)),
    "full MOO pair"
  )
  valid_input <- data.frame(date = as.Date(c("2024-03-01", "2024-06-01")), r_obs = c(1, 2))
  testthat::expect_error(
    import_baseline(valid_input, dynare, models = list()),
    "non-empty named list"
  )

  other_model <- load_dynare_irf_fixture()
  unit_row <- match("r_obs", other_model$M_$varmeta$dynare_name)
  other_model$M_$varmeta$units[[unit_row]] <- "Percent"
  testthat::expect_error(
    import_baseline(valid_input, dynare, models = list(Other = other_model)),
    "Conflicting display units"
  )
})

# A minimal Dynare-class model with the given variable metadata.
baseline_test_model <- function(...) {
  varmeta <- data.frame(..., stringsAsFactors = FALSE)
  if (!("derived_from" %in% names(varmeta))) varmeta$derived_from <- NA_character_
  if (!("derived_transform" %in% names(varmeta))) varmeta$derived_transform <- NA_character_
  if (!("scale_factor" %in% names(varmeta))) varmeta$scale_factor <- 1
  model <- list(M_ = list(varmeta = varmeta), oo_ = list())
  class(model$M_) <- "dynare"
  class(model$oo_) <- "dynare"
  model
}

baseline_test_models <- function() {
  list(
    A = baseline_test_model(
      dynare_name = c("r_obs", "infl"),
      display_name = c("Cash Rate", "Inflation"),
      units = c("Per cent", "Per cent"),
      scale_factor = c(4, 1)
    ),
    B = baseline_test_model(
      dynare_name = c("i", "pi", "u"),
      display_name = c("Cash Rate", "Inflation", "Unemployment Rate"),
      units = c("Per cent", "Per cent", "Per cent")
    )
  )
}

testthat::test_that("import_baseline in display names keeps every model's display-name columns", {
  models <- baseline_test_models()
  input <- data.frame(date = as.Date(c("2024-03-01", "2024-06-01")), check.names = FALSE)
  input[["Cash Rate"]] <- c(4, 4.25)
  input[["Unemployment Rate"]] <- c(4.1, 4.2) # only model B has it

  actual <- import_baseline(input, models = models)

  testthat::expect_named(actual, c("date", "Cash Rate", "Unemployment Rate"))
  testthat::expect_equal(actual[["Cash Rate"]], c(4, 4.25)) # not rescaled
  testthat::expect_s3_class(actual, "ezdyn_baseline")
})

testthat::test_that("import_baseline in display names drops model codes, with a hint to use source_model", {
  models <- baseline_test_models()
  input <- data.frame(date = as.Date("2024-03-01"), r_obs = 1, check.names = FALSE)
  input[["Inflation"]] <- 0.5

  testthat::expect_warning(
    actual <- import_baseline(input, models = models),
    "Ignoring unknown baseline column\\(s\\): `r_obs`.*supply that model as `source_model`"
  )
  testthat::expect_named(actual, c("date", "Inflation"))
})

testthat::test_that("import_baseline in one model's codes rescales them and keeps other models' display names", {
  models <- baseline_test_models()
  input <- data.frame(date = as.Date(c("2024-03-01", "2024-06-01")), r_obs = c(1, 1.0625), check.names = FALSE)
  input[["Unemployment Rate"]] <- c(4.1, 4.2) # a display name of model B, not of the source model

  actual <- import_baseline(input, source_model = models$A, models = models)

  testthat::expect_named(actual, c("date", "Cash Rate", "Unemployment Rate"))
  testthat::expect_equal(actual[["Cash Rate"]], c(4, 4.25)) # x 4, model A's scale factor
  testthat::expect_equal(actual[["Unemployment Rate"]], c(4.1, 4.2))
})

testthat::test_that("import_baseline derives each variable from the first model that can, across models", {
  rate_changes <- function(transform, scale) {
    baseline_test_model(
      dynare_name = c("r", "dr"),
      display_name = c("Cash Rate", "Cash Rate Changes"),
      units = c("Per cent", "Percentage points"),
      derived_from = c(NA, "r"),
      derived_transform = c(NA, transform),
      scale_factor = c(1, scale)
    )
  }
  # Model C derives its year-ended changes from "Cash Rate Changes", which it
  # only has as a model variable: the value must come from another model.
  year_ended <- baseline_test_model(
    dynare_name = c("dr_raw", "dr_ye"),
    display_name = c("Cash Rate Changes", "Cash Rate Changes (year-ended)"),
    units = c("Percentage points", "Percentage points"),
    derived_from = c(NA, "dr_raw"),
    derived_transform = c(NA, "year_ended_sum")
  )
  input <- data.frame(date = as.Date(c("2024-03-01", "2024-06-01", "2024-09-01")), check.names = FALSE)
  input[["Cash Rate"]] <- c(3, 3.5, 4.5)

  first <- import_baseline(input, models = list(One = rate_changes("diff", 1), Two = rate_changes("diff", 2), C = year_ended))
  second <- import_baseline(input, models = list(Two = rate_changes("diff", 2), One = rate_changes("diff", 1), C = year_ended))

  testthat::expect_equal(first[["Cash Rate Changes"]], c(3, 0.5, 1))
  testthat::expect_equal(second[["Cash Rate Changes"]], 2 * c(3, 0.5, 1))
  testthat::expect_equal(first[["Cash Rate Changes (year-ended)"]], cumsum(c(3, 0.5, 1)))
})

testthat::test_that("import_baseline explains passing the list of models as source_model", {
  models <- baseline_test_models()
  input <- data.frame(date = as.Date("2024-03-01"), check.names = FALSE)
  input[["Inflation"]] <- 0.5

  testthat::expect_error(import_baseline(input, models), "`source_model` must be one model; pass the list of models as `models = ...`")
})