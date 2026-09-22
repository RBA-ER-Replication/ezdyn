alt_paths_fixture_forecast_start <- as.Date("2024-03-01")
alt_paths_fixture_forecast_end <- as.Date("2025-12-01")
alt_paths_fixture_data_start <- as.Date("1992-03-01")

load_alt_paths_fixture <- function() {
	list(
		baseline = readxl::read_xlsx(
			irf_fixture_path("alt_paths", "test_data_matrix.xlsx")
		) |>
			dplyr::mutate(date = as.Date(date)),
		alt_path = readxl::read_xlsx(
			irf_fixture_path("alt_paths", "test_supplied_alt_path.xlsx")
		) |>
			dplyr::mutate(Date = as.Date(Date))
	)
}

phase3_alt_path_inputs <- function(models) {
	dynare <- load_dynare_irf_fixture()
	fixtures <- load_alt_paths_fixture()
	baseline <- import_baseline(fixtures$baseline, dynare, models = models)
	forecast_dates <- seq(alt_paths_fixture_forecast_start, alt_paths_fixture_forecast_end, by = "quarter")
	cash_rate <- baseline[["Cash Rate"]][baseline$date %in% forecast_dates]
	alt_paths <- data.frame(date = forecast_dates, check.names = FALSE)
	alt_paths[["Test Path"]] <- cash_rate + seq_along(forecast_dates) / 10
	alt_paths[["Second Path"]] <- cash_rate - seq_along(forecast_dates) / 20
	list(baseline = baseline, alt_paths = alt_paths)
}

phase6_native_name_model <- function() {
	dynare <- load_dynare_irf_fixture()
	native_code <- "cash_rate_alt"
	forecast_dates <- seq(
		alt_paths_fixture_forecast_start,
		alt_paths_fixture_forecast_end,
		by = "quarter"
	)
	irf_wide <- get_irf(
		dynare$M_,
		dynare$oo_,
		shock_names = "eps_r",
		horizon = length(forecast_dates),
		data_frame = TRUE
	)
	irf_wide[[native_code]] <- irf_wide$r_obs
	irf_wide$r_obs <- NULL
	irf <- irf_wide |>
		tidyr::pivot_longer(-c(t, shock), names_to = "resp_var", values_to = "value")

	comparison <- list(
		M_ = list(
			model_name = "Native-name fixture",
			endo.vars = unique(irf$resp_var),
			exo.vars = unique(irf$shock),
			param_df = data.frame(),
			varmeta = dynare$M_$varmeta |>
				dplyr::mutate(
					dynare_name = dplyr::if_else(dynare_name == "r_obs", native_code, dynare_name)
				)
		),
		oo_ = list(irf = irf)
	)
	class(comparison$M_) <- "custom_moo"
	class(comparison$oo_) <- "custom_moo"
	comparison
}

testthat::test_that("get_alt_paths returns one baseline and every model-path response", {
	dynare <- load_dynare_irf_fixture()
	shock_row <- match("eps_r", dynare$M_$shock_meta$shock)
	dynare$M_$shock_meta$description[[shock_row]] <- "Monetary policy shock"
	models <- list(DINGO = dynare, Comparison = dynare)
	inputs <- phase3_alt_path_inputs(models)

	actual <- get_alt_paths(
		alt_paths = inputs$alt_paths,
		models = models,
		baseline = inputs$baseline,
		forecast_start = alt_paths_fixture_forecast_start,
		forecast_end = alt_paths_fixture_forecast_end,
		data_start = alt_paths_fixture_data_start
	)

	testthat::expect_true(all(c("date", "dynare_name", "display_name", "display_unit", "path_name", "model_name", "value", "effective_use_cd") %in% names(actual)))
	testthat::expect_setequal(unique(actual$path_name), c("Baseline", "Test Path", "Second Path"))
	testthat::expect_setequal(unique(stats::na.omit(actual$model_name)), c("DINGO", "Comparison"))

	baseline_rows <- actual |>
		dplyr::filter(path_name == "Baseline")
	testthat::expect_true(all(is.na(baseline_rows$model_name)))
	testthat::expect_true(all(is.na(baseline_rows$dynare_name)))
	testthat::expect_equal(anyDuplicated(baseline_rows[c("date", "display_name")]), 0L)

	instrument_rows <- actual |>
		dplyr::filter(path_name == "Test Path", dynare_name == "r_obs", date >= alt_paths_fixture_forecast_start) |>
		dplyr::arrange(model_name, date)
	testthat::expect_equal(
		instrument_rows$value,
		rep(inputs$alt_paths[["Test Path"]], length(models)),
		tolerance = 1e-6
	)
	testthat::expect_true(all(!instrument_rows$effective_use_cd))
})

testthat::test_that("get_alt_paths warns and falls back when a custom MOO lacks anticipated shocks", {
	dynare <- load_dynare_irf_fixture()
	forecast_dates <- seq(alt_paths_fixture_forecast_start, alt_paths_fixture_forecast_end, by = "quarter")
	fallback_irf <- get_irf(dynare$M_, dynare$oo_, "eps_r", length(forecast_dates), data_frame = TRUE) |>
		tidyr::pivot_longer(-c(t, shock), names_to = "resp_var", values_to = "value")
	fallback <- list(M_ = dynare$M_, oo_ = list(irf = fallback_irf))
	class(fallback$M_) <- "custom_moo"
	class(fallback$oo_) <- "custom_moo"
	fallback$M_$model_name <- "Fallback"
	models <- list(Fallback = fallback)
	inputs <- phase3_alt_path_inputs(models)

	testthat::expect_warning(
		actual <- get_alt_paths(
			alt_paths = inputs$alt_paths[, c("date", "Test Path")], models = models, baseline = inputs$baseline,
			use_cd = TRUE, lambda = 0.8,
			instrument_shock_name = "eps_r",
			forecast_start = alt_paths_fixture_forecast_start, forecast_end = alt_paths_fixture_forecast_end,
			data_start = alt_paths_fixture_data_start
		),
		"does not support anticipated shock"
	)
	testthat::expect_true(all(!actual$effective_use_cd[actual$path_name != "Baseline"]))
})

testthat::test_that("get_alt_paths validates baseline, paths, and models", {
	dynare <- load_dynare_irf_fixture()
	models <- list(DINGO = dynare)
	inputs <- phase3_alt_path_inputs(models)

	missing_instrument <- inputs$baseline
	missing_instrument[["Cash Rate"]] <- NULL
	testthat::expect_error(
		get_alt_paths(inputs$alt_paths, models, missing_instrument,
			instrument_shock_name = "eps_r",
			forecast_start = alt_paths_fixture_forecast_start, forecast_end = alt_paths_fixture_forecast_end,
			data_start = alt_paths_fixture_data_start),
		"does not have baseline metadata"
	)
	testthat::expect_error(
		get_alt_paths(inputs$alt_paths[-1, ], models, inputs$baseline,
			instrument_shock_name = "eps_r",
			forecast_start = alt_paths_fixture_forecast_start, forecast_end = alt_paths_fixture_forecast_end,
			data_start = alt_paths_fixture_data_start),
		"exactly match"
	)
	testthat::expect_error(
		get_alt_paths(inputs$alt_paths, list(), inputs$baseline,
			instrument_shock_name = "eps_r",
			forecast_start = alt_paths_fixture_forecast_start, forecast_end = alt_paths_fixture_forecast_end,
			data_start = alt_paths_fixture_data_start),
		"non-empty named list"
	)
})

testthat::test_that("format_alt_paths_long joins shared baseline history by display name", {
	dynare <- load_dynare_irf_fixture()
	models <- list(DINGO = dynare)
	inputs <- phase3_alt_path_inputs(models)
	paths <- get_alt_paths(
		alt_paths = inputs$alt_paths[, c("date", "Test Path")], models = models, baseline = inputs$baseline,
		instrument_shock_name = "eps_r",
		forecast_start = alt_paths_fixture_forecast_start, forecast_end = alt_paths_fixture_forecast_end,
		data_start = alt_paths_fixture_data_start
	)
	actual <- format_alt_paths_long(
		paths, output_vars = "r_obs", baseline_name_value = "Baseline",
		forecasts_start_date = alt_paths_fixture_forecast_start,
		output_start_date = alt_paths_fixture_data_start, output_end_date = alt_paths_fixture_forecast_end
	)
	testthat::expect_true(all(c("Model", "Mnemonic", "Variable Name", "Baseline Name", "Path Name") %in% names(actual)))
	testthat::expect_equal(actual$Mnemonic, "r_obs")
	testthat::expect_true(any(!is.na(actual[[as.character(alt_paths_fixture_data_start)]])))

	with_baseline <- format_alt_paths_long(
		paths, output_vars = "r_obs", baseline_name_value = "Baseline",
		forecasts_start_date = alt_paths_fixture_forecast_start,
		output_start_date = alt_paths_fixture_data_start, output_end_date = alt_paths_fixture_forecast_end,
		include_baseline_path = TRUE
	)
	baseline_row <- with_baseline |>
		dplyr::filter(`Path Name` == "Baseline")
	testthat::expect_equal(nrow(baseline_row), 1)
	testthat::expect_true(is.na(baseline_row$Model))
	testthat::expect_equal(
		baseline_row[[as.character(alt_paths_fixture_forecast_start)]],
		inputs$baseline[["Cash Rate"]][inputs$baseline$date == alt_paths_fixture_forecast_start]
	)
	testthat::expect_error(
		format_alt_paths_long(
			paths, output_vars = "r_obs", baseline_name_value = "Baseline",
			forecasts_start_date = alt_paths_fixture_forecast_start,
			output_start_date = alt_paths_fixture_data_start, output_end_date = alt_paths_fixture_forecast_end,
			include_baseline_path = NA
		),
		"include_baseline_path"
	)
})

testthat::test_that("get_alt_paths shares display baselines across different native codes", {
	dingo <- load_dynare_irf_fixture()
	comparison <- phase6_native_name_model()
	models <- list(DINGO = dingo, Comparison = comparison)
	inputs <- phase3_alt_path_inputs(models)

	actual <- get_alt_paths(
		alt_paths = inputs$alt_paths[, c("date", "Test Path")],
		models = models,
		baseline = inputs$baseline,
		instrument_shock_name = "eps_r",
		forecast_start = alt_paths_fixture_forecast_start,
		forecast_end = alt_paths_fixture_forecast_end,
		data_start = alt_paths_fixture_data_start
	)

	baseline_rows <- actual |>
		dplyr::filter(path_name == "Baseline", display_name == "Cash Rate")
	testthat::expect_equal(anyDuplicated(baseline_rows$date), 0L)

	comparison_rows <- actual |>
		dplyr::filter(
			model_name == "Comparison",
			path_name == "Test Path",
			display_name == "Cash Rate",
			date >= alt_paths_fixture_forecast_start
		) |>
		dplyr::arrange(date)
	testthat::expect_true(all(comparison_rows$dynare_name == "cash_rate_alt"))
	testthat::expect_equal(comparison_rows$value, inputs$alt_paths[["Test Path"]], tolerance = 1e-6)
})

