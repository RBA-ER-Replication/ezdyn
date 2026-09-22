# Model-specific plot colour, configured at model-load time -----------------

testthat::test_that("custom_moo() stores and validates an optional model colour", {
	moo <- custom_moo(
		irf = data.frame(t = 1, shock = "eps_r", resp_var = "r_obs", value = 1),
		model_name = "MyModel",
		colour = "firebrick"
	)
	testthat::expect_identical(moo$M_$model_colour, "firebrick")

	unset <- custom_moo(
		irf = data.frame(t = 1, shock = "eps_r", resp_var = "r_obs", value = 1),
		model_name = "MyModel"
	)
	testthat::expect_null(unset$M_$model_colour)

	testthat::expect_error(
		custom_moo(
			irf = data.frame(t = 1, shock = "eps_r", resp_var = "r_obs", value = 1),
			colour = c("firebrick", "olivedrab1")
		),
		"`colour` must be NULL or one non-empty colour value"
	)
})

testthat::test_that("read_dynare() stores an optional model colour", {
	dynare <- read_dynare(
		path = irf_fixture_path("dynare", "test_model_dynare.json"),
		path_meta = irf_fixture_path("dynare", "test_metadata_dynare.xlsx"),
		model_name = "Dynare fixture",
		colour = "#FFB611"
	)
	testthat::expect_identical(dynare$M_$model_colour, "#FFB611")

	testthat::expect_error(
		read_dynare(
			path = irf_fixture_path("dynare", "test_model_dynare.json"),
			colour = ""
		),
		"`colour` must be NULL or one non-empty colour value"
	)
})

testthat::test_that("pretty_irf() output carries a model's configured colour", {
	dynare <- load_dynare_irf_fixture()
	dynare$M_$model_colour <- "firebrick"

	pretty <- get_irf(dynare$M_, dynare$oo_, "eps_r", horizon = 2, pretty = TRUE)
	testthat::expect_true(all(pretty$model_colour == "firebrick"))

	unset <- load_dynare_irf_fixture()
	pretty_unset <- get_irf(unset$M_, unset$oo_, "eps_r", horizon = 2, pretty = TRUE)
	testthat::expect_true(all(is.na(pretty_unset$model_colour)))
})
