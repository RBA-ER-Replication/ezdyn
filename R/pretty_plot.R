# Code to automatically plot IRFs from get_irf, get_irf_target, get_irf_target_gabaix.

#' Pick the first non-empty value while enforcing uniqueness.
#'
#' @param x Vector of candidate values.
#' @param default Fallback value if no non-empty values exist.
#'
#' @return A single value from `x` or `default` if all values are empty.
#' @export
irf_pick_first_nonempty <- function(x, default="") {
	x <- x[!is.na(x) & x != ""]
	unique_vals <- unique(x)
	if (length(unique_vals) == 0) {
		default
	} else if (length(unique_vals) > 1) {
		stop("Multiple unit symbols found for a response variable; expected a single unique symbol.")
	} else {
		unique_vals[[1]]
	}
}

#' Filter pretty IRF data by response names and shocks.
#'
#' @param df Data frame created by `pretty_irf`.
#' @param dynare_names Optional vector of dynare names to include.
#' @param display_names Optional vector of display names to include.
#' @param shocks Optional vector of shocks to include.
#'
#' @return Filtered data frame.
#' @export
irf_filter_pretty <- function(df, dynare_names=NULL, display_names=NULL, shocks=NULL) {
	response_filter <- unique(c(dynare_names, display_names))
	is_alt_path <- ezdyn_is_alt_path(df)

	if (!is.null(response_filter)) {
		if (is_alt_path) {
			alternative_models <- df |>
				dplyr::filter(path_name != "Baseline") |>
				dplyr::pull(model_name) |>
				unique()
			matching_display_names <- purrr::map_dfr(dynare_names, function(requested_dynare_name) {
				matches <- df |>
					dplyr::filter(path_name != "Baseline", dynare_name == requested_dynare_name) |>
					dplyr::distinct(model_name, display_name)
				if (!setequal(matches$model_name, alternative_models) ||
					length(unique(matches$display_name)) != 1) {
					stop(
						sprintf(
							"For alternative paths, Dynare name `%s` must identify one display name in every model; use `display_names` instead.",
							requested_dynare_name
						),
						call. = FALSE
					)
				}
				tibble::tibble(display_name = matches$display_name[[1]])
			}) |>
				base::`[[`("display_name")
			display_filter <- unique(c(display_names, matching_display_names))
			df <- df |>
				dplyr::filter(
					dynare_name %in% dynare_names | display_name %in% display_filter
				)
		} else {
			df <- df |>
				dplyr::filter(dynare_name %in% dynare_names | display_name %in% display_names)
		}
	}
	if (!is.null(shocks) && !is_alt_path) {
		df <- df |>
			dplyr::filter(shock %in% shocks)
	}
	df
}

#' Build a named list of plot unit symbols for facet panels.
#'
#' @param df Data frame created by [pretty_irf()] or [get_alt_paths()].
#'
#' @return Named list mapping `display_name` to unit symbols.
#' @export
irf_build_unit_symbols <- function(df) {
	unit_column <- if (ezdyn_is_alt_path(df)) "unit_symbol_baseline" else "unit_symbol_irf"
	if (!(unit_column %in% names(df))) {
		stop(sprintf("Plot data must contain `%s`.", unit_column), call. = FALSE)
	}

	unit_map <- tibble::tibble(
		display_name = df$display_name,
		unit_symbol = df[[unit_column]]
	) |>
		dplyr::group_by(display_name) |>
		dplyr::summarise(unit_symbol = irf_pick_first_nonempty(unit_symbol), .groups = "drop")
	stats::setNames(as.list(unit_map$unit_symbol), unit_map$display_name)
}

#' Add line labels for plotting based on IRF shocks or alternative-path names.
#'
#' @param df Data frame created by `pretty_irf`.
#' @param line_style How to treat shock/model combinations for lines.
#' @param baseline_name Label used for an alternative-path baseline.
#'
#' @return Data frame with a `line_label` column.
#' @export
irf_add_line_labels <- function(df, line_style=c("combined", "split"),
                                baseline_name="Baseline") {
	line_style <- match.arg(line_style)
	if (ezdyn_is_alt_path(df)) {
		path_models <- df |>
			dplyr::filter(path_name != baseline_name) |>
			dplyr::distinct(path_name, model_name) |>
			dplyr::count(path_name, name = "model_count")
		multi_model_paths <- path_models |>
			dplyr::filter(model_count > 1) |>
			dplyr::pull(path_name)

		return(df |>
			dplyr::mutate(
				line_label = dplyr::case_when(
					path_name == baseline_name ~ baseline_name,
					path_name %in% multi_model_paths ~ sprintf("%s (%s)", path_name, model_name),
					.default = path_name
				)
			))
	}

	if (length(unique(df$model_name)) > 1) {
		df <- df |>
			dplyr::mutate(line_label = sprintf("%s (%s)", shock, model_name))
	} else {
		df <- df |>
			dplyr::mutate(line_label = shock)
	}
	df
}

# Identify data returned by `get_alt_paths()`.
ezdyn_is_alt_path <- function(df) {
	is.data.frame(df) && all(c("date", "path_name") %in% names(df))
}

# Validate and optionally collapse manually duplicated shared Baseline rows.
ezdyn_prepare_alt_path_data <- function(df, baseline_name, collapse_baseline) {
	if (!inherits(df$date, "Date") || anyNA(df$date) ||
		anyNA(df$path_name) || any(df$path_name == "")) {
		stop("Alternative-path data must contain non-missing `Date` values and path names.", call. = FALSE)
	}
	if (!is.logical(collapse_baseline) || length(collapse_baseline) != 1 || is.na(collapse_baseline)) {
		stop("`collapse_baseline` must be one logical value.", call. = FALSE)
	}

	baseline <- df |>
		dplyr::filter(path_name == baseline_name)
	alternatives <- df |>
		dplyr::filter(path_name != baseline_name)
	duplicate_baseline <- baseline |>
		dplyr::group_by(date, display_name) |>
		dplyr::summarise(
			n_rows = dplyr::n(),
			n_values = dplyr::n_distinct(value, na.rm = FALSE),
			.groups = "drop"
		) |>
		dplyr::filter(n_rows > 1)

	if (any(duplicate_baseline$n_values > 1)) {
		stop("Duplicate Baseline rows must have identical values for each date and display name.", call. = FALSE)
	}
	if (nrow(duplicate_baseline) > 0 && !collapse_baseline) {
		stop("Duplicate Baseline rows require `collapse_baseline = TRUE`.", call. = FALSE)
	}
	if (isTRUE(collapse_baseline)) {
		baseline <- baseline |>
			dplyr::distinct(date, display_name, .keep_all = TRUE)
	}

	if ("model_name" %in% names(baseline)) {
		baseline <- baseline |>
			dplyr::mutate(model_name = NA_character_)
	}
	if ("dynare_name" %in% names(baseline)) {
		baseline <- baseline |>
			dplyr::mutate(dynare_name = NA_character_)
	}

	dplyr::bind_rows(baseline, alternatives)
}

# Build deterministic line colours, reserving the requested colour for Baseline.
ezdyn_pretty_line_colours <- function(line_labels, baseline_name, baseline_colour) {
	if (!is.character(baseline_colour) || length(baseline_colour) != 1 ||
		is.na(baseline_colour) || baseline_colour == "") {
		stop("`baseline_colour` must be one non-empty colour value.", call. = FALSE)
	}
	line_labels <- unique(line_labels)
	other_labels <- setdiff(line_labels, baseline_name)
	colours <- grDevices::hcl.colors(max(length(other_labels), 1L), palette = "Dynamic")
	colours <- rep_len(colours, length(other_labels))
	names(colours) <- other_labels
	if (baseline_name %in% line_labels) {
		colours <- c(stats::setNames(baseline_colour, baseline_name), colours)
	}
	colours
}

# Omit overlapping Date labels at the rendered width.
ezdyn_alt_path_date_scale <- function() {
	ggplot2::scale_x_date(
		guide=ggplot2::guide_axis(check.overlap=TRUE)
	)
}

#' Build a graph from pretty IRF or alternative-path data.
#'
#' @param df Data frame created by [pretty_irf()] or [get_alt_paths()].
#' @param line_style How to treat shock/model combinations for lines.
#' @param plotter Plotting backend. `"ggrba"` (default) or `"ggplot"`.
#' @param custom_title Provide custom title if TRUE.
#' @param baseline_name Label used for an alternative-path baseline.
#' @param baseline_colour Colour used for an alternative-path baseline.
#'
#' @return List containing the `graph` and `graph_fname`.
#' @export
plot_pretty_graph <- function(df, line_style=c("combined", "split"),
							   plotter=c("ggrba", "ggplot"),
							   custom_title=NULL, baseline_name="Baseline",
							   baseline_colour="royalblue") {
	line_style <- match.arg(line_style)
	plotter <- match.arg(plotter)
	if (plotter == "ggrba" && !requireNamespace("ggrba", quietly = TRUE)) {
		stop("`plotter = \"ggrba\"` requires the optional ggrba package. Use `plotter = \"ggplot\"` or install ggrba.", call. = FALSE)
	}
	is_alt_path <- ezdyn_is_alt_path(df)
	unit_column <- if (is_alt_path) "unit_symbol_baseline" else "unit_symbol_irf"
	ezdyn_validate_pretty_irf_units(df, unit_symbol_column=unit_column)
	response_vars <- unique(df$display_name)
	unit_symbols <- irf_build_unit_symbols(df)
	subtitle_ <- unique(df$display_unit)
	df_plot <- df |>
		dplyr::filter(!is.na(value))
	unit_labels <- unname(unit_symbols[response_vars])
	y_breaks_common <- scales::breaks_pretty(n = 5)
	graph_fname <- if (length(response_vars) == 1) {
		if (is_alt_path) sprintf("Alternative policy path for %s", response_vars) else sprintf("Responses of %s", response_vars)
	} else {
		if (is_alt_path) "Alternative policy paths" else "IRF Responses"
	}

	if (plotter == "ggrba") {
		graph <- ggrba::ggrba(data=df_plot) +
			(if (is_alt_path) ezdyn_alt_path_date_scale() else ggrba::scale_x_continuous_rba(units="qtrs.")) +
			(if (is_alt_path) ggplot2::geom_blank() else ggplot2::geom_hline(yintercept=0)) +
			ggrba::legend_rba()
	} else {
		df_plot$display_name_unit <- sprintf(
			"%s (%s, %s)",
			df_plot$display_name,
			df_plot$display_unit,
			df_plot[[unit_column]]
		)
		graph <- ggplot2::ggplot(df_plot) +
			(if (is_alt_path) ezdyn_alt_path_date_scale() else ggplot2::geom_hline(yintercept = 0)) +
			ggplot2::scale_y_continuous(breaks = y_breaks_common) +
			ggplot2::labs(x = if (is_alt_path) NULL else "Quarter") +
			ggplot2::theme_minimal()
	}

	line_layer <- if (is_alt_path || line_style == "combined") {
		if (is_alt_path) {
			ggplot2::geom_line(ggplot2::aes(x=date, y=value, group=line_label, colour=line_label))
		} else {
			ggplot2::geom_line(ggplot2::aes(x=t, y=value, group=line_label, colour=line_label))
		}
	} else {
		ggplot2::geom_line(ggplot2::aes(x=t, y=value, group=interaction(shock, model_name),
																						colour=shock, linetype=model_name))
	}
	graph <- graph + line_layer

	if (plotter == "ggplot" || is_alt_path) {
		colour_levels <- if (is_alt_path || line_style == "combined") {
			unique(df_plot$line_label)
		} else {
			unique(df_plot$shock)
		}
		colour_values <- if (is_alt_path) {
			ezdyn_pretty_line_colours(colour_levels, baseline_name, baseline_colour)
		} else {
			values <- grDevices::hcl.colors(max(length(colour_levels), 1L), palette = "Dynamic")
			values <- rep_len(values, length(colour_levels))
			names(values) <- colour_levels
			values
		}
		graph <- graph +
			ggplot2::scale_colour_manual(values = colour_values, name = NULL) +
			ggplot2::guides(colour = ggplot2::guide_legend(title = NULL))
	}

	if (length(response_vars) == 1) {
	  if (is.null(custom_title)) {
	    custom_title <- if (is_alt_path) {
				sprintf("Alternative policy path for %s", response_vars)
			} else {
				sprintf("Response of %s", response_vars)
			}
	  }
		if (plotter == "ggrba") {
			graph <- graph +
				ggplot2::labs(title=custom_title, subtitle=subtitle_) +
				ggrba::scale_y_continuous_rba(units=unname(unit_symbols[[response_vars]]))
		} else {
			graph <- graph +
				ggplot2::labs(
					title=custom_title,
					subtitle=subtitle_,
					y=unit_labels
				)
		}
	} else {
	  if (is.null(custom_title)) {
	    custom_title <- if (is_alt_path) "Alternative policy paths" else "IRF Responses"
	  }

		if (plotter == "ggrba") {
			graph <- graph +
				ggrba::facet_rba(~display_name, same_axis=F, y_units=unit_symbols, ncol=2) +
				ggrba::multipanel_title_rba(title=custom_title) +
				ggrba::multipanel_legend_rba()
		} else {
			graph <- graph +
				ggplot2::facet_wrap(~display_name_unit, scales = "free_y", ncol = 3) +
				#ggplot2::geom_text(ggplot2::aes(x=3, y=(ceiling(length(unique(df$display_name_unit))/3)), label=unit_symbol_irf, fontface='bold', size=4)) +
				ggplot2::labs(title = custom_title) +
				ggplot2::theme(
					panel.spacing.y = grid::unit(1.2, "lines"),
					legend.position = "bottom"
				)
		}
	}

	list(graph=graph, graph_fname=graph_fname)
}

#' @rdname plot_pretty_graph
#' @export
irf_plot_pretty_graph <- function(...) {
	plot_pretty_graph(...)
}

# Core Pretty IRF Functions ----------------------------------------------

#' Convert IRF into a pretty format with names and units.
#' @param irf_data The IRF data to be converted. Needs columns t, shock, and the response variables.
#' @param M_ M_ object
#' @return A data frame with columns t, shock, response variable name, response variable value, and units.
#' @export
pretty_irf <- function(irf_data, M_) {
    # Rescale each of the response variables
    irf_data <- irf_data |>
        dplyr::group_by(shock) |>
        dplyr::arrange(t) |>
        dplyr::group_map(~ {
            t_vals <- .x$t
            .x |>
                dplyr::select(-t) |>
                rescale(M_) |>
                dplyr::mutate(t=t_vals, shock=.y$shock)
        }, .keep=F) |>
        dplyr::bind_rows() |>
        # TODO: rename dynare_name to be something more generic.
        tidyr::pivot_longer(cols = -c(t, shock), names_to = "dynare_name", values_to = "value")
    # Get the names and units for each of the response variables.
    names_units <- display_names_and_units(unique(irf_data$dynare_name),  M_, unit_symbol_irf=T, unit_symbol_baseline=T) |>
		dplyr::mutate(display_unit = ifelse(is.na(display_unit), "", display_unit))

	irf_data |>
        dplyr::left_join(names_units, by = dplyr::join_by(dynare_name == Variable)) |>
        dplyr::mutate(model_name = M_$model_name)
}


#' Plot pretty IRF or alternative-path output.
#'
#' @param df Data frame created by [pretty_irf()] or [get_alt_paths()].
#' @param display_names Optional vector of display names to include.
#' @param dynare_names Optional vector of Dynare names to include.
#'   For alternative paths, a requested Dynare name is first mapped to its
#'   display name, which keeps the shared Baseline and matching responses from
#'   all models. Prefer `display_names` for unambiguous multi-model selection:
#'   the same Dynare name can identify different variables across models.
#' @param shocks Optional vector of shocks to include (default all).
#' @param line_style "combined" uses a single legend for shock-model pairs; "split"
#'   uses colour for shock and linetype for model.
#' @param plotter Plotting backend. `"ggrba"` (default) or `"ggplot"`.
#' @param title Provide a custom title for the chart.
#' @param cumulate Cumulate the response variables. Can be FALSE (default), TRUE, or a
#' string vector for which display names the responses should be cumulated for. TODO:
#' Not currently implemented.
#' @param baseline_name Alternative-path label that identifies the shared baseline.
#' @param baseline_colour Colour used for the shared alternative-path baseline.
#' @param collapse_baseline Whether to collapse identical manually duplicated
#'   alternative-path baseline rows. Non-identical duplicates always error.
#' @details
#' For each `display_name`, the plotting pipeline requires one unique non-empty
#' value for `units`/`display_unit`. IRFs use `unit_symbol_irf`; alternative
#' paths use `unit_symbol_baseline` because their values are levels.
#' Alternative-path data are recognised by their `date` and `path_name` columns.
#' They use `date` on the x-axis and do not apply `shocks` filtering.
#'
#' @return List with `graph`, `graph_data`, and `graph_fname`.
#' @export
plot_pretty <- function(df,
									display_names=NULL,
									dynare_names=NULL,
									shocks=NULL,
									line_style=c("combined", "split"),
									plotter=c("ggrba", "ggplot"),
									title=NULL,
									cumulate=FALSE,
									baseline_name="Baseline",
									baseline_colour="royalblue",
									collapse_baseline=TRUE) {
	plotter <- match.arg(plotter)
	is_alt_path <- ezdyn_is_alt_path(df)
	if (is_alt_path) {
		df <- ezdyn_prepare_alt_path_data(df, baseline_name, collapse_baseline)
	}

	df <- irf_filter_pretty(df, dynare_names= dynare_names, display_names=display_names, shocks=shocks)
	df <- irf_add_line_labels(df, line_style=line_style, baseline_name=baseline_name)

# 	if ((is.logical(cumulate) && cumulate) || (is.character(cumulate))) {
#     if (is.logical(cumulate)) {
#       cumulate <- display_names
#     }
# 	}


	out <- plot_pretty_graph(
		df,
		line_style=line_style,
		plotter=plotter,
		custom_title=title,
		baseline_name=baseline_name,
		baseline_colour=baseline_colour
	)
	graph_data <- if (is_alt_path) {
		df |>
			tidyr::pivot_wider(
				id_cols=c("date", "display_name", "path_name", "model_name"),
				names_from="line_label",
				values_from="value"
			)
	} else {
		df |>
			tidyr::pivot_wider(id_cols=c("t", "display_name", "shock", "model_name"), names_from="line_label", values_from="value")
	}
	list(graph=out$graph, graph_data=graph_data, graph_fname=out$graph_fname)
}

#' @rdname plot_pretty
#' @export
plot_pretty_irf <- function(df,
								dynare_names=NULL,
								display_names=NULL,
								shocks=NULL,
								line_style=c("combined", "split"),
								plotter=c("ggrba", "ggplot"),
								title=NULL,
								cumulate=FALSE,
								baseline_name="Baseline",
								baseline_colour="royalblue",
								collapse_baseline=TRUE) {
	plot_pretty(
		df=df,
		display_names=display_names,
		dynare_names=dynare_names,
		shocks=shocks,
		line_style=line_style,
		plotter=plotter,
		title=title,
		cumulate=cumulate,
		baseline_name=baseline_name,
		baseline_colour=baseline_colour,
		collapse_baseline=collapse_baseline
	)
}

