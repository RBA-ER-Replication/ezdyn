# TODO: Recall that the scales are not being applied to the HSDs.
# Future work should implement the scaling function, and then just have a warning be
# thrown that users should be careful with applying nonlinear scales to shock
# decompositions.


#' Convert HD array to long data frame.
#'
#' @param arr 3D array with dimensions [variable x shock x time].
#'
#' @return Data frame with columns `variable`, `shock`, `t`, `value`.
hd_matrix_to_df <- function(arr) {
	if (!is.array(arr) || length(dim(arr)) != 3L) {
		stop("`arr` must be a 3D array.")
	}

	dn <- dimnames(arr)
	if (is.null(dn) || length(dn) != 3L) {
		stop("`arr` must have dimnames for variable, shock, and time.")
	}

	if (is.null(dn[[1]])) dn[[1]] <- paste0("var_", seq_len(dim(arr)[1]))
	if (is.null(dn[[2]])) dn[[2]] <- paste0("shock_", seq_len(dim(arr)[2]))
	if (is.null(dn[[3]])) dn[[3]] <- paste0("t", seq_len(dim(arr)[3]))
	dimnames(arr) <- dn

	out <- as.data.frame.table(arr, responseName = "value", stringsAsFactors = FALSE)
	names(out) <- c("variable", "shock", "t", "value")
	out$variable <- as.character(out$variable)
	out$shock <- as.character(out$shock)
	out$t <- as.character(out$t)
	out
}



#' Plot historical decomposition from `ez_hd()` output.
#'
#' Mirrors `plot_pretty()` inputs, with added `shock_group` support.
#'
#' @param df Data frame created by `ez_hd()`.
#' @param dynare_names Optional vector of variable names to include.
#' @param display_names Optional vector of display names to include.
#' @param shocks Optional vector of shocks to include.
#' @param line_style "combined" or "split" (kept for API parity).
#' @param shock_group Optional string naming a grouping column in `df`.
#'   When provided, structural shocks are aggregated by this group.
#' @param bar_order Optional character vector specifying stack/legend order for bars.
#' @param interactive Logical; if `TRUE`, return an interactive plotly widget.
#' @param bar_colours colours for different bars
#'
#' @return List with `graph`, `graph_data`, and `graph_fname`.
#' @export
plot_pretty_hd <- function(df,
							dynare_names = NULL,
							display_names = NULL,
							shocks = NULL,
							shock_group = NULL,
							interactive = FALSE,
							bar_order = NULL,
							bar_colours = NULL) {


	if (!is.null(dynare_names)) df <- dplyr::filter(df, variable %in% dynare_names)
	if (!is.null(display_names)) df <- dplyr::filter(df, display_name %in% display_names)
	if (!is.null(shocks)) df <- dplyr::filter(df, shock %in% shocks)

	# TODO: remind myself why I am summing these values, I think it should only be 1 value per group anyway.
    actuals <- df |>
  		dplyr::filter(shock == "smoothed") |>
  		dplyr::group_by(t, variable, display_name, display_unit, model_name) |>
  		dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

	bars_df <- df |>
		dplyr::filter(shock != "smoothed", shock != "total")
	if (!is.null(shock_group)) {
		if (!is.character(shock_group) || length(shock_group) != 1 || !shock_group %in% names(df)) {
			stop("`shock_group` must be a single column name present in `df`.")
		}
		bars_df <- bars_df |>
        dplyr::mutate(shock_group = dplyr::case_when(shock == "residual" ~ "Residual",
                                                     (shock != "residual") & is.na(!!rlang::sym(shock_group)) ~ "Other",
                                                     .default=!!rlang::sym(shock_group))) |>
            dplyr::group_by(shock_group, t, variable, display_name, display_unit, model_name) |>
            dplyr::summarise(value = sum(value), shock=unique(shock_group))

	}

	# Optional manual order for stacked bars / legend
	if (!is.null(bar_order)) {
		if (!is.character(bar_order)) {
			stop("`bar_order` must be a character vector when provided.")
		}
		present <- unique(as.character(bars_df$shock))
		ordered_present <- bar_order[bar_order %in% present]
		remaining <- setdiff(present, ordered_present)
		bars_df$shock <- factor(bars_df$shock, levels = c(ordered_present, remaining))
	}

	bars_df <- bars_df |>
		dplyr::mutate(
			hover_text = sprintf(
				"Variable: %s<br>Shock: %s<br>t: %s<br>Value: %.4f",
				display_name, shock, t, value
			)
		)
	actuals <- actuals |>
		dplyr::mutate(
			hover_text = sprintf(
				"Variable: %s<br>Series: smoothed<br>t: %s<br>Value: %.4f",
				display_name, t, value
			)
		)
	# Handle shock colours
	if (is.null(bar_colours)) {
	  shock_levels <- if (is.factor(bars_df$shock)) levels(bars_df$shock) else unique(bars_df$shock)
	  colours <- grDevices::hcl.colors(length(unique(bars_df$shock)), "Dynamic") # Preset colours
	  bar_colours <- stats::setNames(colours, shock_levels)
	  if ("Residual" %in% names(bar_colours)) bar_colours[["Residual"]] <- "grey70"
	  if ("Other" %in% names(bar_colours)) bar_colours[["Other"]] <- "grey30"
	}


	graph <- ggplot2::ggplot(bars_df, ggplot2::aes(x = t, y = value, fill = shock, text = hover_text)) +
		ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
		ggplot2::geom_col(position = "stack", alpha = 0.9) +
		ggplot2::scale_fill_manual(values = bar_colours) +
		ggplot2::geom_line(
			data = actuals,
			mapping = ggplot2::aes(x = t, y = value, group = interaction(display_name, variable, model_name), text = hover_text),
			inherit.aes = FALSE,
			colour = "black",
			linewidth = 1
		) +
		ggplot2::facet_wrap(~display_name, scales = "free_y") +
		ggplot2::labs(x = "Quarter", y = NULL, fill = "Shock",
		              caption= "'Other' includes any shocks not part of the grouping,
		              'Residual' contains approximation error from initial conditions.") +
		ggplot2::theme_classic(base_size = 11 * 1.25)


	response_vars <- unique(stats::na.omit(df$display_name))
	if (length(response_vars) <= 1) {
		resp <- if (length(response_vars) == 1) response_vars[[1]] else "variable"
		graph <- graph + ggplot2::labs(title = sprintf("Historical decomposition of %s — %s", resp, unique(df$model_name)))
		graph_fname <- sprintf("Historical decomposition of %s", resp)
	} else {
		graph <- graph + ggplot2::labs(title = sprintf("Historical decomposition — %s", unique(df$model_name)))
		graph_fname <- "Historical decomposition"
	}

	if (isTRUE(interactive)) {
		if (!requireNamespace("plotly", quietly = TRUE)) {
			stop("Package 'plotly' is required when ggplotly = TRUE.")
		}
		graph <- plotly::ggplotly(graph, tooltip = "text")
	}

	graph_data <- dplyr::left_join(
		bars_df |>
			dplyr::select(t, display_name, variable, model_name, shock, value) |>
			tidyr::pivot_wider(names_from = "shock", values_from = "value"),
		actuals |>
			dplyr::select(t, display_name, variable, model_name, smoothed_total = value),
		by = c("t", "display_name", "variable", "model_name")
	)

	list(graph = graph, graph_data = graph_data, graph_fname = graph_fname)
}
#' Convert historical decomposition output to a tidy table.
#'
#' @param M_ M_ object.
#' @param oo_ oo_ object. If hd_override=NULL, the shock decomposition will be generated
#' by calling get_hd on M_ and oo_. This input will not be used if hd_override is not
#' NULL.
#' @param hd_override Supply HD manually based on either output of `get_hd()` or even
#' run_hd.m MATLAB output. If hd_override is supplied, oo_ can be NULL. Either:
#'   - a list containing `shock_decomposition` (e.g. output of `get_hd()`), or
#'   - a 3D array with dimensions `[variable x shock x time]`.
#'   - a long dataframe containing columns (`t`, `shock`, `variable`, `value`).
#'      This could have been outputted from the MATLAB version of run_hd.
#' @param M_ Dynare model object used to retrieve display names/units metadata
#'   analogously to `pretty_irf()`.
#' @param model_name Optional model name override; defaults to `M_$model_name`
#'   when available.
#'
#' @return A long data frame with at least columns:
#'   `t`, `shock`, `variable`, `model_name`, `display_name`, `display_unit`, `value`.
#'   Additional shock metadata columns are added from `M_$shock_meta` if available.
#' @details TODO: Need to apply rescaling to the shock output.
#'
#' @export
#'
ez_hd <- function(M_, oo_=NULL, hd_override=NULL, model_name = NULL) {

  if (!is.null(oo_)) {
    if (is.null(hd_override)) {
      hd <- get_hd(M_, oo_)
    } else {
      message("Both oo_ and hd_override are not NULL. Going to use shock decomposition
              output from hd_override, rather than re-generating shock decomposition
              using oo_.")
    }
  }
  # Convert output from get_hd (or MATLAB output) into a nice tidy df with
  # variable/shock display names/units.
	out <- if (is.list(hd) && !is.null(hd$shock_decomposition)) {
		hd_matrix_to_df(hd$shock_decomposition)
	} else if (is.data.frame(hd)) {
		hd
	} else {
		hd_matrix_to_df(hd)
	}

	# Misc validation
	required_cols <- c("t", "shock", "variable", "value")
	missing_cols <- setdiff(required_cols, names(out))
	if (length(missing_cols) > 0) {
		stop(sprintf("`hd` data is missing required columns: %s", paste(missing_cols, collapse = ", ")))
	}
	out$t <- as.numeric(out$t)
	out$shock <- as.character(out$shock)
	out$variable <- as.character(out$variable)

	out$model_name <- if (!is.null(model_name)) {
		as.character(model_name)
	} else if (!is.null(M_$model_name)) {
		as.character(M_$model_name)
	} else {
		NA_character_
	}

	# Add variable metadata similarly to pretty_irf().
	names_units <- display_names_and_units(
		unique(out$variable),
		M_,
		unit_symbol_irf = TRUE,
		unit_symbol_baseline = TRUE)
	out <- dplyr::left_join(out, names_units, by = c("variable" = "Variable"))

	# Join shock metadata from M_$shock_meta.
	if (!is.null(M_$shock_meta) && is.data.frame(M_$shock_meta)) {
		out <- dplyr::left_join(out, M_$shock_meta, by = "shock")
	} else {
		out <- out |>
		  dplyr::mutate(description = shock)
	}

	core_cols <- c("t", "shock", "variable", "model_name", "display_name", "display_unit", "value")
	extra_cols <- setdiff(names(out), core_cols)
	out <- out[, c(core_cols, extra_cols), drop = FALSE]

	# Re-order rows to match array order: time, variable, shock.
	#out$t <- factor(out$t, levels = unique(out$t))
	out$variable <- factor(out$variable, levels = unique(out$variable))
	out$shock <- factor(out$shock, levels = unique(out$shock))
	out <- out[order(out$t, out$variable, out$shock), , drop = FALSE]
	out$variable <- as.character(out$variable)
	out$shock <- as.character(out$shock)
	row.names(out) <- NULL

	out
}

#' Returns what shock groupings are available for use with ez_hd.
#' @param M_ dynare M_ object.
#' @export
ez_hd.available_shock_groupings <- function(M_) {
	if(!is.null(M_$shock_meta) && is.data.frame(M_$shock_meta)) {
		colnames(M_$shock_meta) |>
			setdiff(c("shock", "description", "is_measurement_error"))
	} else {
		return(NULL)
	}
}

