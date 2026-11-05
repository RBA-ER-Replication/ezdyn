#' Text RBA element
#' @rdname ezdyn_element_rba
ezdyn_element_text_rba <- function(family = NULL, face = NULL, colour = NULL,
                             size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                             color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE,
                             max.width = NULL, max.height = NULL, title.wrap = FALSE) {

  # `max.width`/`max.height`/`title.wrap` are accepted for call-site
  # compatibility but never read anywhere (no `element_grob` method is
  # registered for a custom class here) - they were always inert, even
  # under pre-S7 ggplot2. Returning a plain `ggplot2::element_text()`
  # unchanged (rather than re-tagging its `class()`) keeps this element
  # usable by ggplot2 >= 4.0's S7-based theme system: reassigning `class()`
  # (or `append()`-ing extra fields, which itself already drops all
  # attributes) strips the object's `S7_class` metadata, which later
  # crashes with "no applicable method for `@`" once ggplot2 tries to read
  # a property off it during rendering.
  ggplot2::element_text(
    family = family, face = face, colour = colour, size = size,
    hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight,
    color = color, margin = margin, debug = debug, inherit.blank = inherit.blank)

}

# Validate that a plot-size argument is a `grid::unit()` object.
check_unit <- function(x) {
  if (!grid::is.unit(x)) {
    stop("`plot.width`, `plot.height`, and `caption.width` must be created with `grid::unit()`.", call. = FALSE)
  }
  x
}

#' Set plot size
#'
#' This function sets the plot width and height and adjusts other theme elements
#' which depend on those.
#'
#' @param plot.width the width of the plot area in grid::units
#' @param plot.height the height of the plot area in grid::units
#' @param plot apply to plot.tile and plot.subtitle
#' @param panel apply to the panel title and subtitle
#' @param caption.width the width of the plot area for captions. Can differ from
#'   panel width because for multipanels sources/footnotes sit outside the plot.
ezdyn_plot_size_rba <- function(plot.width = grid::unit(190, "mm"), plot.height = grid::unit(140, "mm"),
                          plot = TRUE, panel = FALSE, caption.width = plot.width) {
  # Check both inputs are units
  plot.width <- check_unit(plot.width)
  plot.height <- check_unit(plot.height)

  # Convert both to consistent unit
  plot.width <- grid::convertUnit(plot.width, "mm")
  plot.height <- grid::convertUnit(plot.height, "mm")

  # Update all necessary theme elements. `plot.width`/`plot.height` are not
  # stored on the theme itself (that would require registering custom
  # `ggrba.*` elements via `ggplot2::register_theme_elements()`, which
  # nothing in this fallback path reads back); the sizing is instead carried
  # directly by the `max.width`/`max.height` fields on each element below.
  thm <- ggplot2::theme(
    plot.caption = ezdyn_element_note_text_rba(
      family = "",
      colour = "black",
      angle = 0,
      debug = FALSE,
      max.height = Inf,
      face = "plain",
      size = 16, margin = ggplot2::margin(1, 0, 1, 0, "mm"),
      vjust = 0,
      hjust = 0,
      lineheight = 0.9,
      max.width = caption.width)
  )

  if (isTRUE(plot)) {
    thm <- thm + ggplot2::theme(
      plot.title = ezdyn_element_text_rba(size = 28, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.9,
                                    margin = ggplot2::margin(0, 0, 2, 0, "mm"),
                                    title.wrap = F, max.width = plot.width),
      plot.subtitle = ezdyn_element_text_rba(size = 20, hjust = 0.5, vjust = 1, face = "plain", lineheight = 0.9,
                                       margin = ggplot2::margin(0, 0, 2, 0, "mm"), title.wrap = F, max.width = plot.width),
    )

  }

  if (isTRUE(panel)) {
    thm <- thm + ggplot2::theme(
      ggrba.panel.title = ezdyn_element_text_rba(
        size = 20, face = "plain", family = "",
        vjust = 0.5,
        title.wrap = FALSE, lineheight = 0.9,
        max.width = plot.width),
      ggrba.panel.subtitle = ezdyn_element_text_rba(
        size = 18, vjust = 0.5, lineheight = 0.9,
        max.width = plot.width),
    )

  }

  thm

}

#' @rdname ezdyn_element_rba
ezdyn_element_note_text_rba <- function(family = NULL, face = NULL, colour = NULL,
                                  size = NULL, hjust = 0, vjust = 0, angle = NULL, lineheight = NULL,
                                  color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE,
                                  max.width = NULL, max.height = NULL) {

  # See `ezdyn_element_text_rba()`: `max.width`/`max.height` are accepted for
  # call-site compatibility but never read, and re-tagging `class()` on the
  # S7-based `ggplot2::element_text` breaks it under ggplot2 >= 4.0.
  ezdyn_element_text_rba(
    family = family, face = face, colour = colour, size = size,
    hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight,
    color = color, margin = margin, debug = debug, inherit.blank = inherit.blank)

}




#' RBA theme
#'
#' @description This is a ggplot theme that matches Bank standards, which are
#' available
#' here: \href{https://trimweb.rba.gov.au/record/4345163}{Guide to Graphing at
#' RBA}. See \code{\link[ggplot2:theme]{ggplot2::theme()}} for extra explanation
#' of theme aspects.
#'
#' @note Applied by default when you use `ggrba`. If you are adding it manually,
#'   it should be used early when creating gg objects. This is because it
#'   overwrites existing settings: e.g. those set by `legend_rba()` or
#'   `ezdyn_plot_size_rba()`.
#'
#' @export
#' @family functions to theme RBA charts
ezdyn_theme_rba <- function() {
  black <- "#000000"
  .default.plot.width <- grid::unit(190, "mm")
  .default.plot.height <- grid::unit(140, "mm")
  .mm_per_pt <- 0.3528

  thm <- ggplot2::theme(
    # Top level elements
    line = ggplot2::element_line(colour = black, linewidth = .mm_per_pt),
    rect = ggplot2::element_rect(fill = NA, colour = NA, linewidth = .mm_per_pt),
    text = ggplot2::element_text(colour = black, family = "sans", face = "plain", size = 20),
    title = ezdyn_element_text_rba(size = 28, face = "bold", hjust = 0.5,
                                 margin = ggplot2::margin(0, 0, 1, 0, "mm"),
                                 max.width = .default.plot.width,
                                 title.wrap = TRUE),

    # Axes
    ## Title
    axis.title = ggplot2::element_blank(),
    ## Text
    axis.text.x = ggplot2::element_text(size = 20, margin = ggplot2::margin(5, 0, 0, 0, "mm"), vjust = 1, colour = black),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 5, 0, "mm"), vjust = 0, colour = black),
    axis.text.y = ggplot2::element_text(size = 20, margin = ggplot2::margin(0, 2, 0, 0, "mm"), hjust = 1, colour = black),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(0, 0, 0, 2, "mm"), hjust = 0, colour = black),
    ## Ticks
    axis.ticks = ggplot2::element_line(colour = black),
    axis.ticks.length = grid::unit(-2, "mm"),
    axis.ticks.y = ggplot2::element_blank(),

    # Legend
    legend.position = "bottom",
    legend.key = ggplot2::element_blank(),
    legend.key.size = grid::unit(4.5, "mm"),
    legend.text = ggplot2::element_text(size = 18),
    legend.box.background = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(t = -0.3, unit = "cm"),
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.direction = "horizontal",

    # Panel
    ## Border and background
    panel.border = ggplot2::element_rect(colour = black, fill = NA),
    panel.background = ggplot2::element_blank(),
    panel.ontop = FALSE,
    panel.grid.major = ggplot2::element_line(colour = "gray"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.spacing = grid::unit(0, "cm"),

    # Plot titles and captions
    plot.title = ezdyn_element_text_rba(size = 28, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.9,
                                  margin = ggplot2::margin(0, 0, 2, 0, "mm"), title.wrap = TRUE),
    plot.subtitle = ezdyn_element_text_rba(size = 20, hjust = 0.5, vjust = 1, face = "plain", lineheight = 0.9,
                                     margin = ggplot2::margin(0, 0, 2, 0, "mm"), max.width = .default.plot.width),
    plot.margin = ggplot2::margin(2, 2, 2, 2, unit = "mm"),
    # plot.margin = ggplot2::margin(0, 2, 2, 2, unit = "mm"),
    plot.background = ggplot2::element_rect(fill = NA, colour = NA),
    plot.title.position = "panel",
    plot.caption = ezdyn_element_note_text_rba(
      family = "",
      colour = "black",
      angle = 0,
      debug = FALSE,
      max.height = Inf,
      face = "plain",
      size = 16, margin = ggplot2::margin(1, 0, 1, 0, "mm"),
      vjust = 1,
      hjust = 0,
      lineheight = 0.9,
      max.width = .default.plot.width
    ),
    plot.caption.position = "panel",

    # Strip
    strip.background = ggplot2::element_blank(),
    strip.placement = "outside",
    strip.text = ezdyn_element_text_rba(size = 20),

    # Theme is complete
    complete = FALSE

  )

}

