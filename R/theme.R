#' Create base theme elements
#'
#' @param base_font Base font family
#' @param title_font Title font family
#' @param base_size Base font size
#' @param colors Color palette to use
#' @param grid Show grid lines
#' @return A list of theme elements
#' @keywords internal
create_base_theme_elements <- function(base_font = "Open Sans",
                                       title_font = "Roboto",
                                       base_size = 11,
                                       colors = aiddata_colors,
                                       grid = TRUE) {

  # Grid lines
  grid_element <- if (grid) {
    ggplot2::element_line(color = colors$ad_gray90)
  } else {
    ggplot2::element_blank()
  }

  list(
    # Text elements with ggtext support
    text = ggplot2::element_text(
      family = base_font,
      size = base_size,
      color = colors$ad_gray30
    ),
    plot.title = ggtext::element_markdown(
      family = title_font,
      face = "bold",
      size = base_size * 1.4,
      color = colors$ad_wren_twilight,
      margin = ggplot2::margin(b = base_size)
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      family = base_font,
      size = base_size * 1.1,
      color = colors$ad_gray50,
      margin = ggplot2::margin(b = base_size * 0.8),
      lineheight = 1.2
    ),
    plot.caption = ggtext::element_markdown(
      family = base_font,
      size = base_size * 0.9,
      color = colors$ad_gray50,
      margin = ggplot2::margin(t = base_size * 0.8)
    ),

    # Grid elements
    panel.grid.major = grid_element,
    panel.grid.minor = ggplot2::element_blank(),

    # Axis elements
    axis.text = ggplot2::element_text(
      family = base_font,
      size = base_size * 0.9,
      color = colors$ad_gray50
    ),
    axis.title = ggtext::element_markdown(
      family = base_font,
      size = base_size,
      color = colors$ad_gray30,
      margin = ggplot2::margin(t = base_size * 0.5)
    ),

    # Legend elements
    legend.text = ggplot2::element_text(
      family = base_font,
      size = base_size * 0.9
    ),
    legend.title = ggtext::element_markdown(
      family = base_font,
      size = base_size,
      face = "bold",
      color = colors$ad_gray30
    ),

    # Strip text for facets
    strip.text = ggtext::element_markdown(
      family = base_font,
      size = base_size,
      face = "bold",
      color = colors$ad_wren_twilight
    ),

    # Background elements
    plot.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    strip.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),

    # Margins and spacing
    plot.margin = ggplot2::margin(
      t = base_size,
      r = base_size * 1.2,
      b = base_size,
      l = base_size
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )
}

#' AidData ggplot2 theme
#'
#' A ggplot2 theme that implements William & Mary brand guidelines and is optimized
#' for AidData's common visualization types. Requires the Roboto and Open Sans fonts
#' to be installed. Use install_aiddata_fonts() to install them if needed.
#'
#' The theme supports rich text formatting through the ggtext package:
#' - Use markdown formatting in titles, subtitles, and captions
#' - Automatic text wrapping for subtitles
#' - HTML-style text formatting (e.g., <b>bold</b>, <i>italic</i>)
#' - Color text using hex codes (e.g., <span style='color:#00313C'>text</span>)
#'
#' @param base_font Base font family (default: "Open Sans")
#' @param title_font Title font family (default: "Roboto")
#' @param base_size Base font size (default: 11)
#' @param grid Show major grid lines (default: TRUE)
#' @param map Optimize for map visualizations (default: FALSE)
#' @return A ggplot2 theme
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(
#'     title = "Miles per Gallon vs Weight",
#'     subtitle = "A demonstration of the **AidData theme** with rich text support.
#'                 Notice how the subtitle automatically wraps to multiple lines.",
#'     caption = "Data source: Motor Trend Magazine, 1974"
#'   ) +
#'   theme_aiddata()
#'
#' # Color formatting example
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   labs(
#'     title = "Vehicle Performance Analysis",
#'     subtitle = sprintf(
#'       "Comparing fuel efficiency across vehicles with
#'       <span style='color:%s'>4</span>,
#'       <span style='color:%s'>6</span>, and
#'       <span style='color:%s'>8</span> cylinders",
#'       aiddata_colors$ad_patina,
#'       aiddata_colors$ad_spirit_gold,
#'       aiddata_colors$ad_vine
#'     )
#'   ) +
#'   theme_aiddata()
theme_aiddata <- function(base_font = "Open Sans",
                          title_font = "Roboto",
                          base_size = 11,
                          grid = TRUE,
                          map = FALSE) {

  # Get available fonts with fallbacks
  actual_base_font <- get_available_font(base_font)
  actual_title_font <- get_available_font(title_font)

  if (actual_base_font != base_font || actual_title_font != title_font) {
    warning(
      "Recommended fonts not found. Using fallback fonts:\n",
      "  Base font: ", actual_base_font, "\n",
      "  Title font: ", actual_title_font, "\n",
      "Use install_aiddata_fonts() to install recommended fonts."
    )
  }

  # Get base elements with actual fonts
  elements <- create_base_theme_elements(
    base_font = actual_base_font,
    title_font = actual_title_font,
    base_size = base_size,
    grid = grid
  )

  # Create base theme
  theme <- do.call(ggplot2::theme, elements)

  # Start with minimal theme then add our modifications
  complete_theme <- ggplot2::theme_minimal(
    base_family = base_font,
    base_size = base_size
  ) + theme

  # Add map-specific modifications if needed
  if (map) {
    map_elements <- ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
    complete_theme <- complete_theme + map_elements
  }

  complete_theme
}

#' AidData complete theme
#'
#' A complete theme that applies both AidData's visual style and color scales.
#' This function combines theme_aiddata() with appropriate color scales.
#'
#' @inheritParams theme_aiddata
#' @param palette Color palette to use (default: "default")
#' @param discrete_fill Use discrete fill scale (default: TRUE)
#' @return A list containing theme and scale elements
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   theme_aiddata_complete()
theme_aiddata_complete <- function(base_font = "Open Sans",
                                   title_font = "Roboto",
                                   base_size = 11,
                                   grid = TRUE,
                                   palette = "default",
                                   discrete_fill = TRUE) {
  list(
    theme_aiddata(
      base_font = base_font,
      title_font = title_font,
      base_size = base_size,
      grid = grid
    ),
    scale_color_aiddata(palette = palette),
    scale_fill_aiddata(palette = palette, discrete = discrete_fill)
  )
}
