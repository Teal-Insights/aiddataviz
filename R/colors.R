#' AidData color system based on William & Mary brand guidelines
#'
#' Defines the official William & Mary brand colors as used by AidData and
#' provides color scales for use in ggplot2 visualizations.
#'
#' @section Primary Colors:
#' The primary William & Mary colors form the core of the brand identity:
#' - Wren Twilight (#00313C)
#' - Vine (#84344E)
#' - Patina (#00B388)
#' - Spirit Gold (#F0B323)
#' - Silver (#D0DED4)
#'
#' @section Color Palettes:
#' Several pre-defined palettes are available:
#' - default: Primary W&M colors
#' - alt: Alternative combo of secondary/tertiary colors
#' - sequential_green: Green-based sequential scale
#' - sequential_gold: Gold-based sequential scale
#' - diverging_green_gold: Green to gold diverging scale
#' - diverging_vine_sky: Vine to sky blue diverging scale
#'
#' @name aiddata_colors
NULL

#' @rdname aiddata_colors
#' @export
aiddata_colors <- list(
  # Primary palette
  ad_wren_twilight = "#00313C",
  ad_vine = "#84344E",
  ad_patina = "#00B388",
  ad_spirit_gold = "#F0B323",
  ad_silver = "#D0DED4",

  # Secondary & tertiary colors
  ad_colonial_yellow = "#CAB64B",
  ad_college_sky = "#64CCC9",
  ad_weathered_brick = "#E56A54",
  ad_moss = "#789D4A",
  ad_college_woods = "#789F90",
  ad_slate = "#5B6770",
  ad_griffin_green = "#183028",

  # Neutral grays for accessibility
  ad_gray90 = "#E5E5E5",
  ad_gray70 = "#B3B3B3",
  ad_gray50 = "#808080",
  ad_gray30 = "#4D4D4D",
  ad_gray10 = "#1A1A1A",

  # For missing/NA values
  ad_na_color = "#848B90"
)

#' @rdname aiddata_colors
#' @export
aiddata_palettes <- list(
  # Default palette using primary colors
  default = c(
    aiddata_colors$ad_wren_twilight,
    aiddata_colors$ad_vine,
    aiddata_colors$ad_patina,
    aiddata_colors$ad_spirit_gold,
    aiddata_colors$ad_silver
  ),

  # Alternative palette using secondary colors
  alt = c(
    aiddata_colors$ad_college_sky,
    aiddata_colors$ad_weathered_brick,
    aiddata_colors$ad_moss,
    aiddata_colors$ad_colonial_yellow,
    aiddata_colors$ad_slate
  ),

  # Sequential palettes
  sequential_green = c(
    aiddata_colors$ad_griffin_green,
    aiddata_colors$ad_college_woods,
    aiddata_colors$ad_patina,
    aiddata_colors$ad_gray90
  ),
  sequential_gold = c(
    aiddata_colors$ad_spirit_gold,
    aiddata_colors$ad_colonial_yellow,
    aiddata_colors$ad_silver,
    aiddata_colors$ad_gray90
  ),

  # Diverging palettes
  diverging_green_gold = c(
    aiddata_colors$ad_griffin_green,
    aiddata_colors$ad_patina,
    aiddata_colors$ad_silver,
    aiddata_colors$ad_colonial_yellow,
    aiddata_colors$ad_spirit_gold
  ),
  diverging_vine_sky = c(
    aiddata_colors$ad_vine,
    aiddata_colors$ad_weathered_brick,
    aiddata_colors$ad_silver,
    aiddata_colors$ad_college_sky,
    aiddata_colors$ad_patina
  )
)

#' Create an AidData color scale
#'
#' @param palette The name of the palette to use. Options are:
#'   "default", "alt", "sequential_green", "sequential_gold",
#'   "diverging_green_gold", "diverging_vine_sky"
#' @param reverse Boolean indicating whether to reverse the palette
#' @param discrete Boolean indicating whether the scale is discrete or
#'   continuous
#' @param ... Additional arguments passed to discrete_scale() or
#'   scale_color_gradientn()
#'
#' @return A ggplot2 color scale
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_aiddata()
scale_color_aiddata <- function(palette = "default",
                                reverse = FALSE,
                                discrete = TRUE,
                                ...) {
  pal <- aiddata_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "colour",
      palette = grDevices::colorRampPalette(pal),
      na.value = aiddata_colors$ad_na_color,
      ...
    )
  } else {
    ggplot2::scale_color_gradientn(
      colours = pal,
      na.value = aiddata_colors$ad_na_color,
      ...
    )
  }
}

#' @rdname scale_color_aiddata
#' @export
scale_fill_aiddata <- function(palette = "default",
                               reverse = FALSE,
                               discrete = TRUE,
                               ...) {
  pal <- aiddata_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "fill",
      palette = grDevices::colorRampPalette(pal),
      na.value = aiddata_colors$ad_na_color,
      ...
    )
  } else {
    ggplot2::scale_fill_gradientn(
      colours = pal,
      na.value = aiddata_colors$ad_na_color,
      ...
    )
  }
}
