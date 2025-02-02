#' @importFrom utils globalVariables
utils::globalVariables("aiddata_colors")
#' @importFrom rlang .data
NULL

#' AidData Colors and Palettes
#'
#' A comprehensive color system based on William & Mary's brand guidelines.
#' The colors are organized into primary, secondary/tertiary, and neutral
#' groups. See \url{https://brand.wm.edu/index.php/university-colors/} for the
#' original brand guidelines.
#'
#' @section Colors:
#' Colors are organized into groups:
#' \itemize{
#'   \item Primary Colors:
#'     \itemize{
#'       \item Wren Twilight (\code{wren_twilight}): Deep navy blue (#00313C)
#'       \item Vine (\code{vine}): Rich burgundy (#84344E)
#'       \item Patina (\code{patina}): Vibrant teal (#00B388)
#'       \item Spirit Gold (\code{spirit_gold}): Bright gold (#F0B323)
#'       \item Silver (\code{silver}): Light gray (#D0DED4)
#'     }
#'   \item Secondary & Tertiary Colors:
#'     \itemize{
#'       \item Colonial Yellow (\code{colonial_yellow}): Muted gold (#CAB64B)
#'       \item College Sky (\code{college_sky}): Light blue (#64CCC9)
#'       \item Weathered Brick (\code{weathered_brick}): Coral red (#E56A54)
#'       \item Moss (\code{moss}): Olive green (#789D4A)
#'       \item College Woods (\code{college_woods}): Sage green (#789F90)
#'       \item Slate (\code{slate}): Medium gray (#5B6770)
#'       \item Griffin Green (\code{griffin_green}): Deep forest green (#183028)
#'     }
#'   \item Neutral Grays:
#'     \itemize{
#'       \item Gray 90-10: A range of grays from light to dark
#'       \item NA Color (\code{na_color}): Special gray for missing values
#'     }
#' }
#'
#' @section Palettes:
#' Several pre-defined palettes are available:
#' \itemize{
#'   \item Categorical Palettes:
#'     \itemize{
#'       \item \code{default}: Three primary colors (Wren Twilight, Spirit Gold,
#'                   Silver)
#'       \item \code{one_color}: Single color (Wren Twilight)
#'       \item \code{two_colors}: Two colors (Wren Twilight, Spirit Gold)
#'       \item \code{three_colors}: Same as default
#'       \item \code{four_colors}: Adds Patina to three_colors
#'       \item \code{five_colors}: Adds Vine to four_colors
#'     }
#'   \item Sequential Palettes:
#'     \itemize{
#'       \item \code{sequential_green}: Griffin Green to Gray90
#'       \item \code{sequential_gold}: Spirit Gold to Gray90
#'     }
#'   \item Diverging Palettes:
#'     \itemize{
#'       \item \code{diverging_green_gold}: Griffin Green to Spirit Gold
#'       \item \code{diverging_vine_sky}: Vine to College Sky
#'     }
#' }
#'
#' @examples
#' # Access individual colors
#' aiddata_colors$wren_twilight
#' aiddata_colors$spirit_gold
#'
#' # Use with ggplot2
#' library(ggplot2)
#'
#' # Categorical data with default palette
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_aiddata()
#'
#' # Sequential data
#' ggplot(mtcars, aes(wt, mpg, color = disp)) +
#'   geom_point() +
#'   scale_color_aiddata(palette = "sequential_green", discrete = FALSE)
#'
#' @name aiddata_colors
NULL

#' @rdname aiddata_colors
#' @export
aiddata_colors <- list(
  # Primary palette
  wren_twilight = structure("#00313C", names = "Wren Twilight"),
  vine = structure("#84344E", names = "Vine"),
  patina = structure("#00B388", names = "Patina"),
  spirit_gold = structure("#F0B323", names = "Spirit Gold"),
  silver = structure("#D0DED4", names = "Silver"),

  # Secondary & tertiary colors
  colonial_yellow = structure("#CAB64B", names = "Colonial Yellow"),
  college_sky = structure("#64CCC9", names = "College Sky"),
  weathered_brick = structure("#E56A54", names = "Weathered Brick"),
  moss = structure("#789D4A", names = "Moss"),
  college_woods = structure("#789F90", names = "College Woods"),
  slate = structure("#5B6770", names = "Slate"),
  griffin_green = structure("#183028", names = "Griffin Green"),

  # Neutral grays for accessibility
  gray90 = structure("#E5E5E5", names = "Gray 90"),
  gray70 = structure("#B3B3B3", names = "Gray 70"),
  gray50 = structure("#808080", names = "Gray 50"),
  gray30 = structure("#4D4D4D", names = "Gray 30"),
  gray10 = structure("#1A1A1A", names = "Gray 10"),

  # For missing/NA values
  na_color = structure("#848B90", names = "NA Color")
)

#' @rdname aiddata_colors
#' @export
aiddata_palettes <- list(
  # Categorical palettes
  default = c(
    aiddata_colors$wren_twilight,
    aiddata_colors$spirit_gold,
    aiddata_colors$silver
  ),

  one_color = c(
    aiddata_colors$wren_twilight
  ),

  two_colors = c(
    aiddata_colors$wren_twilight,
    aiddata_colors$spirit_gold
  ),

  three_colors = c(
    aiddata_colors$wren_twilight,
    aiddata_colors$spirit_gold,
    aiddata_colors$silver
  ),

  four_colors = c(
    aiddata_colors$wren_twilight,
    aiddata_colors$spirit_gold,
    aiddata_colors$silver,
    aiddata_colors$patina
  ),

  five_colors = c(
    aiddata_colors$wren_twilight,
    aiddata_colors$spirit_gold,
    aiddata_colors$silver,
    aiddata_colors$patina,
    aiddata_colors$vine
  ),

  # Specialized palettes
  sequential_green = c(
    aiddata_colors$griffin_green,
    aiddata_colors$college_woods,
    aiddata_colors$patina,
    aiddata_colors$gray90
  ),

  sequential_gold = c(
    aiddata_colors$spirit_gold,
    aiddata_colors$colonial_yellow,
    aiddata_colors$silver,
    aiddata_colors$gray90
  ),

  diverging_green_gold = c(
    aiddata_colors$griffin_green,
    aiddata_colors$patina,
    aiddata_colors$silver,
    aiddata_colors$colonial_yellow,
    aiddata_colors$spirit_gold
  ),

  diverging_vine_sky = c(
    aiddata_colors$vine,
    aiddata_colors$weathered_brick,
    aiddata_colors$silver,
    aiddata_colors$college_sky,
    aiddata_colors$patina
  )
)

#' Get ggplot2 version
#'
#' Internal helper function to get ggplot2 version.
#' Used for handling version-specific functionality.
#'
#' @return A package_version object representing the installed ggplot2 version
#' @keywords internal
#' @export
get_ggplot2_version <- function() {
  utils::packageVersion("ggplot2")
}

#' Create AidData color scales for ggplot2
#'
#' Functions to create color and fill scales using AidData's color palettes.
#' These functions implement William & Mary's brand guidelines in a way that
#' works well for data visualization.
#'
#' @param palette The name of the palette to use:
#'   \itemize{
#'     \item \code{"default"}, \code{"one_color"} through \code{"five_colors"}:
#'           Categorical palettes with 1-5 colors
#'     \item \code{"sequential_green"}, \code{"sequential_gold"}: For ordered
#'           data
#'     \item \code{"diverging_green_gold"}, \code{"diverging_vine_sky"}: For
#'           data with meaningful center point
#'   }
#' @param reverse Boolean indicating whether to reverse the palette order
#' @param discrete Boolean indicating whether the scale is discrete (TRUE) or
#'   continuous (FALSE)
#' @param ... Additional arguments passed to discrete_scale() or
#'   scale_color_gradientn()
#'
#' @return A ggplot2 color or fill scale
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage with default palette (3 colors)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_aiddata()
#'
#' # Using specific number of colors
#' ggplot(mtcars, aes(wt, mpg, color = factor(gear))) +
#'   geom_point() +
#'   scale_color_aiddata(palette = "two_colors")
#'
#' # Sequential palette for continuous data
#' ggplot(mtcars, aes(wt, mpg, color = disp)) +
#'   geom_point() +
#'   scale_color_aiddata(palette = "sequential_green", discrete = FALSE)
#'
#' # Fill scale with diverging palette
#' ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
#'   geom_bar() +
#'   scale_fill_aiddata(palette = "diverging_green_gold")
#'
#' @name scale_color_aiddata
#' @aliases scale_fill_aiddata
NULL

#' @rdname scale_color_aiddata
#' @export
scale_color_aiddata <- function(palette = "default",
                                reverse = FALSE,
                                discrete = TRUE,
                                ...) {
  pal <- aiddata_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  if (discrete) {
    # Pre-3.5.0 ggplot2 needs scale_name
    if (get_ggplot2_version() < "3.5.0") {
      ggplot2::discrete_scale(
        aesthetics = "colour",
        scale_name = paste0("aiddata_", palette),
        palette = grDevices::colorRampPalette(pal),
        na.value = unname(aiddata_colors$na_color),  # Unname here
        ...
      )
    } else {
      ggplot2::discrete_scale(
        aesthetics = "colour",
        palette = grDevices::colorRampPalette(pal),
        na.value = unname(aiddata_colors$na_color),  # And here
        ...
      )
    }
  } else {
    ggplot2::scale_color_gradientn(
      colours = pal,
      na.value = unname(aiddata_colors$na_color),  # And here
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
    # Pre-3.5.0 ggplot2 needs scale_name
    if (get_ggplot2_version() < "3.5.0") {
      ggplot2::discrete_scale(
        aesthetics = "fill",
        scale_name = paste0("aiddata_", palette),
        palette = grDevices::colorRampPalette(pal),
        na.value = unname(aiddata_colors$na_color),  # Unname here
        ...
      )
    } else {
      ggplot2::discrete_scale(
        aesthetics = "fill",
        palette = grDevices::colorRampPalette(pal),
        na.value = unname(aiddata_colors$na_color),  # And here
        ...
      )
    }
  } else {
    ggplot2::scale_fill_gradientn(
      colours = pal,
      na.value = unname(aiddata_colors$na_color),  # And here
      ...
    )
  }
}
