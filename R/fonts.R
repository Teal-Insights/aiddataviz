# R/fonts.R


#' @importFrom ragg agg_png
#' @importFrom dplyr filter mutate select
#' @importFrom rlang .data
NULL

# At the top of fonts.R

utils::globalVariables(c("family", "style", "path"))

#' Ensure fonts are properly registered
#'
#' This is used internally to track and register available fonts.
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c("family", "style", "path"))
}


#' Make font families available in ggplot2
#'
#' @description
#' Sometimes, even when fonts are loaded on a computer, they are not
#' immediately available for use in R. This function fixes that.
#'
#' @details
#' See [June Choe's blog post explaining this](
#' https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/
#' ).
#' This function requires the user to have already downloaded the
#' font on their computer, and to have set up `ragg` as described
#' in the blog post.
#'
#' @param family_name name of the font family.
#' @param silent do you want to suppress the message?
#' @param check_only logical. If TRUE, only checks if fonts are available
#'   without attempting to register them.
#'
#' @return The function returns a message indicating whether the font
#'   was successfully loaded or not.
#' @export
#'
#' @examples
#' # load Roboto (must be loaded from Google Fonts on system already)
#' font_hoist("Roboto")
#'
#' # Try loading a font not on your computer, or that doesn't exist.
#' # It will give an informative error message.
#' font_hoist("Lobster Monster")
font_hoist <- function(family_name, silent = FALSE, check_only = FALSE) {
  # Remove ANSI color codes or make them conditional
  # Only use colors when in an interactive terminal
  use_colors <- interactive()

  # Define color codes conditionally
  green <- if (use_colors) "\033[32m" else ""
  red <- if (use_colors) "\033[31m" else ""
  reset <- if (use_colors) "\033[0m" else ""

  # Initialize vectors to track fonts that were and were not
  # successfully registered
  successful_fonts <- c()
  failed_fonts <- c()
  already_loaded_fonts <- c()

  # Step 1: Fetch all system fonts using the systemfonts package
  font_specs <- systemfonts::system_fonts() |>
    dplyr::filter(family == family_name) |>
    dplyr::mutate(family = paste(family, style)) |>
    dplyr::select(plain = path, name = family)

  # Step 2: Check if any fonts were found for the given family name
  if (nrow(font_specs) == 0) {
    if (!silent) {
      message(paste0(
        red,
        "No fonts found for the family '", family_name, "'. \n",
        "Make sure that the font is downloaded to your computer, ",
        "and that you can find it using systemfonts::system_fonts().",
        reset
      ))
    }
    return(list(specs = NULL, available = FALSE))
  }

  if (check_only) {
    return(list(specs = font_specs, available = TRUE))
  }

  # Step 3: Define a function to safely register a font
  safe_register_font <- function(plain, name) {
    tryCatch({
      systemfonts::register_font(plain = plain, name = name)
      successful_fonts <<- c(successful_fonts, name)
    }, error = function(e) {
      failed_fonts <<- c(failed_fonts, name)
    })
  }

  # Step 4: Register each font
  purrr::pwalk(as.list(font_specs), safe_register_font)

  # Step 5: Display a summary message
  if (length(successful_fonts) > 0) {
    message(paste0(
      green,
      "Successfully hoisted ", length(successful_fonts),
      " font(s) for the family '", family_name, "': ",
      paste(successful_fonts, collapse = ", "),
      reset
    ))
  }

  if (length(already_loaded_fonts) > 0) {
    message(paste0(
      green,
      "The following font(s) for the family '",
      family_name,
      "' are already loaded: ",
      paste(already_loaded_fonts, collapse = ", "),
      reset
    ))
  }

  if (length(failed_fonts) > 0) {
    message(paste0(
      red,
      "Failed to hoist ", length(failed_fonts), " font(s) for the family '",
      family_name, "': ",
      paste(failed_fonts, collapse = ", "),
      reset
    ))
  }

  invisible(font_specs)
}

#' Check if required fonts are installed
#' @keywords internal
.check_fonts <- function() {
  required_fonts <- c("Roboto", "Open Sans")
  all(required_fonts %in% unique(systemfonts::system_fonts()$family))
}


#' Find an available font from a list of options
#'
#' @param font_family Primary font family to use
#' @param fallbacks Vector of fallback font families
#' @return Name of first available font, or "sans" if none found
#' @export
get_available_font <- function(font_family, fallbacks = c("Arial", "Helvetica", "sans")) {
  # Get all system fonts
  available_fonts <- unique(systemfonts::system_fonts()$family)

  # First try the desired font
  if (font_family %in% available_fonts) {
    return(font_family)
  }

  # Then try fallbacks in order
  for (font in fallbacks) {
    if (font %in% available_fonts) {
      return(font)
    }
  }

  # If nothing else works, return "sans"
  "sans"
}



#' Package startup message
#'
#' Checks for required fonts and displays message if they're missing.
#'
#' @param libname The library where the package is installed
#' @param pkgname The package name
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Skip if being loaded by devtools/pkgload
  if ("pkgload" %in% loadedNamespaces()) {
    return()
  }

  # Check fonts in interactive sessions
  if (interactive() && !.check_fonts()) {
    packageStartupMessage(
      "Note: AidData themes require specific fonts:\n",
      "- Roboto (https://fonts.google.com/specimen/Roboto)\n",
      "- Open Sans (https://fonts.google.com/specimen/Open+Sans)\n\n",
      "Please install these fonts from Google Fonts."
    )
  }
}

#' Get font family with fallbacks
#'
#' @param font_family The primary font family to use
#' @param fallbacks Character vector of fallback fonts
#' @return A font family name that is available on the system
get_font_family <- function(font_family,
                            fallbacks = c("Arial", "Helvetica", "sans-serif", "sans")) {
  available_fonts <- unique(systemfonts::system_fonts()$family)

  # First try requested font
  if (font_family %in% available_fonts) {
    return(font_family)
  }

  # Then try each fallback in order
  for (fallback in fallbacks) {
    if (fallback %in% available_fonts) {
      return(fallback)
    }
  }

  # Last resort - "sans"
  "sans"
}
