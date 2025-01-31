# R/fonts.R

#' Make font families available in ggplot2
#'
#' Sometimes, even when fonts are loaded on a computer, they are not
#' immediately available for use in R. This function fixes that.
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
font_hoist <- function(family_name, silent = FALSE, check_only = FALSE) {

  # Fetch all system fonts using the systemfonts package
  font_specs <- systemfonts::system_fonts() |>
    dplyr::filter(family == family_name) |>
    dplyr::mutate(family = paste(family, style)) |>
    dplyr::select(plain = path, name = family)

  # Check if any fonts were found for the given family name
  if (nrow(font_specs) == 0) {
    if (!silent) {
      message(paste0(
        "No fonts found for the family '", family_name, "'. \n",
        "Make sure that the font is downloaded to your computer."
      ))
    }
    return(list(specs = NULL, available = FALSE))
  }

  if (check_only) {
    return(list(specs = font_specs, available = TRUE))
  }

  # Define a function to safely register a font
  safe_register_font <- function(plain, name) {
    tryCatch({
      systemfonts::register_font(plain = plain, name = name)
      TRUE
    }, error = function(e) {
      FALSE
    })
  }

  # Register each font
  results <- purrr::pwalk(as.list(font_specs), safe_register_font)

  # Return font specifications invisibly
  invisible(font_specs)
}

#' Check if required fonts are installed
#' @keywords internal
.check_fonts <- function() {
  required_fonts <- c("Roboto", "Open Sans")
  all(required_fonts %in% unique(systemfonts::system_fonts()$family))
}

#' Package startup message
#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (!.check_fonts()) {
    packageStartupMessage(
      "Note: The AidData theme requires the Roboto and Open Sans fonts.\n",
      "Install them from Google Fonts: ",
      "https://fonts.google.com/specimen/Roboto ",
      "https://fonts.google.com/specimen/Open+Sans"
    )
  }
}
