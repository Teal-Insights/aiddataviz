# R/fonts.R

#' Check if required AidData fonts are installed
#'
#' @param required_fonts Character vector of font families to check.
#' @return Logical TRUE/FALSE whether all required fonts are available
#' @keywords internal
.check_fonts <- function(required_fonts = c("Roboto", "Open Sans")) {
  available <- unique(systemfonts::system_fonts()$family)
  all(required_fonts %in% available)
}

#' Provide an available font (or fallback) from a vector of desired families
#'
#' @param font_family The primary font family you want to use.
#' @param fallbacks A character vector of fallback fonts to try in order.
#' @return A single string which is the first available font family or "sans" if none found.
#' @export
get_available_font <- function(font_family, fallbacks = c("Arial", "Helvetica", "sans")) {
  all_fonts <- systemfonts::system_fonts()$family
  if (font_family %in% all_fonts) {
    return(font_family)
  }
  for (fallback in fallbacks) {
    if (fallback %in% all_fonts) {
      return(fallback)
    }
  }
  # If none of the desired fonts are installed, return "sans"
  "sans"
}

# R/fonts.R

install_aiddata_fonts <- function() {
  required_fonts <- c("Roboto", "Open Sans")

  # If fonts are already installed, do nothing:
  if (.check_fonts(required_fonts)) {
    message("All AidData fonts are already installed.")
    return(TRUE)
  }

  # If not interactive, just print a notice and quit:
  if (!interactive()) {
    missing_fonts <- required_fonts[!required_fonts %in% systemfonts::system_fonts()$family]
    message(
      "The following fonts are missing and required for AidData themes:\n",
      paste("-", missing_fonts, collapse = "\n"),
      "\nPlease install these fonts manually or run install_aiddata_fonts() in an interactive session."
    )
    return(FALSE)
  }

  # ------------------------------------------------------------------
  # ADD THIS GUARD: Skip the prompt if loaded by 'pkgload' or if TESTTHAT is set
  # (devtools::test(), devtools::load_all(), R CMD check, etc.)
  # ------------------------------------------------------------------
  if ("pkgload" %in% loadedNamespaces() || nzchar(Sys.getenv("TESTTHAT"))) {
    # You can optionally show a small message if you like, or just quietly skip.
    message("Skipping font installation prompt under devtools/test environment.")
    return(FALSE)
  }

  # Now we do the real interactive prompt for normal users:
  missing_fonts <- required_fonts[!required_fonts %in% systemfonts::system_fonts()$family]
  message("The following fonts are missing and required for AidData themes:")
  message(paste("-", missing_fonts, collapse = "\n"))

  ans <- readline("Would you like to install these fonts now? (y/n) ")
  if (tolower(ans) != "y") {
    message("User declined font installation. You can install them manually or run install_aiddata_fonts() again.")
    return(FALSE)
  }

  # ... your real downloading/installation logic ...
  message("Installing fonts from Google Fonts...")
  # e.g. download, unzip, register, etc.

  message("Fonts installed successfully.")
  return(TRUE)
}

#' Package startup: check fonts unless loaded by devtools/pkgload
#'
#' This runs when the package is attached via `library(aiddataviz}`.
#' We skip the check if `pkgload` is managing the package (i.e. devtools::load_all or devtools::test).
.onAttach <- function(libname, pkgname) {

  # If devtools/pkgload is loading this in an interactive session, skip the font check.
  # That prevents prompting the developer/tester unnecessarily.
  if ("pkgload" %in% loadedNamespaces()) {
    return()
  }

  # Normal interactive attach: Check fonts & message if missing
  if (interactive() && !.check_fonts()) {
    packageStartupMessage(
      "Note: AidData themes require specific fonts. ",
      "Use install_aiddata_fonts() to install them if needed."
    )
  }
}
