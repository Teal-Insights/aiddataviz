# tests/testthat/test-fonts.R

test_that("get_available_font returns appropriate fallbacks", {
  # Mock system fonts to only have Arial
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = "Arial",
        style = "Regular",
        stringsAsFactors = FALSE
      )
    },
    .package = "systemfonts"
  )

  # Should return Arial when Roboto isn't available
  expect_equal(
    get_available_font("Roboto", fallbacks = c("Arial", "Helvetica")),
    "Arial"
  )

  # Mock system fonts to have no fonts
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = character(0),
        style = character(0),
        stringsAsFactors = FALSE
      )
    },
    .package = "systemfonts"
  )

  # Should return "sans" when no fonts are available
  expect_equal(
    get_available_font("Roboto", fallbacks = c("Arial", "Helvetica")),
    "sans"
  )
})

test_that("get_available_font returns requested font when available", {
  # Mock system fonts to include Roboto
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = c("Roboto", "Arial"),
        style = c("Regular", "Regular"),
        stringsAsFactors = FALSE
      )
    },
    .package = "systemfonts"
  )

  expect_equal(
    get_available_font("Roboto", fallbacks = c("Arial", "Helvetica")),
    "Roboto"
  )
})
test_that("font_hoist works for available fonts", {
  # Mock systemfonts::system_fonts() to return Roboto
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = "Roboto",
        style = "Regular",
        path = "/path/to/Roboto-Regular.ttf",
        stringsAsFactors = FALSE
      )
    },
    register_font = function(...) TRUE,
    .package = "systemfonts"
  )

  result <- font_hoist("Roboto", check_only = TRUE)
  expect_true(result$available)
  expect_equal(nrow(result$specs), 1)
})

test_that("font_hoist handles missing fonts", {
  # Mock systemfonts::system_fonts() to return no fonts
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = character(0),
        style = character(0),
        path = character(0),
        stringsAsFactors = FALSE
      )
    },
    .package = "systemfonts"
  )

  expect_message(
    result <- font_hoist("NonexistentFont"),
    "No fonts found"
  )
  expect_false(result$available)
})

test_that(".check_fonts detects missing required fonts", {
  # Mock systemfonts::system_fonts() to return only Roboto
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = "Roboto",
        style = "Regular",
        stringsAsFactors = FALSE
      )
    },
    .package = "systemfonts"
  )

  expect_false(.check_fonts())
})

test_that(".check_fonts detects all required fonts", {
  # Mock systemfonts::system_fonts() to return both required fonts
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = c("Roboto", "Open Sans"),
        style = c("Regular", "Regular"),
        stringsAsFactors = FALSE
      )
    },
    .package = "systemfonts"
  )

  expect_true(.check_fonts())
})
