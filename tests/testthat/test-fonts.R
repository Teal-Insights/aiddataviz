

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
  # Mock empty system fonts
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

test_that("install_aiddata_fonts skips when fonts already installed", {
  skip_if_not_installed("jsonlite")

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

  expect_message(
    install_aiddata_fonts(),
    "already installed"
  )
})

test_that("install_aiddata_fonts handles missing fonts", {
  skip_if_not_installed("jsonlite")

  # Mock systemfonts to show no fonts
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = character(0),
        style = character(0),
        stringsAsFactors = FALSE
      )
    },
    reset_font_cache = function() NULL,
    .package = "systemfonts"
  )

  # Mock jsonlite to return an empty response
  local_mocked_bindings(
    fromJSON = function(...) {
      list(items = data.frame())
    },
    .package = "jsonlite"
  )

  # Use withr to set test environment
  withr::with_envvar(
    new = list("TESTTHAT" = "true"),
    {
      # Test that we get the manual installation message
      expect_message(
        install_aiddata_fonts(),
        "Please install them manually from Google Fonts"
      )
    }
  )
})
