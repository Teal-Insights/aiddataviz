# tests/testthat/test-fonts.R

test_that("get_available_font returns appropriate fallback", {
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = c("Arial", "Times New Roman"),
        style = c("Regular", "Regular"),
        stringsAsFactors = FALSE
      )
    },
    .package = "systemfonts"
  )

  expect_equal(
    get_available_font("Open Sans", fallbacks = c("Arial", "Helvetica")),
    "Arial"
  )

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

  expect_equal(
    get_available_font("Open Sans", fallbacks = c("Arial", "Helvetica")),
    "sans"
  )
})

test_that("install_aiddata_fonts handles non-interactive mode", {
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = "Arial",  # Missing both required fonts
        style = "Regular",
        stringsAsFactors = FALSE
      )
    },
    .package = "systemfonts"
  )

  # Force non-interactive mode
  withr::local_options(list(interactive = FALSE))

  # Capture and test output
  expect_message(
    result <- install_aiddata_fonts(),
    "manually install"
  )
  expect_false(result)
})

test_that("install_aiddata_fonts handles interactive install", {
  skip_on_ci()

  temp_dir <- tempdir()
  test_zip <- file.path(temp_dir, "Roboto.zip")

  # Mock system fonts
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(
        family = "Arial",  # Missing both required fonts
        style = "Regular",
        stringsAsFactors = FALSE
      )
    },
    register_font = function(...) TRUE,
    .package = "systemfonts"
  )

  # Mock download functions
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      expect_match(url, "fonts.google.com")
      file.create(destfile)
      0
    },
    unzip = function(zipfile, ...) {
      ttf_path <- file.path(temp_dir, "Roboto-Regular.ttf")
      file.create(ttf_path)
      ttf_path
    },
    .package = "utils"
  )

  # Force interactive mode and mock readline
  withr::local_options(list(interactive = TRUE))

  # Use mockery to mock base::readline
  mockery::stub(install_aiddata_fonts, "readline", function(...) "y")

  expect_message(
    result <- install_aiddata_fonts(),
    "Installing fonts"
  )
  expect_true(result)

  unlink(test_zip)
})
