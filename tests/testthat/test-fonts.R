
# Test for main utility functions
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

# Test Google Fonts API interaction
test_that("fetch_google_font_info handles API responses correctly", {
  skip_if_not_installed("jsonlite")

  # Mock successful API response
  mock_response <- list(
    items = data.frame(
      family = "Roboto",
      files = I(list(
        regular = "https://fonts.gstatic.com/s/roboto/regular.ttf"
      ))
    )
  )

  local_mocked_bindings(
    fromJSON = function(...) mock_response,
    .package = "jsonlite"
  )

  result <- fetch_google_font_info("Roboto")
  expect_false(is.null(result))
  expect_equal(result$family, "Roboto")

  # Test API error handling
  local_mocked_bindings(
    fromJSON = function(...) stop("API Error"),
    .package = "jsonlite"
  )

  expect_null(fetch_google_font_info("Roboto"))
})

# Test font variant downloading
test_that("download_font_variant handles download correctly", {
  # Create a temporary file that exists
  temp_file <- tempfile(fileext = ".txt")
  writeLines("test", temp_file)

  # Mock successful download
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      file.copy(temp_file, destfile)
      0L
    },
    .package = "utils"
  )

  expect_true(
    download_font_variant(
      url = "https://example.com/font.ttf",
      dest_path = tempfile(fileext = ".ttf")
    )
  )

  # Mock failed download
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      1L
    },
    .package = "utils"
  )

  expect_false(
    download_font_variant(
      url = "https://invalid.url/font.ttf",
      dest_path = tempfile()
    )
  )

  unlink(temp_file)
})

# Test font directory detection
test_that("get_font_directory returns correct path", {
  # Test Windows path
  local_mocked_bindings(
    Sys.info = function() c(sysname = "Windows"),
    Sys.getenv = function(x, ...) {
      if (x == "LOCALAPPDATA") return("C:/Users/test/AppData/Local")
      ""
    },
    .package = "base"  # Specify base package for these functions
  )

  expect_match(
    get_font_directory(),
    "Windows/Fonts$"
  )

  # Test macOS path
  local_mocked_bindings(
    Sys.info = function() c(sysname = "Darwin"),
    .package = "base"  # Specify base package for Sys.info
  )

  expect_match(
    get_font_directory(),
    "Library/Fonts$"
  )
})

# Test single font installation
test_that("install_single_font handles various scenarios", {
  skip_if_not_installed("jsonlite")

  # Mock successful installation
  local_mocked_bindings(
    system_fonts = function() {
      data.frame(family = character(0))
    },
    .package = "systemfonts"
  )

  local_mocked_bindings(
    fetch_google_font_info = function(...) {
      list(files = list(regular = "https://example.com/font.ttf"))
    },
    download_font_variants = function(...) tempfile(),
    install_font_files = function(...) TRUE,
    .env = asNamespace("aiddataviz")
  )

  result <- install_single_font(
    "Roboto",
    tempdir(),
    force_reinstall = FALSE
  )
  expect_true(result$success)

  # Mock missing font
  local_mocked_bindings(
    fetch_google_font_info = function(...) NULL,
    .env = asNamespace("aiddataviz")
  )

  result <- install_single_font(
    "NonexistentFont",
    tempdir(),
    force_reinstall = FALSE
  )
  expect_false(result$success)
  expect_match(result$message, "not found in Google Fonts")
})

# Test message formatting
test_that("format_installation_message returns correct messages", {
  success_msg <- format_installation_message(TRUE)
  expect_match(success_msg, "successfully")
  expect_match(success_msg, "restart R")

  failure_msg <- format_installation_message(FALSE)
  expect_match(failure_msg, "manually")
  expect_match(failure_msg, "Google Fonts")
})

# Test main installation function
test_that("install_aiddata_fonts coordinates installation process", {
  skip_if_not_installed("jsonlite")

  # Mock successful installation
  local_mocked_bindings(
    .check_fonts = function() FALSE,
    get_font_directory = function() tempdir(),
    install_single_font = function(...) list(success = TRUE),
    format_installation_message = function(...) "Success message",
    .env = asNamespace("aiddataviz")
  )

  expect_message(
    result <- install_aiddata_fonts(),
    "Success message"
  )
  expect_true(result)

  # Mock partial failure
  local_mocked_bindings(
    install_single_font = function(font_name, ...) {
      list(
        success = font_name == "Roboto",
        message = if (font_name != "Roboto") "Failed to install"
      )
    },
    .env = asNamespace("aiddataviz")
  )

  expect_message(
    result <- install_aiddata_fonts(),
    "Failed to install"
  )
  expect_false(result)
})

# Test for already installed fonts
test_that("install_aiddata_fonts skips when fonts present", {
  local_mocked_bindings(
    .check_fonts = function() TRUE,
    .env = asNamespace("aiddataviz")
  )

  expect_message(
    install_aiddata_fonts(),
    "already installed"
  )
})
