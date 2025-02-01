
test_that("theme_aiddata returns valid ggplot2 theme", {
  # Mock systemfonts to return no fonts
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

  # Should warn about missing fonts and return a valid theme
  expect_warning(
    theme <- theme_aiddata(),
    "Recommended fonts not found"
  )
  expect_s3_class(theme, "theme")
})

test_that("theme_aiddata handles map parameter", {
  theme <- suppressWarnings(theme_aiddata(map = TRUE))
  expect_s3_class(theme, "theme")

  # Check map-specific modifications
  expect_s3_class(theme$axis.text, "element_blank")
  expect_s3_class(theme$axis.title, "element_blank")
  expect_s3_class(theme$panel.grid, "element_blank")

  # Check margins
  margin <- theme$plot.margin
  expect_s3_class(margin, "unit")
  expect_equal(length(margin), 4)  # t, r, b, l
  expect_equal(as.numeric(margin), c(0, 0, 0, 0))
})

test_that("theme_aiddata_complete returns list with correct components", {
  complete <- suppressWarnings(theme_aiddata_complete())
  expect_type(complete, "list")
  expect_length(complete, 3)

  # Check components
  expect_s3_class(complete[[1]], "theme")
  expect_s3_class(complete[[2]], "ScaleDiscrete")
  expect_s3_class(complete[[3]], "ScaleDiscrete")
})
