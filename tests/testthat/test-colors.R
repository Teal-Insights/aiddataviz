test_that("aiddata_colors contains correct hex codes", {
  expect_equal(aiddata_colors$ad_wren_twilight, "#00313C")
  expect_equal(aiddata_colors$ad_vine, "#84344E")
  expect_equal(aiddata_colors$ad_patina, "#00B388")
  expect_equal(aiddata_colors$ad_spirit_gold, "#F0B323")
  expect_equal(aiddata_colors$ad_silver, "#D0DED4")
})

test_that("aiddata_palettes contain the correct colors", {
  # Test default palette
  expect_equal(
    aiddata_palettes$default,
    c(
      aiddata_colors$ad_wren_twilight,
      aiddata_colors$ad_vine,
      aiddata_colors$ad_patina,
      aiddata_colors$ad_spirit_gold,
      aiddata_colors$ad_silver
    )
  )

  # Test sequential palette
  expect_equal(
    aiddata_palettes$sequential_green,
    c(
      aiddata_colors$ad_griffin_green,
      aiddata_colors$ad_college_woods,
      aiddata_colors$ad_patina,
      aiddata_colors$ad_gray90
    )
  )
})

test_that("scale_color_aiddata returns valid ggplot2 scale", {
  scale <- scale_color_aiddata("default")
  expect_s3_class(scale, "ScaleDiscrete")
  expect_true("colour" %in% scale$aesthetics)
  expect_equal(scale$na.value, aiddata_colors$ad_na_color)

  # Test continuous scale
  cont_scale <- scale_color_aiddata("default", discrete = FALSE)
  expect_s3_class(cont_scale, "ScaleContinuous")
})

test_that("scale_fill_aiddata returns valid ggplot2 scale", {
  scale <- scale_fill_aiddata("default")
  expect_s3_class(scale, "ScaleDiscrete")
  expect_true("fill" %in% scale$aesthetics)
  expect_equal(scale$na.value, aiddata_colors$ad_na_color)

  # Test continuous scale
  cont_scale <- scale_fill_aiddata("default", discrete = FALSE)
  expect_s3_class(cont_scale, "ScaleContinuous")
})

test_that("color scales handle reverse parameter", {
  # Helper function to extract colors from scale
  get_scale_colors <- function(scale, n = 3) {
    if (inherits(scale, "ScaleDiscrete")) {
      scale$palette(n)
    } else {
      scale$palette(seq(0, 1, length.out = n))
    }
  }

  # Test discrete scales
  scale_default <- scale_color_aiddata("default", reverse = FALSE)
  scale_reversed <- scale_color_aiddata("default", reverse = TRUE)

  default_colors <- get_scale_colors(scale_default)
  reversed_colors <- get_scale_colors(scale_reversed)
  expect_equal(reversed_colors, rev(default_colors))

  # Test continuous scales
  scale_cont_default <- scale_color_aiddata("default",
    reverse = FALSE,
    discrete = FALSE
  )
  scale_cont_reversed <- scale_color_aiddata("default",
    reverse = TRUE,
    discrete = FALSE
  )

  cont_default_colors <- get_scale_colors(scale_cont_default)
  cont_reversed_colors <- get_scale_colors(scale_cont_reversed)
  expect_equal(cont_reversed_colors, rev(cont_default_colors))
})

test_that("scales work with different ggplot2 versions", {
  # Skip this test on older R versions that don't have local_mocked_bindings
  if (!exists("local_mocked_bindings", asNamespace("testthat"))) {
    skip("local_mocked_bindings not available")
  }

  # Test with old version
  withr::local_envvar(old = Sys.getenv("GGPLOT2_VERSION"))
  testthat::local_mocked_bindings(
    get_ggplot2_version = function() package_version("3.4.9"),
    .env = environment(scale_color_aiddata)
  )

  # Suppress warning about deprecated scale_name since it's expected
  suppressWarnings({
    old_scale <- scale_color_aiddata("default")
  })
  expect_s3_class(old_scale, "ScaleDiscrete")
  expect_equal(old_scale$aesthetics, "colour")

  # Test with new version
  testthat::local_mocked_bindings(
    get_ggplot2_version = function() package_version("3.5.0"),
    .env = environment(scale_color_aiddata)
  )
  new_scale <- scale_color_aiddata("default")
  expect_s3_class(new_scale, "ScaleDiscrete")
  expect_equal(new_scale$aesthetics, "colour")
})
