# Test ti_colors hex codes
test_that("aiddata_colors contains correct hex codes", {
  # Just test the hex values, not the named structure
  expect_equal(unname(aiddata_colors$wren_twilight), "#00313C")
  expect_equal(unname(aiddata_colors$vine), "#84344E")
  expect_equal(unname(aiddata_colors$patina), "#00B388")
  expect_equal(unname(aiddata_colors$spirit_gold), "#F0B323")
  expect_equal(unname(aiddata_colors$silver), "#D0DED4")
})

test_that("aiddata_palettes contain the correct colors", {
  # Test default palette
  expect_equal(
    unname(aiddata_palettes$default),
    c("#00313C", "#F0B323", "#D0DED4")
  )

  # Test sequential palette
  expect_equal(
    unname(aiddata_palettes$sequential_green),
    c("#183028", "#789F90", "#00B388", "#E5E5E5")
  )

})

test_that("scale_color_aiddata returns valid ggplot2 scale", {
  scale <- scale_color_aiddata("default")
  expect_s3_class(scale, "ScaleDiscrete")
  expect_true("colour" %in% scale$aesthetics)
  expect_equal(scale$na.value, unname(aiddata_colors$na_color))

  # Test continuous scale
  cont_scale <- scale_color_aiddata("default", discrete = FALSE)
  expect_s3_class(cont_scale, "ScaleContinuous")
})

test_that("scale_fill_aiddata returns valid ggplot2 scale", {
  scale <- scale_fill_aiddata("default")
  expect_s3_class(scale, "ScaleDiscrete")
  expect_true("fill" %in% scale$aesthetics)
  expect_equal(scale$na.value, unname(aiddata_colors$na_color))

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
  scale_cont_default <- scale_color_aiddata(
    "default",
    reverse = FALSE,
    discrete = FALSE
  )
  scale_cont_reversed <- scale_color_aiddata(
    "default",
    reverse = TRUE,
    discrete = FALSE
  )

  cont_default_colors <- get_scale_colors(scale_cont_default)
  cont_reversed_colors <- get_scale_colors(scale_cont_reversed)
  expect_equal(cont_reversed_colors, rev(cont_default_colors))
})
