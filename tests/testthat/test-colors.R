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

test_that("scale_color_aiddata returns a ggplot2 scale", {
  scale <- scale_color_aiddata()
  expect_s3_class(scale, "ScaleDiscrete")
  expect_true("colour" %in% scale$aesthetics)
  expect_equal(scale$na.value, aiddata_colors$ad_na_color)
})

test_that("scale_fill_aiddata returns a ggplot2 scale", {
  scale <- scale_fill_aiddata()
  expect_s3_class(scale, "ScaleDiscrete")
  expect_true("fill" %in% scale$aesthetics)
  expect_equal(scale$na.value, aiddata_colors$ad_na_color)
})

test_that("color scales handle reverse parameter", {
  scale_default <- scale_color_aiddata(palette = "default", reverse = FALSE)
  scale_reversed <- scale_color_aiddata(palette = "default", reverse = TRUE)

  n_colors <- 3
  default_colors <- scale_default$palette(n_colors)
  reversed_colors <- scale_reversed$palette(n_colors)

  expect_equal(reversed_colors, rev(default_colors))
})
