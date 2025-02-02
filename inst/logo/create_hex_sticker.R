library(hexSticker)
library(ggplot2)
library(aiddataviz)
library(showtext)
library(dplyr)

# Add Google fonts
# font_add_google("Roboto", "Roboto")
# showtext_auto()
font_hoist("Roboto")

# Create bar chart with proper factor levels
p <- ggplot(gcdf_yearly_flows,
            aes(x = commitment_year,
                y = commitments_bn,
                fill = factor(flow_class,
                              levels = c("OOF-like",
                                         "ODA-like",
                                         "Vague Official Finance")))) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "OOF-like" = aiddata_colors$ad_spirit_gold,
      "ODA-like" = aiddata_colors$ad_wren_twilight,
      "Vague Official Finance" = aiddata_colors$ad_silver
    )
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  )

# Create hex sticker with adjusted dimensions
sticker(
  subplot = p,
  package = "aiddataviz",
  p_size = 23,
  p_color = aiddata_colors$ad_wren_twilight,
  p_family = "Roboto Bold",
  s_x = 1,
  s_y = 0.85,  # Moved up slightly
  s_width = 1.5,  # Reduced width
  s_height = 1.1,  # Reduced height
  h_fill = "white",
  h_color = aiddata_colors$ad_wren_twilight,
  spotlight = FALSE,
  l_y = 0.4,
  url = "https://teal-insights.github.io/aiddataviz/",
  u_color = aiddata_colors$ad_gray50,
  u_size = 5,
  filename = "man/figures/logo.png",
  dpi = 500
)
