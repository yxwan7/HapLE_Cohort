# ==============================================================================
# TITLE:     Lexis diagram
# DATE:      2025-04-28
# UPDATE:    2025-07-05
# ==============================================================================

# Load necessary package
library(ggplot2)

#-------------------------------
# 1. Create polygon data for background fill regions
#-------------------------------

# "Blue" region (using light blue: "#bcd6ed") for the area between the 1931 and 1937 cohort life lines
blue_poly <- data.frame(
  x = c(2000, 2000, 1931 + 79, 1937 + 79),
  y = c(2000 - 1937, 2000 - 1931, 79, 79),
  group = "Earlier cohort"
)
# Calculated y values: (63, 69, 79, 79)

# "Green" region (using light green: "#c4deb1") for the area between the 1941 and 1947 cohort life lines
green_poly <- data.frame(
  x = c(1941 + 63, 1947 + 63, 2020, 2020),
  y = c(63, 63, 2020 - 1947, 2020 - 1941),
  group = "Later cohort"
)
# Calculated y values: (63, 63, 73, 79)

poly_df <- rbind(blue_poly, green_poly)

# #---------------------------------------------
# # 2. Create data for individual cohort life line segments (REMOVED)
# #---------------------------------------------

# # (a) Life lines for each year (full lines, automatically clipped)
# # For cohorts 1931 to 1937 (light blue)
# early_segments <- data.frame(
#   xstart = 1931:1937,
#   ystart = 0,
#   xend   = (1931:1937) + 100,
#   yend   = 100
# )

# # For cohorts 1941 to 1947 (light green)
# late_segments <- data.frame(
#   xstart = 1941:1947,
#   ystart = 0,
#   xend   = (1941:1947) + 100,
#   yend   = 100
# )

# # (b) Main cohort life lines with emphasized styling
# main_lines <- data.frame(
#   cohort = c(1931, 1937, 1947),
#   xstart = c(1931, 1937, 1947),
#   ystart = c(0, 0, 0),
#   xend   = c(1931 + 100, 1937 + 100, 1947 + 100),
#   yend   = c(100, 100, 100),
#   col    = c("#bcd6ed", "#536d75", "#c4deb1") # early: light blue, mid: blue-green mix, late: light green
# )

#-------------------------------------------------
# 3. Create data for emphasized polygon regions
#-------------------------------------------------

# First emphasized region (for early cohort):
# Defined by vertices:
# A = (2002, 71), B = (2002, 65), C = (2008, 71), D = (2008, 77)
emph1 <- data.frame(
  x = c(2002, 2002, 2008, 2008),
  y = c(71, 65, 71, 77)
)

# Second emphasized region (for later cohort):
# Defined by vertices:
# A = (2012, 71), B = (2012, 65), C = (2018, 71), D = (2018, 77)
emph2 <- data.frame(
  x = c(2012, 2012, 2018, 2018),
  y = c(71, 65, 71, 77)
)

#-------------------------------
# 4. Plot using ggplot2
#-------------------------------
p <- ggplot() +
  # (a) Add background fill polygons with transparency (alpha = 0.3)
  geom_polygon(
    data = poly_df,
    aes(x = x, y = y, group = group, fill = group),
    color = NA, alpha = 0.5
  ) +

  # # (b) Add individual cohort life lines (thin lines) - REMOVED
  # geom_segment(
  #   data = early_segments,
  #   aes(x = xstart, y = ystart, xend = xend, yend = yend),
  #   color = "#bcd6ed", size = 0.5, show.legend = FALSE
  # ) +
  # geom_segment(
  #   data = late_segments,
  #   aes(x = xstart, y = ystart, xend = xend, yend = yend),
  #   color = "#c4deb1", size = 0.5, show.legend = FALSE
  # ) +

  # # (c) Add main cohort life lines (thicker lines) - REMOVED
  # geom_segment(
  #   data = main_lines,
  #   aes(x = xstart, y = ystart, xend = xend, yend = yend, color = col),
  #   size = 1, show.legend = FALSE
  # ) +
  # scale_color_identity() +

  # (d) Add thick dashed gray reference lines
  geom_hline(yintercept = 65, color = "gray", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 68, color = "gray", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 71, color = "gray", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 74, color = "gray", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 77, color = "gray", linetype = "dashed", size = 0.5) +
  geom_vline(
    xintercept = c(2002, 2005, 2008, 2012, 2014, 2018),
    color = "gray", linetype = "dashed", size = 0.5
  ) +

  # (e) Add emphasized regions (polygon outlines with dashed lines)
  geom_polygon(
    data = emph1, aes(x = x, y = y),
    fill = NA, color = "black", linetype = "dashed", size = 1.0
  ) +
  geom_polygon(
    data = emph2, aes(x = x, y = y),
    fill = NA, color = "black", linetype = "dashed", size = 1.0
  ) +

  # Add solid lines within emphasized regions
  geom_segment(
    aes(x = 2002, y = 68, xend = 2008, yend = 74),
    color = "black", linetype = "solid", size = 1.0
  ) +
  geom_segment(
    aes(x = 2012, y = 68, xend = 2018, yend = 74),
    color = "black", linetype = "solid", size = 1.0
  ) +

  # Add solid points at line endpoints
  geom_point(
    data = data.frame(
      x = c(2002, 2008, 2012, 2018),
      y = c(68, 74, 68, 74)
    ),
    aes(x = x, y = y),
    color = "black", size = 3, shape = 16
  ) +

  # (f) Set coordinate limits, axis breaks, and plot title
  coord_cartesian(xlim = c(2002, 2018), ylim = c(65, 77)) +
  scale_x_continuous(
    breaks = c(2002, 2005, 2008, 2012, 2014, 2018),
    labels = c("2002", "2005", "2008", "2012", "2014", "2018")
  ) +
  scale_y_continuous(breaks = c(65, 68, 71, 74, 77)) +
  labs(x = "Period", y = "Age") +

  # (g) Define fill colors for background polygons and add legend
  scale_fill_manual(
    name = NULL,
    values = c(
      "Earlier cohort" = "#87CEEB",
      "Later cohort" = "#a8e6cf"
    )
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +

  # (h) Use a grid theme and position the legend at the bottom right
  theme_bw() +
  theme(
    panel.grid = element_blank(), # Remove all default grid lines
    legend.position = c(0.99, 0.01),
    legend.justification = c(1, 0)
  )
#-------------------------------
# 5. Save Plot
#-------------------------------

ggsave("R output/2_1_Lexis_Diagram.png",
  plot = p,
  width = 6, height = 5, dpi = 300
)
