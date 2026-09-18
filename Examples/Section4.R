library(dplyr)
library(ggplot2)
library(NotchedBoxplot)

# ==============================================================================
# PART 1: Normal Distribution Data
# ==============================================================================
# 1. Simulate data
# ==============================================================================
library(ggplot2)
library(patchwork)

set.seed(2018)
N1_size <- 50
N2_size <- 100

# Normal baseline data: Group A ~ N(0, 1), Group B ~ N(0.5, 1)
sim_data_norm <- rbind(
  data.frame(Group = "Group A", Value = rnorm(N1_size, 0, 1)),
  data.frame(Group = "Group B", Value = rnorm(N2_size, 0.5, 1))
)

y_breaks_norm <- seq(-3, 4, by = 1)
y_limits_norm <- c(-2, 3.5)
x_limits <- c(0.4, 2.6)

# ==============================================================================
# 2. Plotting
# ==============================================================================
jcgs_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "grey92", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.8),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5, color = "grey20"),
    plot.margin = margin(10, 5, 5, 5),
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 14, color = "black"),
    panel.spacing = unit(0, "lines")
  )
# ==============================================================================

# Display only median notches (facing each other)
p_new_2 <- notched_boxplot(sim_data_norm, "Group", "Value",
                           show_mean_ci = FALSE,
                           show_med_ci = TRUE ,
                           width = 0.5,
                           indent_pct = 0.125,
                           med_side = c("right", "left")
) +
  scale_y_continuous(breaks = y_breaks_norm) +
  coord_cartesian(ylim = y_limits_norm, xlim = x_limits) +
  labs(title = "(b) Median-notched boxplot", x = NULL, y = NULL) +
  jcgs_theme +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) # Hide Y-axis

# Display only mean notches (facing each other)
p_new_1 <- notched_boxplot(sim_data_norm, "Group", "Value",
                           show_mean_ci = TRUE,
                           show_med_ci = FALSE,
                           width = 0.5,
                           indent_pct = 0.125,
                           mean_side = c("right", "left")
) +
  scale_y_continuous(breaks = y_breaks_norm) +
  coord_cartesian(ylim = y_limits_norm, xlim = x_limits) +
  labs(title = "(c) Mean-notched boxplot", x = NULL, y = NULL) +
  jcgs_theme +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) # Hide Y-axis

# Display both mean and median notches simultaneously
p_new <- notched_boxplot(sim_data_norm, "Group", "Value",
                         show_mean_ci = TRUE,
                         show_med_ci = TRUE,
                         width = 0.5,
                         indent_pct = 0.125,
                         mean_side = "left",
                         med_side="right"
) +
  scale_y_continuous(breaks = y_breaks_norm) +
  coord_cartesian(ylim = y_limits_norm, xlim = x_limits) +
  labs(title = "(a) Dual-notched boxplot", x = NULL, y = NULL) +
  jcgs_theme

# ==========================================
# 4. Combine panels
# ==========================================
# Arrange panels: (a) Dual -> (b) Median -> (c) Mean
final_panel <- p_new | p_new_2 | p_new_1

print(final_panel)
ggsave("new_notch.pdf", final_panel, width = 9.5, height = 6.5, device = "pdf")

library(dplyr)
library(ggplot2)
library(patchwork)
library(NotchedBoxplot)

# diamonds
# =====================================================================
# 1. Direct Data Preparation
# =====================================================================
# Extract the "Fair_D" and "Fair_H" groups from the diamonds dataset
plot_data <- diamonds %>%
  mutate(Condition_Group = paste(cut, color, sep = "_")) %>%
  filter(Condition_Group %in% c("Fair_D", "Fair_H")) %>%
  mutate(Condition_Group = factor(Condition_Group, levels = c("Fair_D", "Fair_H")))

# =====================================================================
# 2. Global Theme Definition
# =====================================================================
jcgs_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "grey92", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.8),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12, margin = margin(t = 5)),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 14,  hjust = 0.5, color = "grey20"),
    plot.margin = margin(10, 5, 10, 5)
  )

# =====================================================================
# 3. Top Left (a): The Classic Median-Notched Boxplot
# =====================================================================
p_classic <- ggplot(plot_data, aes(x = Condition_Group, y = price)) +
  stat_boxplot(geom = "errorbar", width = 0.2, linewidth = 0.4) +
  geom_boxplot(
    notch = TRUE,
    fill = "white",
    width = 0.5,
    linewidth = 0.4,
    outlier.size = 2
  )  +
  labs(
    title = "(a) Median-notched boxplot (1978)",
    x = "",
    y = "Price (Thousands of USD)"
  ) +
  scale_y_continuous(labels = function(x) x / 1000) +
  jcgs_theme

# =====================================================================
# 4. Bottom Left (b): Proposed Mean Only (Facing)
# =====================================================================
p_dual <- notched_boxplot(
  data = plot_data,
  group_col = "Condition_Group",
  value_col = "price",
  show_mean_ci = TRUE,
  show_med_ci = TRUE,
  width = 0.5,
  indent_pct = 0.125
) +
  labs(
    title = "(b) Mean-notched boxplot",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(labels = function(x) x / 1000) +
  jcgs_theme +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

# =====================================================================
# 5. Assemble and Save
# =====================================================================
final_grid <- (p_classic | p_dual)

print(final_grid)

ggsave("diamonds_2.pdf", plot = final_grid, width = 8.5, height = 7.5, device = cairo_pdf)
