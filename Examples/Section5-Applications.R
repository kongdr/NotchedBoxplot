# warpbreaks
library(ggplot2)
library(patchwork)
library(NotchedBoxplot)
# ==============================================================================
# 1. Data Preparation
# ==============================================================================
data(warpbreaks)

# Combine wool and tension factors into group labels (e.g., A_L, A_M)
warpbreaks$Group <- paste(warpbreaks$wool, warpbreaks$tension, sep = "_")
warpbreaks$Group <- factor(
  warpbreaks$Group,
  levels = c("A_L", "B_L", "A_M", "B_M", "A_H", "B_H")
)

# ==============================================================================
# 2. Define Theme
# ==============================================================================
jcgs_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "grey92", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.8),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12, margin = margin(t = 5)),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 12,  hjust = 0.5, color = "grey20"),
    plot.margin = margin(10, 5, 10, 5)
  )

# ==============================================================================
# 3. Plotting
# ==============================================================================

# Plot (a): Median-notched boxplot (1978)
group_levels <- c("A_L", "B_L", "A_M", "B_M", "A_H", "B_H")
warpbreaks$Group <- factor(warpbreaks$Group, levels = group_levels)

p_classical <- ggplot(warpbreaks, aes(x = as.numeric(Group), y = breaks, group = Group)) +
  stat_boxplot(geom = "errorbar", width = 0.2, linewidth = 0.4) +
  geom_boxplot(
    notch = TRUE,
    fill = "white",
    width = 0.35,
    linewidth = 0.4,
    outlier.size = 2
  ) +
  scale_x_continuous(
    breaks = seq_along(group_levels),
    labels = group_levels,
    limits = c(0.5, length(group_levels) + 0.5),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(title = "(a) Median-notched boxplots (1978)", x = NULL, y = "Number of breaks") +
  jcgs_theme

# Plot (b): Proposed dual-notched boxplot
p_proposed <- notched_boxplot(warpbreaks, "Group", "breaks",
                              show_mean = TRUE,
                              show_med = TRUE,
                              width = 0.35,
                              mean_side = "left") +
  labs(title = "(b) Dual-notched boxplots", x = NULL, y = "Number of breaks") +
  jcgs_theme

# ==============================================================================
# 4. Combine Panels and Save
# ==============================================================================
final_stacked_plot <- (p_classical / p_proposed) +
  plot_layout(heights = c(1, 1))

print(final_stacked_plot)

ggsave("realdata_1978.pdf", final_stacked_plot, width = 8.2, height = 10, device = "pdf")
