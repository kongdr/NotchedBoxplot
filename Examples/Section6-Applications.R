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
p_mean_only <- notched_boxplot(
  data = plot_data, 
  group_col = "Condition_Group", 
  value_col = "price",
  show_mean_ci = TRUE,    
  show_med_ci = FALSE,    
  mean_side = c("right", "left"), 
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
final_grid <- (p_classic | p_mean_only)

print(final_grid)

ggsave("diamonds_2.pdf", plot = final_grid, width = 8.5, height = 7.5, device = "pdf")


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
  labs(title = "(a) Median-notched boxplot (1978)", x = NULL, y = "Number of breaks") +
  jcgs_theme

# Plot (b): Proposed dual-notched boxplot
p_proposed <- notched_boxplot(warpbreaks, "Group", "breaks", 
                              show_mean_ci = TRUE, 
                              show_med_ci = TRUE, 
                              width = 0.35,
                              mean_side = "left") + 
  labs(title = "(b) Dual-notched boxplot", x = NULL, y = "Number of breaks") + 
  jcgs_theme

# ==============================================================================
# 4. Combine Panels and Save
# ==============================================================================
final_stacked_plot <- (p_classical / p_proposed) + 
  plot_layout(heights = c(1, 1))

print(final_stacked_plot)

ggsave("realdata_1978.pdf", final_stacked_plot, width = 8.2, height = 10, device = "pdf")