library(ggplot2)
library(dplyr)

set.seed(2026) 

# Define a function to generate data with a subtle mean shift
generate_data <- function(n) {
  # Group A: Standard normal distribution as the baseline
  group_a <- rnorm(n, mean = 0, sd = 1)
  
  # Group B: Normal distribution with a slight mean shift of +0.3
  group_b <- rnorm(n, mean = 0.3, sd = 1)
  
  data.frame(
    Size_Num = n,
    Group = rep(c("Group A", "Group B"), each = n),
    Value = c(group_a, group_b)
  )
}

# Generate data for two different sample sizes
df_plot <- bind_rows(
  generate_data(200),  
  generate_data(2000)   
)

# Create and format factor levels for faceting based on sample size
df_plot$N_Label <- factor(df_plot$Size_Num, 
                          levels = c(200, 2000),
                          labels = c("n = 200", "n = 2000"))

# ==========================================
# Define a custom global theme 
# ==========================================
jcgs_theme <- theme_minimal(base_size = 14) + 
  theme(
    panel.background = element_rect(fill = "grey92", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.8),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12), 
    plot.title = element_text(size = 12, hjust = 0.5, color = "grey20"),
    plot.margin = margin(10, 5, 10, 5),
    axis.text = element_text(color = "black"),
    legend.position = "none",
    strip.placement = "outside",           
    strip.background = element_blank(),    
    strip.text = element_text(size = 12, color = "black"),
    panel.spacing = unit(0, "lines")       
  )

# ==========================================
# Generate the Boxplot
# ==========================================
p_evolution <- ggplot(df_plot, aes(x = Group, y = Value)) + 
  stat_boxplot(geom = "errorbar", width = 0.2, color = "black", linewidth = 0.4) +
  geom_boxplot(notch = TRUE, 
               fill = "white",            
               outlier.colour = "black", 
               outlier.shape = 16, 
               outlier.size = 2,
               width = 0.5, linewidth = 0.4,
               alpha = 1) +
  facet_wrap(~ N_Label, strip.position = "bottom") +
  labs(title = NULL, x = NULL, y = NULL) +
  jcgs_theme 

# Print the plot
print(p_evolution)

# Save the final plot to a PDF file
ggsave("Limit3.pdf", p_evolution, width = 8.2, height = 7, device = "pdf")


#======The Median-notched Boxplot (1978) and Tukey's modification (1993) =======
library(ggplot2)
library(dplyr)
library(patchwork)

# ---------------------------------------------------------
# 1. DATA PREPARATION (All 6 Warpbreaks Groups)
# ---------------------------------------------------------
data(warpbreaks)

# Combine wool and tension to create A-L, A-M, etc.
warpbreaks$Group <- paste(warpbreaks$wool, warpbreaks$tension, sep = "_")

# Define the 6 target levels
group_levels <- c("A_L", "B_L", "A_M", "B_M", "A_H", "B_H")

warpbreaks$Group <- factor(warpbreaks$Group, levels = group_levels)

# Use the full dataset
plot_data <- warpbreaks

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
# Panel (a): The 1978 Original Notched Boxplot
# =====================================================================
p_1978 <- ggplot(plot_data, aes(x = as.numeric(Group), y = breaks, group = Group)) +
  stat_boxplot(geom = "errorbar", width = 0.2, linewidth = 0.4) +
  geom_boxplot(
    notch = TRUE, 
    fill = "white", 
    width = 0.35, 
    linewidth = 0.4,
    outlier.size = 2
  ) +
  scale_x_continuous(
    breaks = 1:length(group_levels),
    labels = group_levels,
    limits = c(0.5, length(group_levels) + 0.5), # Expanded limits for 6 groups
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "(a) Median-notched boxplot (1978)", 
    y = "Number of breaks"
  ) +
  jcgs_theme

# =====================================================================
# Panel (b): The 1993 Tukey Diaglypt (Thicker Waist)
# =====================================================================
stats <- plot_data %>%
  group_by(Group) %>%
  summarise(
    y0 = min(boxplot.stats(breaks)$stats),      
    y25 = quantile(breaks, 0.25),               
    y50 = median(breaks),                       
    y75 = quantile(breaks, 0.75),               
    y100 = max(boxplot.stats(breaks)$stats),    
    iqr = IQR(breaks),
    n = n(),
    ci_low = y50 - 1.58 * iqr / sqrt(n),       
    ci_high = y50 + 1.58 * iqr / sqrt(n),
    .groups = 'drop'
  )

outliers <- plot_data %>%
  group_by(Group) %>%
  filter(breaks < min(boxplot.stats(breaks)$stats) | breaks > max(boxplot.stats(breaks)$stats)) %>%
  ungroup()

poly_data <- data.frame()

actual_hinge_w <- 0.175    
p_w <- 0.085               
fence_w <- 0.10            

# The math remains identical to preserve the flawless pi/4 smooth curve
w <- p_w + (actual_hinge_w - p_w) / (1 - cos(pi / 4))

for(i in 1:nrow(stats)) {
  row <- stats[i,]
  x_center <- i
  
  y_vals <- seq(row$ci_low, row$ci_high, length.out = 100)
  h_diff <- max(row$ci_high - row$y50, 1e-6) 
  
  angles <- (abs(y_vals - row$y50) / h_diff) * (pi / 4)
  x_offsets <- p_w + (w - p_w) * (1 - cos(angles))
  
  poly <- data.frame(
    Group = row$Group,
    x = c(x_center - rev(x_offsets), x_center + x_offsets),
    y = c(rev(y_vals), y_vals)
  )
  poly_data <- rbind(poly_data, poly)
}

p_1993 <- ggplot() +
  geom_segment(data = stats, aes(x=as.numeric(as.factor(Group)), xend=as.numeric(as.factor(Group)), 
                                 y=pmax(y75, ci_high), yend=y100), linewidth=0.4) +
  geom_segment(data = stats, aes(x=as.numeric(as.factor(Group)), xend=as.numeric(as.factor(Group)), 
                                 y=pmin(y25, ci_low), yend=y0), linewidth=0.4) +
  
  geom_segment(data = stats, aes(x=as.numeric(as.factor(Group)) - actual_hinge_w, xend=as.numeric(as.factor(Group)) + actual_hinge_w, 
                                 y=y75, yend=y75), linewidth=0.4) +
  geom_segment(data = stats, aes(x=as.numeric(as.factor(Group)) - actual_hinge_w, xend=as.numeric(as.factor(Group)) + actual_hinge_w, 
                                 y=y25, yend=y25), linewidth=0.4) +
  
  geom_segment(data = stats, aes(x=as.numeric(as.factor(Group)) - fence_w, xend=as.numeric(as.factor(Group)) + fence_w, 
                                 y=y100, yend=y100), linewidth=0.4) +
  geom_segment(data = stats, aes(x=as.numeric(as.factor(Group)) - fence_w, xend=as.numeric(as.factor(Group)) + fence_w, 
                                 y=y0, yend=y0), linewidth=0.4) +
  
  geom_polygon(data = poly_data, aes(x=x, y=y, group=Group), fill="black") +
  
  geom_point(data = stats, aes(x=as.numeric(as.factor(Group)), y=y50), color="white", size=2.5) +
  
  geom_point(data = outliers, aes(x=as.numeric(as.factor(Group)), y=breaks), shape=1, size=2) +
  
  scale_x_continuous(
    breaks = 1:length(group_levels), 
    labels = group_levels, 
    limits = c(0.5, length(group_levels) + 0.5), 
    expand = c(0, 0)
  ) +
  labs(
    title = "(b) Tukey's modification (1993)",
    y = "Number of breaks" 
  ) +
  jcgs_theme

# =====================================================================
# Assemble and Display the Stacked Grid
# =====================================================================
final_plot <- (p_1978 / p_1993) + 
  plot_layout(heights = c(1, 1))
print(final_plot)
ggsave("classic_notch_1.pdf", final_plot, width = 8.2, height = 10, device = "pdf")
