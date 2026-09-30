library(ggplot2)
library(patchwork)
library(NotchedBoxplot)
# ==============================================================================
# Setting I (Equal Medians)
# ==============================================================================
n <- 200
set.seed(2025)
x_ref <- rnorm(n)
x_g5  <- rgamma(n, shape = 5,   scale = 1)
x_g2  <- rgamma(n, shape = 2,   scale = 1)
set.seed(4443)
x_g03 <- rgamma(n, shape = 0.6, scale = 1)

# ==============================================================================
# Define Population Parameters for Transformation
# ==============================================================================
iqr_norm <- qnorm(0.75) - qnorm(0.25)

get_gamma_params <- function(shape) {
  list(
    mean   = shape * 1,
    median = qgamma(0.50, shape = shape, scale = 1),
    iqr    = qgamma(0.75, shape = shape, scale = 1) - qgamma(0.25, shape = shape, scale = 1)
  )
}

p_g5  <- get_gamma_params(5)
p_g2  <- get_gamma_params(2)
p_g03 <- get_gamma_params(0.6)

# ==============================================================================
# Apply Transformations (Setting I and Setting II)
# ==============================================================================

# Setting I: Equal Medians (Centered at Population Median)
ref_med       <- (x_ref - 0) / iqr_norm
skew_mild_I   <- (x_g5  - p_g5$median) / p_g5$iqr
skew_mod_I    <- (x_g2  - p_g2$median) / p_g2$iqr
skew_strong_I <- (x_g03 - p_g03$median) / p_g03$iqr

# Setting II: Equal Means (Centered at Population Mean)
ref_mean       <- (x_ref - 0) / iqr_norm
skew_mild_II   <- (x_g5  - p_g5$mean) / p_g5$iqr
skew_mod_II    <- (x_g2  - p_g2$mean) / p_g2$iqr
skew_strong_II <- (x_g03 - p_g03$mean) / p_g03$iqr

# ==============================================================================
# PART 4: Assemble Data Frames for the 2x3 Plot Layout
# ==============================================================================
make_data <- function(reference, skewed) {
  data.frame(
    Group = factor(
      rep(c("Reference", "Skewed"), each = length(reference)),
      levels = c("Reference", "Skewed")
    ),
    Value = c(reference, skewed)
  )
}

# Setting I
dat_I_mild   <- make_data(ref_med, skew_mild_I)
dat_I_mod    <- make_data(ref_med, skew_mod_I)
dat_I_strong <- make_data(ref_med, skew_strong_I)

# Setting II
dat_II_mild   <- make_data(ref_mean, skew_mild_II)
dat_II_mod    <- make_data(ref_mean, skew_mod_II)
dat_II_strong <- make_data(ref_mean, skew_strong_II)
plot_ylim <- c(-2, 5.5)

# ==============================================================================
# Global Theme Definition
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
    plot.title = element_text(size = 14, hjust = 0.5, color = "grey20"),
    plot.margin = margin(10, 5, 5, 5),
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 14, color = "black"),
    panel.spacing = unit(0, "lines")
  )
# ==============================================================================
# ROW 1: Setting I (Equal Medians)
# ==============================================================================
p1_I <- notched_boxplot(dat_I_mild, "Group", "Value",
                        show_mean = TRUE, show_med = TRUE,
                        width = 0.5, indent_pct = 0.125,
                        mean_side = "left", med_side = "right") +
  coord_cartesian(ylim = plot_ylim) +
  labs(title = "(a) Mild skewness", x = NULL, y = "Standardized value") +
  jcgs_theme

p2_I <- notched_boxplot(dat_I_mod, "Group", "Value",
                        show_mean = TRUE, show_med = TRUE,
                        width = 0.5, indent_pct = 0.125,
                        mean_side = "left", med_side = "right") +
  coord_cartesian(ylim = plot_ylim) +
  labs(title = "(b) Moderate skewness", x = NULL, y = NULL) +
  jcgs_theme +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

p3_I <- notched_boxplot(dat_I_strong, "Group", "Value",
                        show_mean = TRUE, show_med = TRUE,
                        width = 0.5, indent_pct = 0.125,
                        mean_side = "left", med_side = "right") +
  coord_cartesian(ylim = plot_ylim) +
  labs(title = "(c) Strong skewness", x = NULL, y = NULL) +
  jcgs_theme +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

# ==============================================================================
# ROW 2: Setting II (Equal Means)
# ==============================================================================
p1_II <- notched_boxplot(dat_II_mild, "Group", "Value",
                         show_mean = TRUE, show_med = TRUE,
                         width = 0.5, indent_pct = 0.125,
                         mean_side = "left", med_side = "right") +
  coord_cartesian(ylim = plot_ylim) +
  labs(title = "(d) Mild skewness", x = NULL, y = "Standardized value") +
  jcgs_theme

p2_II <- notched_boxplot(dat_II_mod, "Group", "Value",
                         show_mean = TRUE, show_med = TRUE,
                         width = 0.5, indent_pct = 0.125,
                         mean_side = "left", med_side = "right") +
  coord_cartesian(ylim = plot_ylim) +
  labs(title = "(e) Moderate skewness", x = NULL, y = NULL) +
  jcgs_theme +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

p3_II <- notched_boxplot(dat_II_strong, "Group", "Value",
                         show_mean = TRUE, show_med = TRUE,
                         width = 0.5, indent_pct = 0.125,
                         mean_side = "left", med_side = "right") +
  coord_cartesian(ylim = plot_ylim) +
  labs(title = "(f) Strong skewness", x = NULL, y = NULL) +
  jcgs_theme +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

# ==============================================================================
# 4. Combine all panels using patchwork
# ==============================================================================
row1 <- wrap_elements((p1_I | p2_I | p3_I) +
                        plot_annotation(title = "(I) Equal medians",
                                        theme = theme(plot.title = element_text(hjust = 0.5, size = 15,  color = "black"))))

row2 <- wrap_elements((p1_II | p2_II | p3_II) +
                        plot_annotation(title = "(II) Equal means",
                                        theme = theme(plot.title = element_text(hjust = 0.5, size = 15,  color = "black"))))

final_panel <- row1 / row2

print(final_panel)
ggsave("notch_separation.pdf", final_panel, width = 9.5, height = 12, device = cairo_pdf)
