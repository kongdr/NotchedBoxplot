library(NotchedBoxplot)
# ==============================================================================
# PART 1: STUDY 1 - mean notches vs median notches
# ==============================================================================
n <- 200
standardize_normal <- function(n) {
  med0 <- 0
  iqr0 <- qnorm(0.75) - qnorm(0.25)

  x <- rnorm(n, mean = 0, sd = 1)
  (x - med0) / iqr0
}

standardize_gamma <- function(n, shape) {
  set.seed(2025)
  med0 <- qgamma(0.50, shape = shape, scale = 1)
  q1   <- qgamma(0.25, shape = shape, scale = 1)
  set.seed(2024)
  q3   <- qgamma(0.75, shape = shape, scale = 1)
  iqr0 <- q3 - q1

  x <- rgamma(n, shape = shape, scale = 1)
  (x - med0) / iqr0
}

reference <- standardize_normal(n)
skew_mild     <- standardize_gamma(n, shape = 5)
skew_moderate <- standardize_gamma(n, shape = 2)
skew_strong   <- standardize_gamma(n, shape = 0.3)

make_data <- function(reference, skewed) {
  data.frame(
    Group = factor(
      rep(c("Reference", "Skewed"), each = length(reference)),
      levels = c("Reference", "Skewed")
    ),
    Value = c(reference, skewed)
  )
}

dat_a <- make_data(reference, skew_mild)
dat_b <- make_data(reference, skew_moderate)
dat_c <- make_data(reference, skew_strong)

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
# ---------------------------------------------------
# Plot (a): Mild skewness
# ---------------------------------------------------
p1 <- notched_boxplot(dat_a, "Group", "Value",
                      show_mean = TRUE,
                      show_med = TRUE,
                      width = 0.5,
                      indent_pct = 0.125,
                      mean_side = "left",
                      med_side = "right"
) +
  coord_cartesian(ylim = c(-1.8, 5.5)) +
  labs(title = "(a) Mild skewness", x = NULL, y = "Standardized value") +
  jcgs_theme

# ---------------------------------------------------
# Plot (b): Moderate skewness
# ---------------------------------------------------
p2 <- notched_boxplot(dat_b, "Group", "Value",
                      show_mean = TRUE,
                      show_med = TRUE,
                      width = 0.5,
                      indent_pct = 0.125,
                      mean_side = "left",
                      med_side = "right"
) +
  coord_cartesian(ylim = c(-1.8, 5.5)) +
  labs(title = "(b) Moderate skewness", x = NULL, y = NULL) +
  jcgs_theme +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

# ---------------------------------------------------
# Plot (c): Strong skewness
# ---------------------------------------------------
p3 <- notched_boxplot(dat_c, "Group", "Value",
                      show_mean = TRUE,
                      show_med = TRUE,
                      width = 0.5,
                      indent_pct = 0.125,
                      mean_side = "left",
                      med_side = "right"
) +
  coord_cartesian(ylim = c(-1.8, 5.5)) +
  labs(title = "(c) Strong skewness", x = NULL, y = NULL) +
  jcgs_theme +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

# ==============================================================================
# 4. Combine panels
# ==============================================================================
final_panel <- p1 | p2 | p3

print(final_panel)
ggsave("notch_separation.pdf", final_panel, width = 9.5, height = 6.5, device = cairo_pdf)
