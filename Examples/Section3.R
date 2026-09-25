# ==============================================================================
# (a) Relative MAIE under 5 Outliers (Chauvenet as Baseline)
# (b) Relative MAIE under N(0,1) (Chauvenet as Baseline)
# ==============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Computation Function (MAIE)
calc_notch_stats <- function(x, k, n_original, c_mu = 1.7) {
  q1 <- quantile(x, 0.25, names = FALSE)
  q3 <- quantile(x, 0.75, names = FALSE)
  iqr <- q3 - q1

  if (is.infinite(k)) {
    lower_fence <- -Inf
    upper_fence <- Inf
  } else {
    lower_fence <- q1 - k * iqr
    upper_fence <- q3 + k * iqr
  }

  x_clean <- x[x >= lower_fence & x <= upper_fence]
  n_clean <- length(x_clean)

  m <- mean(x_clean)
  sd_clean <- sd(x_clean)
  se <- sd_clean / sqrt(n_clean)

  L <- m - c_mu * se
  U <- m + c_mu * se

  limit <- c_mu / sqrt(n_original)

  D <- abs(L + limit) + abs(U - limit)
  list(D = D, sd_clean = sd_clean)
}

set.seed(2026)

n_seq <- c(10,20, 50, 100,200,300,400, 500, 600, 700,800,900,1000)
iterations <- 10000
study2_results <- data.frame()

for (N in n_seq) {
  k_chau <- qnorm(1 - 0.25/N) / 1.35 - 0.5

  D_no_cont <- numeric(iterations)
  D_15_cont <- numeric(iterations)
  D_ch_cont <- numeric(iterations)

  D_no_pure <- numeric(iterations)
  D_15_pure <- numeric(iterations)
  D_ch_pure <- numeric(iterations)

  SD_15_pure <- numeric(iterations)
  SD_ch_pure <- numeric(iterations)

  for (i in 1:iterations) {
    # ---------------------------------------------------------
    # Scenario A: 5 Fixed Outliers
    # ---------------------------------------------------------
    n_contam <- 5
    x_contam <- c(rnorm(N - n_contam, 0, 1), rnorm(n_contam, 4, 1))

    D_no_cont[i] <- calc_notch_stats(x_contam, Inf, N - n_contam)$D
    D_15_cont[i] <- calc_notch_stats(x_contam, 1.5, N - n_contam)$D
    D_ch_cont[i] <- calc_notch_stats(x_contam, k_chau, N - n_contam)$D

    # ---------------------------------------------------------
    # Scenario B: N(0,1)
    # ---------------------------------------------------------
    x_pure <- rnorm(N, 0, 1)

    res_no_pure <- calc_notch_stats(x_pure, Inf, N)
    D_no_pure[i] <- res_no_pure$D

    res_15_pure <- calc_notch_stats(x_pure, 1.5, N)
    D_15_pure[i] <- res_15_pure$D
    SD_15_pure[i] <- res_15_pure$sd_clean

    res_ch_pure <- calc_notch_stats(x_pure, k_chau, N)
    D_ch_pure[i] <- res_ch_pure$D
    SD_ch_pure[i] <- res_ch_pure$sd_clean
  }

  study2_results <- rbind(study2_results, data.frame(
    N = N,
    # Scenario A: Chauvenet is the baseline denominator
    ReMAIE_No_Cont = mean(D_no_cont) / mean(D_ch_cont),
    ReMAIE_15_Cont = mean(D_15_cont) / mean(D_ch_cont),
    ReMAIE_Ch_Cont = mean(D_ch_cont) / mean(D_ch_cont), # Exactly 1.0

    # Scenario B: Chauvenet is the baseline denominator
    ReMAIE_No_Pure = mean(D_no_pure) / mean(D_ch_pure),
    ReMAIE_15_Pure = mean(D_15_pure) / mean(D_ch_pure),
    ReMAIE_Ch_Pure = mean(D_ch_pure) / mean(D_ch_pure), # Exactly 1.0

    # Internal Tracking
    SD_15_Pure = mean(SD_15_pure),
    SD_Ch_Pure = mean(SD_ch_pure)
  ))
  cat("Completed Dual Simulation for N =", N, "\n")
}

# ==============================================================================
# PLOTTING
# ==============================================================================
theme_unified <- function() {
  theme_minimal(base_size = 16) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(color = "gray95", linewidth = 0.3),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      axis.text.y = element_text(color = "black", size = 16),
      axis.text.x = element_text(color = "black", size = 16, margin = margin(t = 5)),
      axis.title = element_text(color = "black"),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 16,  hjust = 0.5, color = "grey20"),
      plot.margin = margin(10, 5, 10, 5),
      legend.key.width = unit(1.5, "cm")
    )
}

# ==============================================================================
# PLOTTING SECTION
# ==============================================================================
method_levels <- c("Without outlier removal","Fixed fences","Chauvenet-type fences")

method_colors <- c(
  "Chauvenet-type fences" = "#E63946",
  "Fixed fences" = "darkgreen",
  "Without outlier removal" = "black"
)

method_linetypes <- c(
  "Chauvenet-type fences" = "solid",
  "Fixed fences" = "dashed",
  "Without outlier removal" = "dotted"
)
# (a) Relative MAIE under 5 Outliers (Chauvenet as curve, baseline = 1)
plot_A_data <- study2_results %>%
  pivot_longer(cols = c(ReMAIE_15_Cont, ReMAIE_No_Cont, ReMAIE_Ch_Cont), names_to = "Method", values_to = "ReMAIE") %>%
  mutate(
    N = as.numeric(as.character(N)),
    Method = factor(case_when(
      Method == "ReMAIE_No_Cont" ~ "Without outlier removal",
      Method == "ReMAIE_Ch_Cont" ~ "Chauvenet-type fences",
      Method == "ReMAIE_15_Cont" ~ "Fixed fences"
    ), levels = method_levels)
  )

pA <- ggplot(plot_A_data, aes(x = N, y = ReMAIE, color = Method, group = Method, linetype = Method)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0.6, 3), breaks = c(0.6, 1.0, 1.4, 1.8, 2.2, 2.6, 3.0)) +
  scale_x_continuous(limits = c(0, 1050), breaks = seq(0, 1000, 250)) +
  labs(title = "(a) ReMAIE under 5 Outliers", x = "n", y = "ReMAIE") +
  scale_color_manual(values = method_colors, limits = method_levels) +
  scale_linetype_manual(values = method_linetypes, limits = method_levels) +
  theme_unified()

# (b) Relative MAIE under N(0,1) (Chauvenet and Fixed 1.5)
plot_B_data <- study2_results %>%
  pivot_longer(cols = c(ReMAIE_15_Pure, ReMAIE_Ch_Pure), names_to = "Method", values_to = "ReMAIE") %>%
  mutate(
    N = as.numeric(as.character(N)),
    Method = factor(case_when(
      Method == "ReMAIE_Ch_Pure" ~ "Chauvenet-type fences",
      Method == "ReMAIE_15_Pure" ~ "Fixed fences"
    ), levels = method_levels)
  )
target_n <- c(10, 20, 50, 100, 200, 300, 400,500, 800, 1000)
pB <- ggplot(plot_B_data %>% filter(N %in% target_n), aes(x = N, y = ReMAIE, color = Method, group = Method, linetype = Method)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0.9, 1.1), breaks = c(0.9, 0.95, 1.0, 1.05, 1.1)) +
  scale_x_continuous(limits = c(0, 1050), breaks = c(0, 250, 500, 750, 1000)) +
  labs(title = "(b) ReMAIE under N(0,1)", x = "n", y = "") +
  scale_color_manual(values = method_colors) +
  scale_linetype_manual(values = method_linetypes) +
  guides(color = "none", linetype = "none") +
  theme_unified()

# Combine Plots
plot_final <- (pA | pB) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.box.just = "center",
    legend.key.width = unit(2, "cm")
  )

print(plot_final)
ggsave("fences.pdf", plot_final, width = 12, height = 5, device = "pdf")
