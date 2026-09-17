library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)

set.seed(2026)
iterations <- 2000
multiplier <- 1.7
alpha <- 0.05

calc_notch_and_mean <- function(x, k_multiplier) {
  q1 <- quantile(x, 0.25, names = FALSE)
  q3 <- quantile(x, 0.75, names = FALSE)
  iqr_full <- q3 - q1
  
  lf <- q1 - k_multiplier * iqr_full
  uf <- q3 + k_multiplier * iqr_full
  
  x_in <- x[x >= lf & x <= uf]
  
  n_eff <- length(x_in)
  mean_in <- mean(x_in)
  
  sd_in <- ifelse(n_eff > 1, sd(x_in), 0)
  hw <- multiplier * sd_in / sqrt(n_eff)
  
  return(list(
    notch = c(lower = mean_in - hw, upper = mean_in + hw),
    est_mean = mean_in
  ))
}

# ==============================================================================
# Define a unified theme for all output plots
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
      plot.margin = margin(10, 5, 10, 5)
    )
}

# ==============================================================================
# PART 1: STUDY 1 - Visual Power under different distributions
# ==============================================================================
N_study1 <- 500
delta_seq_1 <- seq(0, 0.8, by = 0.1) 
study1_results <- data.frame()

# Chauvenet coefficient for N = 500
k_chau_study1 <- qnorm(1 - 0.25/N_study1) / 1.35 - 0.5

scenarios_study1 <- c("Normal", "Heavy-Tailed", "LogNormal", "Variance_Mixture")

for (dist_type in scenarios_study1) {
  for (delta in delta_seq_1) {
    rej_sd <- logical(iterations)
    rej_welch <- logical(iterations)
    
    for (i in 1:iterations) {
      if (dist_type == "Normal") {
        g_A <- rnorm(N_study1, 0, 1)
        g_B <- rnorm(N_study1, 0, 1) + delta
        
      } else if (dist_type == "Heavy-Tailed") {
        g_A <- rt(N_study1, df = 3)
        g_B <- rt(N_study1, df = 3) + delta 
        
      } else if (dist_type == "LogNormal") {
        g_A <- rlnorm(N_study1, 0, 1)
        g_B <- rlnorm(N_study1, 0, 1) + delta
        
      } else if (dist_type == "Variance_Mixture") {
        g_A <- ifelse(runif(N_study1) < 0.85, rnorm(N_study1, 0, 1), rnorm(N_study1, 0, 3))
        g_B <- ifelse(runif(N_study1) < 0.85, rnorm(N_study1, 0, 1), rnorm(N_study1, 0, 3)) + delta
      }
      
      rej_welch[i] <- t.test(g_A, g_B)$p.value <= alpha
      
      res_A <- calc_notch_and_mean(g_A, k_chau_study1)
      res_B <- calc_notch_and_mean(g_B, k_chau_study1)
      
      rej_sd[i] <- max(res_A$notch["lower"], res_B$notch["lower"]) > min(res_A$notch["upper"], res_B$notch["upper"])
    }
    
    study1_results <- rbind(study1_results, data.frame(
      Distribution = dist_type, Delta = delta,
      Notch_SD = mean(rej_sd), Welch = mean(rej_welch)
    ))
  }
}

# ==============================================================================
# PART 3: 4-PANEL COMPOSITE FIGURE GENERATION
# ==============================================================================
study1_colors <- c("Notch_SD" = "#E63946", "Welch" = "black")
study1_lines <- c("Notch_SD" = "solid", "Welch" = "dashed")
study1_labels <- c("Mean notch", "Welch t-test")

# (a) Normal Data Calibration
plot_A <- study1_results %>% filter(Distribution == "Normal") %>%
  pivot_longer(cols = c(Notch_SD, Welch), names_to = "Method", values_to = "Rate")

pA <- ggplot(plot_A, aes(x = Delta, y = Rate, color = Method, linetype = Method)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black") +
  annotate("text", x = 0.7, y = 0.12, label = "alpha == 0.05", parse = TRUE) +
  labs(title = "(a) Normally distributed data", x = expression(delta), y = "Rejection rate") +
  scale_color_manual(values = study1_colors, labels = study1_labels) +
  scale_linetype_manual(values = study1_lines, labels = study1_labels) +
  theme_unified()

# (b) Heavy-Tailed Data Calibration
plot_B <- study1_results %>% filter(Distribution == "Heavy-Tailed") %>%
  pivot_longer(cols = c(Notch_SD, Welch), names_to = "Method", values_to = "Rate")

pB <- ggplot(plot_B, aes(x = Delta, y = Rate, color = Method, linetype = Method)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black") +
  annotate("text", x = 0.7, y = 0.12, label = "alpha == 0.05", parse = TRUE) +
  labs(title = "(b) Heavy-tailed data ", x = expression(delta), y = "Rejection rate") + 
  scale_color_manual(values = study1_colors, labels = study1_labels) +
  scale_linetype_manual(values = study1_lines, labels = study1_labels) +
  theme_unified() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) 

# (c) Variance Mixture Data Calibration
plot_C_dist <- study1_results %>% filter(Distribution == "Variance_Mixture") %>%
  pivot_longer(cols = c(Notch_SD, Welch), names_to = "Method", values_to = "Rate")

pC_dist <- ggplot(plot_C_dist, aes(x = Delta, y = Rate, color = Method, linetype = Method)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black") +
  annotate("text", x = 0.7, y = 0.12, label = "alpha == 0.05", parse = TRUE) +
  labs(title = "(c) Variance-mixture data", x = expression(delta), y = "Rejection rate") + 
  scale_color_manual(values = study1_colors, labels = study1_labels) +
  scale_linetype_manual(values = study1_lines, labels = study1_labels) +
  theme_unified() 

# (d) Skewed Data Calibration (LogNormal)
plot_D_dist <- study1_results %>% filter(Distribution == "LogNormal") %>%
  pivot_longer(cols = c(Notch_SD, Welch), names_to = "Method", values_to = "Rate")

pD_dist <- ggplot(plot_D_dist, aes(x = Delta, y = Rate, color = Method, linetype = Method)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black") +
  annotate("text", x = 0.7, y = 0.12, label = "alpha == 0.05", parse = TRUE) +
  labs(title = "(d) Skewed data", x = expression(delta), y = "Rejection rate") +
  scale_color_manual(values = study1_colors, labels = study1_labels) +
  scale_linetype_manual(values = study1_lines, labels = study1_labels) +
  theme_unified()+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) 

# Combine into a 2x2 layout
plot_Visual <- (pA | pB) / (pC_dist | pD_dist) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", legend.box = "horizontal")

print(plot_Visual)
ggsave("Visual_power.pdf", plot_Visual, width = 12, height = 9, device = "pdf")

# ==============================================================================
# PART 2: STUDY 2 - Robustness under Contamination
# ==============================================================================
n_seq <- c(100, 200, 500, 1000, 2000, 5000, 10000)
contam_rate <- 0.02
valid_tail_prob <- 0.07  
m_tail <- 3.2
s_tail <- 0.2
delta_true <- valid_tail_prob * m_tail 

study2_results <- data.frame()

for (N in n_seq) {
  k_chau <- qnorm(1 - 0.25/N) / 1.35 - 0.5
  n_contam <- round(N * contam_rate)
  n_clean <- N - n_contam
  
  rej_15 <- logical(iterations)
  rej_ch <- logical(iterations)
  diff_15 <- numeric(iterations)
  diff_ch <- numeric(iterations)
  
  for (i in 1:iterations) {
    nB_tail <- rbinom(1, size = n_clean, prob = valid_tail_prob)
    nB_core <- n_clean - nB_tail
    
    clean_A <- rnorm(n_clean, 0, 1)
    contam_A <- runif(n_contam, 7, 8)
    g_A <- c(clean_A, contam_A)
    
    clean_B <- c(rnorm(nB_core, 0, 1), rnorm(nB_tail, m_tail, s_tail))
    contam_B <- runif(n_contam, 7, 8)
    g_B <- c(clean_B, contam_B)
    
    res_A_15 <- calc_notch_and_mean(g_A, 1.5)
    res_B_15 <- calc_notch_and_mean(g_B, 1.5)
    rej_15[i] <- max(res_A_15$notch["lower"], res_B_15$notch["lower"]) > min(res_A_15$notch["upper"], res_B_15$notch["upper"])
    diff_15[i] <- res_B_15$est_mean - res_A_15$est_mean
    
    res_A_ch <- calc_notch_and_mean(g_A, k_chau)
    res_B_ch <- calc_notch_and_mean(g_B, k_chau)
    rej_ch[i] <- max(res_A_ch$notch["lower"], res_B_ch$notch["lower"]) > min(res_A_ch$notch["upper"], res_B_ch$notch["upper"])
    diff_ch[i] <- res_B_ch$est_mean - res_A_ch$est_mean
  }
  
  study2_results <- rbind(study2_results, data.frame(
    N = N,
    Reject_15 = mean(rej_15),
    Reject_Ch = mean(rej_ch),
    RMSE_15 = sqrt(mean((diff_15 - delta_true)^2)),
    RMSE_Ch = sqrt(mean((diff_ch - delta_true)^2))
  ))
  cat("Completed Contamination Simulation for N =", N, "\n")
}

# ==============================================================================
# Plot Stress Test Results
# ==============================================================================

# (a) Power under Contamination
plot_C <- study2_results %>%
  pivot_longer(cols = c(Reject_15, Reject_Ch), names_to = "Method", values_to = "Rate") %>%
  mutate(N = factor(N, levels = n_seq))

pC <- ggplot(plot_C, aes(x = N, y = Rate, color = Method, group = Method, linetype = Method)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  labs(title = "(a) Power under contamination", x = "n", y = "Power") +
  scale_color_manual(
    breaks = c("Reject_Ch", "Reject_15"), 
    values = c("Reject_Ch" = "#E63946", "Reject_15" = "darkgreen"), 
    labels = c("Reject_Ch" = "Chauvenet", "Reject_15" = "Fixed 1.5")
  ) +
  scale_linetype_manual(
    breaks = c("Reject_Ch", "Reject_15"), 
    values = c("Reject_Ch" = "dotdash",  "Reject_15" = "dashed"), 
    labels = c("Reject_Ch" = "Chauvenet", "Reject_15" = "Fixed 1.5")
  ) +
  theme_unified()

# (b) RMSE of Mean-Difference Recovery
plot_D <- study2_results %>%
  pivot_longer(cols = c(RMSE_15, RMSE_Ch), names_to = "Method", values_to = "Error") %>%
  mutate(N = factor(N, levels = n_seq))

pD <- ggplot(plot_D, aes(x = N, y = Error, color = Method, group = Method, linetype = Method)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  labs(title = "(b) RMSE of mean difference", x = "n", y = "RMSE") +
  scale_color_manual(
    breaks = c("RMSE_Ch", "RMSE_15"), 
    values = c("RMSE_Ch" = "#E63946",  "RMSE_15" = "darkgreen"), 
    labels = c("RMSE_Ch" = "Chauvenet", "RMSE_15" = "Fixed 1.5")
  ) +
  scale_linetype_manual(
    breaks = c("RMSE_Ch", "RMSE_15"), 
    values = c("RMSE_Ch" = "dotdash", "RMSE_15" = "dashed"), 
    labels = c("RMSE_Ch" = "Chauvenet", "RMSE_15" = "Fixed 1.5")
  ) +
  theme_unified()

plot_conta <- (pC | pD) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", legend.box = "horizontal")

print(plot_conta) 
ggsave("conta.pdf", plot_conta, width = 12, height = 5, device = "pdf")