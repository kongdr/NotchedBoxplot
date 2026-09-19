#' Title: Dual-notched boxplot using ggplot2
#'
#' This function generates a notched boxplot, which can simultaneously compare both the group means and medians.
#' For theoretical details, please refer to Kong et al. (2026).
#'
#' @references Deru Kong, Xuming He, WenWu Wang, Tiejun Tong (2026). Dual-notched Boxplot: A New Visualization for Simultaneous Comparison of Means and Medians
#'
#' @param data The data frame containing the data.
#' @param group_col The column name for grouping data (character string).
#' @param value_col The column name for the values to plot (character string).
#' @param fence_rule Character. The outlier detection rule. Options are "chau" (the sample-size-adjusted fences based on Chauvenet’s criterion) or "1.5" (Tukey’s fences with a 1.5 multiplier). Default is "chau".
#' @param show_mean Logical. Whether to display the mean notch. Default is TRUE.
#' @param show_med Logical. Whether to display the median notch. Default is TRUE.
#' @param width Numeric. The width of the box. Default is 0.4.
#' @param indent_pct Numeric. The depth of the notch indentation as a percentage of the width. Default is 0.125.
#' @param mean_side Character. The side of the box to display the mean notch ("left", "right" or "both"). Default is "left".
#' @param med_side Character. The side of the box to display the median notch ("left", "right" or "both"). Default is "right".
#' @param line_color Character. The color of the box borders, whiskers, and median line. Default is "black".
#' @param fill_color Character. The default fill color for the box. Default is "white".
#' @param mean_color Character. The color of the dashed mean line. Default is "black".
#' @param outlier_size Numeric. The size of the points representing outliers. Default is 2.
#' @param horizontal Logical. Whether to draw the boxplot horizontally. Default is FALSE.
#'
#' @return  A ggplot object representing the dual-notched boxplot.
#' @export
#' @import ggplot2
#' @importFrom dplyr %>% group_by summarise mutate filter left_join bind_rows select n
#' @importFrom stats median quantile IQR qnorm
#'
#' @examples
#' # Example using the built-in ToothGrowth dataset (Vertical)
#' notched_boxplot(data = ToothGrowth,
#'               group_col = "supp",
#'               value_col = "len",
#'               show_mean = TRUE,
#'               show_med = FALSE)
#'
#' # Example using the built-in iris dataset (Horizontal)
#' notched_boxplot(data = iris,
#'               group_col = "Species",
#'               value_col = "Sepal.Width",
#'               horizontal = TRUE)
notched_boxplot <- function(data,
                            group_col,
                            value_col,
                            fence_rule = c("chau", "1.5"),
                            show_mean = TRUE,
                            show_med = TRUE,
                            width = 0.4,
                            indent_pct = 0.125,
                            mean_side = "left",
                            med_side = "right",
                            line_color = "black",
                            fill_color = "white",
                            mean_color = "black",
                            outlier_size = 2,
                            horizontal = FALSE) {

  # --- 1. Statistical Calculations ---
  df <- data.frame(group = data[[group_col]], value = data[[value_col]])
  levels_x <- levels(factor(df$group))

  fence <- match.arg(fence_rule)
  stats_full <- df %>%
    group_by(group) %>%
    summarise(
      n_full = n(),
      med_val = median(value, na.rm = TRUE),
      q1 = quantile(value, 0.25, na.rm = TRUE),
      q3 = quantile(value, 0.75, na.rm = TRUE),
      iqr = IQR(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      fence_k = ifelse(fence_rule == "chau",
                       qnorm(1 - 0.25 / n_full) / 1.35 - 0.5,
                       1.5),
      fence_low = q1 - fence_k * iqr,
      fence_high = q3 + fence_k * iqr
    )

  df_with_fences <- df %>%
    left_join(stats_full %>% dplyr::select(group, fence_low, fence_high), by = "group")

  core_stats <- df_with_fences %>%
    filter(value >= fence_low & value <= fence_high) %>%
    group_by(group) %>%
    summarise(
      n_eff = n(),
      mean_val = mean(value, na.rm = TRUE),
      sd_in = sd(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(sd_in = ifelse(is.na(sd_in), 0, sd_in))

  stats <- stats_full %>%
    left_join(core_stats, by = "group") %>%
    mutate(
      ci_mean_delta = 1.7 * (sd_in / sqrt(n_eff)),
      mean_top = mean_val + ci_mean_delta,
      mean_bot = mean_val - ci_mean_delta,

      ci_med_delta = 1.7 * (1.25 * iqr / (1.349 * sqrt(n_full))),
      med_top = med_val + ci_med_delta,
      med_bot = med_val - ci_med_delta
    )

  # --- 2. Plotting Containers Initialization ---
  poly_fill_list <- list()
  border_segments <- data.frame()
  points_outlier <- data.frame()
  points_mean <- data.frame()
  segment_whiskers <- data.frame()
  segment_median <- data.frame()
  segment_mean <- data.frame()

  ind <- width * indent_pct
  m_sides <- rep(mean_side, length.out = length(levels_x))
  d_sides <- rep(med_side, length.out = length(levels_x))

  get_hg_indent <- function(target_y, center_y, top_y, bot_y, notch_depth) {
    if (target_y < top_y && target_y > bot_y) {
      half_h <- if(target_y >= center_y) (top_y - center_y) else (center_y - bot_y)
      ratio <- abs(target_y - center_y) / max(half_h, 1e-10)
      return(notch_depth * ratio)
    }
    return(0)
  }

  get_dm_indent <- function(target_y, center_y, top_y, bot_y, notch_depth) {
    if (target_y < top_y && target_y > bot_y) {
      half_h <- if(target_y >= center_y) (top_y - center_y) else (center_y - bot_y)
      ratio <- abs(target_y - center_y) / max(half_h, 1e-10)
      return(notch_depth * (1 - ratio))
    }
    return(0)
  }

  for(i in 1:length(levels_x)) {
    g_name <- levels_x[i]
    row <- stats[stats$group == g_name, ]
    curr_m_side <- m_sides[i]; curr_d_side <- d_sides[i]
    x_center <- i; w <- width / 2; x_left <- x_center - w; x_right <- x_center + w

    raw_vals <- df$value[df$group == g_name]
    out_vals <- raw_vals[raw_vals < row$fence_low | raw_vals > row$fence_high]
    if(length(out_vals) > 0) points_outlier <- rbind(points_outlier, data.frame(x=x_center, y=out_vals, group_id=g_name))

    poly_fill_list[[length(poly_fill_list)+1]] <- data.frame(
      x = c(x_left, x_left, x_right, x_right), y = c(row$q1, row$q3, row$q3, row$q1),
      group_id = g_name, type = "base_rect", poly_id = length(poly_fill_list) + 1)

    l_off_q3 <- 0; l_off_q1 <- 0; r_off_q3 <- 0; r_off_q1 <- 0
    if(show_mean && curr_m_side %in% c("left", "both")) {
      l_off_q3 <- max(l_off_q3, get_hg_indent(row$q3, row$mean_val, row$mean_top, row$mean_bot, ind))
      l_off_q1 <- max(l_off_q1, get_hg_indent(row$q1, row$mean_val, row$mean_top, row$mean_bot, ind))
    }
    if(show_med && curr_d_side %in% c("left", "both")) {
      l_off_q3 <- max(l_off_q3, get_dm_indent(row$q3, row$med_val, row$med_top, row$med_bot, ind))
      l_off_q1 <- max(l_off_q1, get_dm_indent(row$q1, row$med_val, row$med_top, row$med_bot, ind))
    }
    if(show_mean && curr_m_side %in% c("right", "both")) {
      r_off_q3 <- max(r_off_q3, get_hg_indent(row$q3, row$mean_val, row$mean_top, row$mean_bot, ind))
      r_off_q1 <- max(r_off_q1, get_hg_indent(row$q1, row$mean_val, row$mean_top, row$mean_bot, ind))
    }
    if(show_med && curr_d_side %in% c("right", "both")) {
      r_off_q3 <- max(r_off_q3, get_dm_indent(row$q3, row$med_val, row$med_top, row$med_bot, ind))
      r_off_q1 <- max(r_off_q1, get_dm_indent(row$q1, row$med_val, row$med_top, row$med_bot, ind))
    }

    border_segments <- rbind(border_segments, data.frame(
      x = c(x_left + l_off_q3, x_left + l_off_q1),
      xend = c(x_right - r_off_q3, x_right - r_off_q1),
      y = c(row$q3, row$q1), yend = c(row$q3, row$q1), group_id = g_name, line_type = "solid"))

    # Median solid line
    m_start_off <- if(show_med && curr_d_side %in% c("left", "both")) ind else if(show_mean && curr_m_side %in% c("left", "both")) get_hg_indent(row$med_val, row$mean_val, row$mean_top, row$mean_bot, ind) else 0
    m_end_off   <- if(show_med && curr_d_side %in% c("right", "both")) ind else if(show_mean && curr_m_side %in% c("right", "both")) get_hg_indent(row$med_val, row$mean_val, row$mean_top, row$mean_bot, ind) else 0
    segment_median <- rbind(segment_median, data.frame(x=x_left+m_start_off, xend=x_right-m_end_off, y=row$med_val, yend=row$med_val, group_id=g_name))

    # Mean dashed line
    if(show_mean){
      mean_start <- x_left
      mean_end <- x_right
      d_indent_at_mean <- 0
      if(show_med) d_indent_at_mean <- get_dm_indent(row$mean_val, row$med_val, row$med_top, row$med_bot, ind)

      if(show_med && curr_d_side %in% c("left", "both")) mean_start <- x_left + d_indent_at_mean
      if(show_med && curr_d_side %in% c("right", "both")) mean_end <- x_right - d_indent_at_mean
      segment_mean <- rbind(segment_mean, data.frame(x = mean_start, xend = mean_end, y = row$mean_val, yend = row$mean_val, group_id = g_name))
    }

    if(show_mean) {
      if(curr_m_side %in% c("left", "both")) {
        m_anchor <- x_left; m_tip <- x_left + ind; m_ext <- x_left - ind
        poly_fill_list[[length(poly_fill_list)+1]] <- data.frame(x=c(m_ext, m_tip, m_anchor), y=c(row$mean_top, row$mean_top, row$mean_val), group_id=g_name, type="mean_hg", poly_id = length(poly_fill_list) + 1)
        poly_fill_list[[length(poly_fill_list)+1]] <- data.frame(x=c(m_ext, m_tip, m_anchor), y=c(row$mean_bot, row$mean_bot, row$mean_val), group_id=g_name, type="mean_hg", poly_id = length(poly_fill_list) + 1)
        border_segments <- rbind(border_segments, data.frame(
          x=c(m_ext, m_ext, m_tip, m_ext, m_anchor, m_tip), y=c(row$mean_top, row$mean_bot, row$mean_top, row$mean_top, row$mean_val, row$mean_bot),
          xend=c(m_tip, m_tip, m_anchor, m_anchor, m_ext, m_anchor), yend=c(row$mean_top, row$mean_bot, row$mean_val, row$mean_val, row$mean_bot, row$mean_val),
          group_id=g_name, line_type = "solid"))
        points_mean <- rbind(points_mean, data.frame(x=m_anchor, y=row$mean_val, group_id=g_name))
      }
      if(curr_m_side %in% c("right", "both")) {
        m_anchor <- x_right; m_tip <- x_right - ind; m_ext <- x_right + ind
        poly_fill_list[[length(poly_fill_list)+1]] <- data.frame(x=c(m_ext, m_tip, m_anchor), y=c(row$mean_top, row$mean_top, row$mean_val), group_id=g_name, type="mean_hg", poly_id = length(poly_fill_list) + 1)
        poly_fill_list[[length(poly_fill_list)+1]] <- data.frame(x=c(m_ext, m_tip, m_anchor), y=c(row$mean_bot, row$mean_bot, row$mean_val), group_id=g_name, type="mean_hg", poly_id = length(poly_fill_list) + 1)
        border_segments <- rbind(border_segments, data.frame(
          x=c(m_ext, m_ext, m_tip, m_ext, m_anchor, m_tip), y=c(row$mean_top, row$mean_bot, row$mean_top, row$mean_top, row$mean_val, row$mean_bot),
          xend=c(m_tip, m_tip, m_anchor, m_anchor, m_ext, m_anchor), yend=c(row$mean_top, row$mean_bot, row$mean_val, row$mean_val, row$mean_bot, row$mean_val),
          group_id=g_name, line_type = "solid"))
        points_mean <- rbind(points_mean, data.frame(x=m_anchor, y=row$mean_val, group_id=g_name))
      }
    }

    if(show_med) {
      if(curr_d_side %in% c("left", "both")) {
        d_anchor <- x_left; d_tip_in <- x_left + ind; d_tip_out <- x_left - ind
        poly_fill_list[[length(poly_fill_list)+1]] <- data.frame(x=c(d_anchor, d_tip_out, d_anchor, d_tip_in), y=c(row$med_top, row$med_val, row$med_bot, row$med_val), group_id=g_name, type="med_diamond", poly_id = length(poly_fill_list) + 1)
        border_segments <- rbind(border_segments, data.frame(
          x=c(d_anchor, d_anchor, d_anchor, d_anchor), y=c(row$med_top, row$med_top, row$med_bot, row$med_bot),
          xend=c(d_tip_out, d_tip_in, d_tip_out, d_tip_in), yend=rep(row$med_val, 4),
          group_id=g_name, line_type = "solid"))
      }
      if(curr_d_side %in% c("right", "both")) {
        d_anchor <- x_right; d_tip_in <- x_right - ind; d_tip_out <- x_right + ind
        poly_fill_list[[length(poly_fill_list)+1]] <- data.frame(x=c(d_anchor, d_tip_out, d_anchor, d_tip_in), y=c(row$med_top, row$med_val, row$med_bot, row$med_val), group_id=g_name, type="med_diamond", poly_id = length(poly_fill_list) + 1)
        border_segments <- rbind(border_segments, data.frame(
          x=c(d_anchor, d_anchor, d_anchor, d_anchor), y=c(row$med_top, row$med_top, row$med_bot, row$med_bot),
          xend=c(d_tip_out, d_tip_in, d_tip_out, d_tip_in), yend=rep(row$med_val, 4),
          group_id=g_name, line_type = "solid"))
      }
    }

    draw_v <- function(sx, has_ci, ct, cb) {
      if(has_ci) {
        if(row$q3 > ct) border_segments <<- rbind(border_segments, data.frame(x=sx, y=row$q3, xend=sx, yend=ct, group_id=g_name, line_type="solid"))
        if(row$q1 < cb) border_segments <<- rbind(border_segments, data.frame(x=sx, y=row$q1, xend=sx, yend=cb, group_id=g_name, line_type="solid"))
      } else border_segments <<- rbind(border_segments, data.frame(x=sx, y=row$q1, xend=sx, yend=row$q3, group_id=g_name, line_type="solid"))
    }
    draw_v(x_left, (show_mean && curr_m_side %in% c("left", "both")) || (show_med && curr_d_side %in% c("left", "both")),
           if(curr_m_side %in% c("left", "both") && show_mean) row$mean_top else row$med_top, if(curr_m_side %in% c("left", "both") && show_mean) row$mean_bot else row$med_bot)
    draw_v(x_right, (show_mean && curr_m_side %in% c("right", "both")) || (show_med && curr_d_side %in% c("right", "both")),
           if(curr_m_side %in% c("right", "both") && show_mean) row$mean_top else row$med_top, if(curr_m_side %in% c("right", "both") && show_mean) row$mean_bot else row$med_bot)

    inside_vals <- raw_vals[raw_vals >= row$fence_low & raw_vals <= row$fence_high]
    act_low <- if(length(inside_vals)>0) min(inside_vals) else row$q1
    act_high <- if(length(inside_vals)>0) max(inside_vals) else row$q3
    segment_whiskers <- rbind(segment_whiskers, data.frame(x=rep(x_center,2), xend=rep(x_center,2), y=c(row$q1,row$q3), yend=c(act_low, act_high), group_id=g_name))
    segment_whiskers <- rbind(segment_whiskers, data.frame(x=rep(x_center-w*0.4,2), xend=rep(x_center+w*0.4,2), y=c(act_low, act_high), yend=c(act_low, act_high), group_id=g_name))
  }

  # --- 3. Plotting Layers ---
  p <- ggplot() +
    geom_polygon(data = bind_rows(poly_fill_list), aes(x = x, y = y, group = poly_id, fill = type), color = NA, alpha = 1) +
    scale_fill_manual(values = c("base_rect" = fill_color, "mean_hg" = "mediumpurple", "med_diamond" = "lightblue")) +
    geom_segment(data = border_segments,
                 aes(x = x, y = y, xend = xend, yend = yend, linetype = line_type),
                 color = line_color, linewidth = 0.5) +
    scale_linetype_identity() +

    geom_segment(data = segment_whiskers, aes(x = x, y = y, xend = xend, yend = yend), color = line_color, linewidth = 0.5) +
    geom_segment(data = segment_median, aes(x = x, y = y, xend = xend, yend = yend), color = line_color, linewidth = 0.5) +

    (if(nrow(segment_mean) > 0) geom_segment(data = segment_mean,
                                             aes(x = x, y = y, xend = xend, yend = yend),
                                             color = mean_color, linetype = "dashed", linewidth = 0.5)) +

    (if(nrow(points_outlier) > 0) geom_point(data = points_outlier, aes(x = x, y = y),
                                             shape = 16, size = outlier_size, color = "black")) +

    scale_x_continuous(
      breaks = seq_along(levels_x),
      labels = levels_x,
      limits = c(0.5, length(levels_x) + 0.5),
      expand = c(0, 0)
    ) +
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
      strip.text = element_text(size = 14, color = "black"),
      panel.spacing = unit(0, "lines")
    ) + guides(fill = "none")

  if (horizontal) {
    p <- p +
      coord_flip(clip = "off") +
      theme(axis.title.y = element_blank())
  } else {
    p <- p +
      coord_cartesian(clip = "off") +
      theme(axis.title.x = element_blank())
  }

  return(p)
}
