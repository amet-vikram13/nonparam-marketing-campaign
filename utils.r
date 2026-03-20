################################################################################
# utils.r — Generate all tables and figures for the LaTeX report
# Run this script AFTER marketing_campaign_analysis.r and
# model_comparison_and_prediction.r, or standalone (it loads the data itself).
################################################################################

rm(list = ls())

# ---- Libraries ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(gridExtra)

# ---- Create output directory ----
output_dir <- "report_output"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ---- Load data ----
marketing_data <- read.csv("WA_Marketing-Campaign.csv", header = TRUE)
marketing_data$Treatment <- as.factor(marketing_data$Treatment)

cat("Data loaded:", nrow(marketing_data), "observations,",
    length(unique(marketing_data$LocationID)), "locations\n")

################################################################################
# 1. TABLE: Summary Statistics by Treatment (tab:summary_stats)
################################################################################

summary_stats <- marketing_data %>%
  group_by(Treatment) %>%
  summarise(
    N      = n(),
    Mean   = round(mean(SalesInThousands), 2),
    Median = round(median(SalesInThousands), 2),
    SD     = round(sd(SalesInThousands), 2),
    Min    = round(min(SalesInThousands), 2),
    Max    = round(max(SalesInThousands), 2),
    Q1     = round(quantile(SalesInThousands, 0.25), 2),
    Q3     = round(quantile(SalesInThousands, 0.75), 2),
    IQR    = round(IQR(SalesInThousands), 2),
    .groups = "drop"
  )

# Write CSV
write.csv(summary_stats, file.path(output_dir, "tab_summary_stats.csv"),
          row.names = FALSE)

# Generate LaTeX table rows
sink(file.path(output_dir, "tab_summary_stats.tex"))
for (i in 1:nrow(summary_stats)) {
  r <- summary_stats[i, ]
  cat(sprintf("%s & %d & %.2f & %.2f & %.2f & %.2f & %.2f & %.2f \\\\\n",
              r$Treatment, r$N, r$Mean, r$Median, r$SD, r$Min, r$Max, r$IQR))
}
sink()

cat("Written: tab_summary_stats.csv / .tex\n")

################################################################################
# 2. TABLE: Market Distribution (tab:market_distribution)
################################################################################

market_dist <- marketing_data %>%
  group_by(MarketSize) %>%
  summarise(
    Observations = n(),
    Markets      = length(unique(MarketID)),
    Locations    = length(unique(LocationID)),
    .groups = "drop"
  )

write.csv(market_dist, file.path(output_dir, "tab_market_distribution.csv"),
          row.names = FALSE)

sink(file.path(output_dir, "tab_market_distribution.tex"))
for (i in 1:nrow(market_dist)) {
  r <- market_dist[i, ]
  cat(sprintf("%s & %d & %d & %d \\\\\n",
              r$MarketSize, r$Observations, r$Markets, r$Locations))
}
sink()

cat("Written: tab_market_distribution.csv / .tex\n")

################################################################################
# 3. FIGURE: Box plots (fig:boxplots)
################################################################################

p_box <- ggplot(marketing_data,
                aes(x = Treatment, y = SalesInThousands, fill = Treatment)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title    = "Sales Distribution by Promotion Type",
       subtitle = "Box plots with individual data points",
       x = "Promotion Type", y = "Sales (in Thousands)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "none")

ggsave(file.path(output_dir, "plot_boxplots.png"),
       plot = p_box, width = 10, height = 6, dpi = 300)
cat("Written: plot_boxplots.png\n")

################################################################################
# 4. FIGURE: Violin plots (fig:violins)
################################################################################

p_violin <- ggplot(marketing_data,
                   aes(x = Treatment, y = SalesInThousands, fill = Treatment)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title    = "Sales Distribution Density by Promotion",
       subtitle = "Violin plots showing distribution shape",
       x = "Promotion Type", y = "Sales (in Thousands)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "none")

ggsave(file.path(output_dir, "plot_violins.png"),
       plot = p_violin, width = 10, height = 6, dpi = 300)
cat("Written: plot_violins.png\n")

################################################################################
# 5. TABLE: Kruskal-Wallis results (tab:kw_results)
################################################################################

kw_test <- kruskal.test(SalesInThousands ~ Treatment, data = marketing_data)
alpha <- 0.05

kw_df <- data.frame(
  Test      = "Kruskal-Wallis",
  Statistic = round(kw_test$statistic, 4),
  DF        = kw_test$parameter,
  P_Value   = format(kw_test$p.value, digits = 6, scientific = TRUE),
  Decision  = ifelse(kw_test$p.value < alpha, "Reject $H_0$", "Fail to reject $H_0$")
)

write.csv(kw_df, file.path(output_dir, "tab_kw_results.csv"), row.names = FALSE)

sink(file.path(output_dir, "tab_kw_results.tex"))
cat(sprintf("Kruskal-Wallis & %.4f & %d & %s & %s \\\\\n",
            kw_test$statistic, kw_test$parameter,
            format(kw_test$p.value, digits = 6, scientific = TRUE),
            ifelse(kw_test$p.value < alpha, "Reject $H_0$", "Fail to reject $H_0$")))
sink()

cat("Written: tab_kw_results.csv / .tex\n")

################################################################################
# 6. TABLE: Pairwise Wilcoxon-Mann-Whitney (tab:pairwise)
################################################################################

promo_1 <- marketing_data$SalesInThousands[marketing_data$Treatment == "1"]
promo_2 <- marketing_data$SalesInThousands[marketing_data$Treatment == "2"]
promo_3 <- marketing_data$SalesInThousands[marketing_data$Treatment == "3"]

wmw_1vs2 <- wilcox.test(promo_1, promo_2, exact = FALSE, correct = TRUE)
wmw_1vs3 <- wilcox.test(promo_1, promo_3, exact = FALSE, correct = TRUE)
wmw_2vs3 <- wilcox.test(promo_2, promo_3, exact = FALSE, correct = TRUE)

raw_p <- c(wmw_1vs2$p.value, wmw_1vs3$p.value, wmw_2vs3$p.value)
bonf_p <- p.adjust(raw_p, method = "bonferroni")
holm_p <- p.adjust(raw_p, method = "holm")
labels <- c("Promo 1 vs 2", "Promo 1 vs 3", "Promo 2 vs 3")

pairwise_df <- data.frame(
  Comparison   = labels,
  Raw_P        = raw_p,
  Bonferroni_P = bonf_p,
  Holm_P       = holm_p,
  Bonf_Sig     = ifelse(bonf_p < alpha, "Yes", "No"),
  Holm_Sig     = ifelse(holm_p < alpha, "Yes", "No")
)

write.csv(pairwise_df, file.path(output_dir, "tab_pairwise.csv"), row.names = FALSE)

fmt_p <- function(p) {
  if (p < 0.0001) return(format(p, digits = 4, scientific = TRUE))
  return(sprintf("%.4f", p))
}

sink(file.path(output_dir, "tab_pairwise.tex"))
for (i in 1:nrow(pairwise_df)) {
  cat(sprintf("%s & %s & %s & %s & %s & %s \\\\\n",
              pairwise_df$Comparison[i],
              fmt_p(pairwise_df$Raw_P[i]),
              fmt_p(pairwise_df$Bonferroni_P[i]),
              fmt_p(pairwise_df$Holm_P[i]),
              pairwise_df$Bonf_Sig[i],
              pairwise_df$Holm_Sig[i]))
}
sink()

cat("Written: tab_pairwise.csv / .tex\n")

################################################################################
# 7. TABLE: Median comparisons (tab:median_comp)
################################################################################

med_1 <- median(promo_1)
med_2 <- median(promo_2)
med_3 <- median(promo_3)

median_comp <- data.frame(
  Comparison      = c("Promo 2 vs Promo 1", "Promo 3 vs Promo 1", "Promo 3 vs Promo 2"),
  Median_Diff     = round(c(med_2 - med_1, med_3 - med_1, med_3 - med_2), 2),
  Pct_Change      = round(c((med_2 - med_1)/med_1 * 100,
                             (med_3 - med_1)/med_1 * 100,
                             (med_3 - med_2)/med_2 * 100), 2)
)

write.csv(median_comp, file.path(output_dir, "tab_median_comp.csv"), row.names = FALSE)

sink(file.path(output_dir, "tab_median_comp.tex"))
for (i in 1:nrow(median_comp)) {
  cat(sprintf("%s & %+.2f & %+.2f\\%% \\\\\n",
              median_comp$Comparison[i],
              median_comp$Median_Diff[i],
              median_comp$Pct_Change[i]))
}
sink()

cat("Written: tab_median_comp.csv / .tex\n")

################################################################################
# 8. FIGURE: LOWESS curves (fig:lowess)
################################################################################

lowess_fits <- list()
for (treat in levels(marketing_data$Treatment)) {
  sub <- marketing_data[marketing_data$Treatment == treat, ]
  lfit <- lowess(sub$Week, sub$SalesInThousands, f = 0.5)
  lowess_fits[[treat]] <- data.frame(
    Treatment    = treat,
    Week         = lfit$x,
    Fitted_Sales = lfit$y
  )
}
lowess_combined <- do.call(rbind, lowess_fits)

p_lowess <- ggplot() +
  geom_point(data = marketing_data,
             aes(x = Week, y = SalesInThousands, color = Treatment),
             alpha = 0.3, size = 2) +
  geom_line(data = lowess_combined,
            aes(x = Week, y = Fitted_Sales, color = Treatment),
            linewidth = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "LOWESS Smoothing: Sales Trends by Promotion",
       subtitle = "Locally weighted regression with f = 0.5",
       x = "Week", y = "Sales (in Thousands)", color = "Promotion") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "right") +
  facet_wrap(~ Treatment, ncol = 3)

ggsave(file.path(output_dir, "plot_lowess.png"),
       plot = p_lowess, width = 12, height = 4, dpi = 300)
cat("Written: plot_lowess.png\n")

################################################################################
# 9. FIGURE: Smooth spline curves (fig:splines) +
#    TABLE: Spline parameters (tab:spline_params)
################################################################################

spline_fits   <- list()
spline_models <- list()

for (treat in levels(marketing_data$Treatment)) {
  sub <- marketing_data[marketing_data$Treatment == treat, ]
  sfit <- smooth.spline(sub$Week, sub$SalesInThousands, cv = TRUE)
  spline_models[[treat]] <- sfit

  pred_weeks <- seq(min(sub$Week), max(sub$Week), length.out = 100)
  preds <- predict(sfit, pred_weeks)
  spline_fits[[treat]] <- data.frame(
    Treatment    = treat,
    Week         = preds$x,
    Fitted_Sales = preds$y
  )
}
spline_combined <- do.call(rbind, spline_fits)

# Spline parameters table
spline_params <- data.frame(
  Treatment = names(spline_models),
  DF        = sapply(spline_models, function(m) round(m$df, 4)),
  Lambda    = sapply(spline_models, function(m) formatC(m$lambda, format = "e", digits = 4)),
  CV_Score  = sapply(spline_models, function(m) round(m$cv.crit, 4))
)

write.csv(spline_params, file.path(output_dir, "tab_spline_params.csv"), row.names = FALSE)

sink(file.path(output_dir, "tab_spline_params.tex"))
for (i in 1:nrow(spline_params)) {
  cat(sprintf("%s & %.4f & %s & %.4f \\\\\n",
              spline_params$Treatment[i],
              as.numeric(spline_params$DF[i]),
              spline_params$Lambda[i],
              as.numeric(spline_params$CV_Score[i])))
}
sink()

# Spline plot
p_spline <- ggplot() +
  geom_point(data = marketing_data,
             aes(x = Week, y = SalesInThousands, color = Treatment),
             alpha = 0.3, size = 2) +
  geom_line(data = spline_combined,
            aes(x = Week, y = Fitted_Sales, color = Treatment),
            linewidth = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Smooth Spline Fitting: Sales Trends by Promotion",
       subtitle = "Cross-validated smoothing parameter selection",
       x = "Week", y = "Sales (in Thousands)", color = "Promotion") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "right") +
  facet_wrap(~ Treatment, ncol = 3)

ggsave(file.path(output_dir, "plot_splines.png"),
       plot = p_spline, width = 12, height = 4, dpi = 300)
cat("Written: plot_splines.png, tab_spline_params.csv / .tex\n")

################################################################################
# 10. FIGURE: LOWESS vs Spline comparison (fig:comparison)
################################################################################

lowess_combined$Method <- "LOWESS"
spline_sub <- spline_combined %>%
  filter(Week %in% seq(min(Week), max(Week), length.out = 50))
spline_sub$Method <- "Smooth Spline"

combined_fits <- rbind(
  lowess_combined[, c("Treatment", "Week", "Fitted_Sales", "Method")],
  spline_sub[,      c("Treatment", "Week", "Fitted_Sales", "Method")]
)

p_comp <- ggplot() +
  geom_point(data = marketing_data,
             aes(x = Week, y = SalesInThousands),
             alpha = 0.2, size = 1.5, color = "gray30") +
  geom_line(data = combined_fits,
            aes(x = Week, y = Fitted_Sales, color = Method, linetype = Method),
            linewidth = 1) +
  scale_color_manual(values = c("LOWESS" = "#E41A1C", "Smooth Spline" = "#377EB8")) +
  scale_linetype_manual(values = c("LOWESS" = "solid", "Smooth Spline" = "dashed")) +
  labs(title = "Comparison: LOWESS vs Smooth Spline Fitting",
       subtitle = "Both methods capture non-linear trends in sales data",
       x = "Week", y = "Sales (in Thousands)", color = "Method", linetype = "Method") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom") +
  facet_wrap(~ Treatment, ncol = 3, scales = "free_y")

ggsave(file.path(output_dir, "plot_comparison.png"),
       plot = p_comp, width = 12, height = 4, dpi = 300)
cat("Written: plot_comparison.png\n")

################################################################################
# 11. TABLE: Model comparison metrics (tab:model_comparison)
################################################################################

calculate_gof <- function(observed, predicted, n_params = NULL) {
  n <- length(observed)
  res <- observed - predicted
  rmse <- sqrt(mean(res^2))
  mae  <- mean(abs(res))
  mape <- mean(abs(res / observed)) * 100
  ss_res <- sum(res^2)
  ss_tot <- sum((observed - mean(observed))^2)
  r2 <- 1 - ss_res / ss_tot
  aic <- NA; bic <- NA
  if (!is.null(n_params)) {
    ll <- -n/2 * log(2 * pi) - n/2 * log(ss_res / n) - n/2
    aic <- 2 * n_params - 2 * ll
    bic <- log(n) * n_params - 2 * ll
  }
  data.frame(RMSE = rmse, MAE = mae, MAPE = mape,
             R_squared = r2, AIC = aic, BIC = bic)
}

model_comp <- data.frame()

for (treat in levels(marketing_data$Treatment)) {
  sub <- marketing_data %>% filter(Treatment == treat) %>% arrange(Week)
  x <- sub$Week; y <- sub$SalesInThousands; n <- length(y)

  # LOWESS
  lfit <- lowess(x, y, f = 0.5)
  lowess_edf <- n / 0.5
  lg <- calculate_gof(y, lfit$y, lowess_edf)
  lg$Treatment <- treat; lg$Method <- "LOWESS"; lg$Eff_DF <- round(lowess_edf, 2)

  # Spline
  sfit <- smooth.spline(x, y, cv = TRUE)
  sp <- predict(sfit, x)$y
  sg <- calculate_gof(y, sp, sfit$df)
  sg$Treatment <- treat; sg$Method <- "Spline"; sg$Eff_DF <- round(sfit$df, 2)

  model_comp <- rbind(model_comp, lg, sg)
}

write.csv(model_comp, file.path(output_dir, "tab_model_comparison.csv"), row.names = FALSE)

sink(file.path(output_dir, "tab_model_comparison.tex"))
for (i in 1:nrow(model_comp)) {
  r <- model_comp[i, ]
  cat(sprintf("%s & %s & %.4f & %.4f & %.2f & %.4f & %.2f & %.2f \\\\\n",
              r$Treatment, r$Method, r$RMSE, r$MAE, r$MAPE,
              r$R_squared, r$AIC, r$BIC))
}
sink()

cat("Written: tab_model_comparison.csv / .tex\n")

################################################################################
# 12. TABLE: Cross-validation results (tab:cv_results)
################################################################################

perform_loocv <- function(x, y, method = "lowess", f = 0.5) {
  n <- length(x)
  preds <- numeric(n)
  for (i in 1:n) {
    xt <- x[-i]; yt <- y[-i]; xp <- x[i]
    if (method == "lowess") {
      fit <- lowess(xt, yt, f = f)
      preds[i] <- approx(fit$x, fit$y, xout = xp, rule = 2)$y
    } else {
      fit <- smooth.spline(xt, yt, cv = FALSE)
      preds[i] <- predict(fit, xp)$y
    }
  }
  preds
}

cv_results <- data.frame()
for (treat in levels(marketing_data$Treatment)) {
  sub <- marketing_data %>% filter(Treatment == treat) %>% arrange(Week)
  x <- sub$Week; y <- sub$SalesInThousands

  lp <- perform_loocv(x, y, "lowess", 0.5)
  sp <- perform_loocv(x, y, "spline")

  cv_results <- rbind(cv_results, data.frame(
    Treatment = treat,
    Method    = c("LOWESS", "Spline"),
    CV_RMSE   = c(sqrt(mean((y - lp)^2)), sqrt(mean((y - sp)^2))),
    CV_MAE    = c(mean(abs(y - lp)),       mean(abs(y - sp)))
  ))
}

write.csv(cv_results, file.path(output_dir, "tab_cv_results.csv"), row.names = FALSE)

sink(file.path(output_dir, "tab_cv_results.tex"))
for (i in 1:nrow(cv_results)) {
  r <- cv_results[i, ]
  cat(sprintf("%s & %s & %.4f & %.4f \\\\\n",
              r$Treatment, r$Method, r$CV_RMSE, r$CV_MAE))
}
sink()

cat("Written: tab_cv_results.csv / .tex\n")

################################################################################
# 13. FIGURE: Prediction comparison (fig:predictions)
################################################################################

new_weeks <- seq(1, 6, by = 0.5)
all_preds <- data.frame()

for (treat in levels(marketing_data$Treatment)) {
  sub <- marketing_data %>% filter(Treatment == treat) %>% arrange(Week)
  x <- sub$Week; y <- sub$SalesInThousands
  dr <- range(x)

  lfit <- lowess(x, y, f = 0.5)
  lp   <- approx(lfit$x, lfit$y, xout = new_weeks, method = "linear", rule = 2)$y

  sfit <- smooth.spline(x, y, cv = TRUE)
  sp   <- predict(sfit, new_weeks)$y

  all_preds <- rbind(all_preds, data.frame(
    Treatment         = treat,
    Week              = new_weeks,
    LOWESS_Prediction = lp,
    Spline_Prediction = sp,
    Extrapolated      = new_weeks < dr[1] | new_weeks > dr[2]
  ))
}

p_pred <- ggplot() +
  geom_point(data = marketing_data,
             aes(x = Week, y = SalesInThousands),
             alpha = 0.4, size = 2, color = "gray40") +
  geom_line(data = all_preds,
            aes(x = Week, y = LOWESS_Prediction, color = "LOWESS"),
            linetype = "solid", linewidth = 1) +
  geom_line(data = all_preds,
            aes(x = Week, y = Spline_Prediction, color = "Spline"),
            linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dotted", alpha = 0.5) +
  geom_vline(xintercept = 4, linetype = "dotted", alpha = 0.5) +
  scale_color_manual(values = c("LOWESS" = "#E41A1C", "Spline" = "#377EB8")) +
  facet_wrap(~ Treatment, ncol = 3) +
  labs(title    = "Model Predictions: LOWESS vs Smooth Spline",
       subtitle = "Dotted lines indicate original data range; beyond = extrapolation",
       x = "Week", y = "Predicted Sales (in Thousands)", color = "Method") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

ggsave(file.path(output_dir, "prediction_comparison.png"),
       plot = p_pred, width = 12, height = 5, dpi = 300)
cat("Written: prediction_comparison.png\n")

################################################################################
# 14. SUMMARY: Print all table values to console for quick reference
################################################################################

cat("\n========== REPORT VALUES FOR COPY-PASTE ==========\n")

cat("\n--- Summary Stats (Table 2) ---\n")
print(summary_stats, row.names = FALSE)

cat("\n--- Kruskal-Wallis (Table 4) ---\n")
cat(sprintf("  Chi-sq = %.4f, df = %d, p = %s\n",
            kw_test$statistic, kw_test$parameter,
            format(kw_test$p.value, digits = 6)))
cat(sprintf("  Decision: %s\n",
            ifelse(kw_test$p.value < alpha, "Reject H0", "Fail to reject H0")))

cat("\n--- Pairwise Comparisons (Table 5) ---\n")
print(pairwise_df, row.names = FALSE)

cat("\n--- Median Comparisons (Table 6) ---\n")
cat(sprintf("  Median Promo 1: %.2f\n", med_1))
cat(sprintf("  Median Promo 2: %.2f\n", med_2))
cat(sprintf("  Median Promo 3: %.2f\n", med_3))
print(median_comp, row.names = FALSE)

cat("\n--- Spline Parameters (Table 7) ---\n")
print(spline_params, row.names = FALSE)

cat("\n--- Model Comparison (Table 8) ---\n")
print(model_comp[, c("Treatment", "Method", "RMSE", "MAE", "R_squared", "AIC", "BIC")],
      row.names = FALSE, digits = 4)

cat("\n--- Cross-Validation (Table 9) ---\n")
print(cv_results, row.names = FALSE, digits = 4)

cat("\n===================================================\n")
cat("All outputs saved to:", normalizePath(output_dir), "\n")
cat("Files generated:\n")
cat(paste(" ", list.files(output_dir), collapse = "\n"), "\n")
