
################################################################################
# Fast Food Marketing Campaign Analysis
# Non-Parametric Statistical Testing and Curve Fitting
################################################################################
# Project Overview:
# This script performs non-parametric statistical analysis on a marketing
# campaign dataset with three different promotional strategies across multiple
# store locations. The analysis includes:
# 1. Kruskal-Wallis H-test for overall group differences
# 2. Post-hoc pairwise comparisons with multiple testing corrections
# 3. Sign tests for directional verification
# 4. Non-parametric curve fitting (LOWESS and Smooth Splines)
################################################################################

# Clear workspace and set working directory
rm(list = ls())
cat("\n=== Fast Food Marketing Campaign Analysis ===\n")

################################################################################
# 1. LOAD REQUIRED LIBRARIES
################################################################################
# Install packages if needed (uncomment if first time running)
# install.packages(c("ggplot2", "dplyr", "tidyr", "BSDA", "gridExtra", "RColorBrewer"))

library(ggplot2)      # For advanced visualization
library(dplyr)        # For data manipulation
library(tidyr)        # For data reshaping
library(BSDA)         # For Sign Test
library(gridExtra)    # For arranging multiple plots
library(RColorBrewer) # For color palettes

cat("✓ All required libraries loaded successfully\n")

################################################################################
# 2. DATA LOADING AND PREPARATION
################################################################################
# NOTE: Replace 'your_dataset.csv' with your actual CSV filename
# Expected columns: LocationID, Treatment, SalesInThousands, MarketSize, 
#                   AgeOfStore, Week (or similar time identifier)

# For demonstration purposes, we'll generate sample data that matches your structure
# COMMENT OUT THIS SECTION when using your actual dataset
set.seed(42)  # For reproducibility
generate_sample_data <- function() {
  n_locations <- 30  # 10 locations per promotion
  weeks <- 4
  
  data <- expand.grid(
    LocationID = 1:n_locations,
    Week = 1:weeks
  ) %>%
    mutate(
      # Assign treatments: evenly distributed across locations
      Treatment = rep(c(1, 2, 3), each = (n_locations/3) * weeks),
      # Market size: categorical variable (Small, Medium, Large)
      MarketSize = sample(c("Small", "Medium", "Large"), n(), replace = TRUE),
      # Age of store: continuous variable (1-20 years)
      AgeOfStore = rep(sample(1:20, n_locations, replace = TRUE), each = weeks),
      # Sales generation with treatment effects
      # Promotion 1: baseline (mean ~50)
      # Promotion 2: moderate improvement (mean ~55)
      # Promotion 3: strong improvement (mean ~62)
      SalesInThousands = case_when(
        Treatment == 1 ~ rnorm(n(), 50 + AgeOfStore * 0.5, 8),
        Treatment == 2 ~ rnorm(n(), 55 + AgeOfStore * 0.5, 8),
        Treatment == 3 ~ rnorm(n(), 62 + AgeOfStore * 0.5, 8)
      )
    ) %>%
    mutate(Treatment = factor(Treatment, levels = c(1, 2, 3)))
  
  return(data)
}

# Generate sample data
marketing_data <- generate_sample_data()

# UNCOMMENT THE FOLLOWING LINE to load your actual CSV file:
marketing_data <- read.csv("WA_Marketing-Campaign.csv", header = TRUE)

# Ensure Treatment is a factor
marketing_data$Treatment <- as.factor(marketing_data$Treatment)

cat("✓ Data loaded successfully\n")
cat(sprintf("  - Total observations: %d\n", nrow(marketing_data)))
cat(sprintf("  - Unique locations: %d\n", length(unique(marketing_data$LocationID))))
cat(sprintf("  - Treatments: %s\n", paste(levels(marketing_data$Treatment), collapse = ", ")))

################################################################################
# 3. EXPLORATORY DATA ANALYSIS
################################################################################
cat("\n=== Exploratory Data Analysis ===\n")

# Summary statistics by treatment group
summary_stats <- marketing_data %>%
  group_by(Treatment) %>%
  summarise(
    N = n(),
    Mean = mean(SalesInThousands),
    Median = median(SalesInThousands),
    SD = sd(SalesInThousands),
    Min = min(SalesInThousands),
    Max = max(SalesInThousands),
    Q1 = quantile(SalesInThousands, 0.25),
    Q3 = quantile(SalesInThousands, 0.75)
  )

print(summary_stats)

# Visualization 1: Box plots comparing promotions
p1 <- ggplot(marketing_data, aes(x = Treatment, y = SalesInThousands, fill = Treatment)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Sales Distribution by Promotion Type",
    subtitle = "Box plots with individual data points",
    x = "Promotion Type",
    y = "Sales (in Thousands)",
    caption = "Jittered points show individual observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

# Visualization 2: Violin plots with medians
p2 <- ggplot(marketing_data, aes(x = Treatment, y = SalesInThousands, fill = Treatment)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Sales Distribution Density by Promotion",
    subtitle = "Violin plots showing distribution shape",
    x = "Promotion Type",
    y = "Sales (in Thousands)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

# Display plots
print(p1)
print(p2)

################################################################################
# 4. NON-PARAMETRIC TESTING: KRUSKAL-WALLIS H-TEST
################################################################################
cat("\n=== Kruskal-Wallis H-Test ===\n")
cat("Null Hypothesis (H0): All three promotions have identical distributions\n")
cat("Alternative Hypothesis (H1): At least one promotion differs\n\n")

# Perform Kruskal-Wallis test
# This is a non-parametric alternative to one-way ANOVA
# Tests whether samples originate from the same distribution
kw_test <- kruskal.test(SalesInThousands ~ Treatment, data = marketing_data)

cat("Kruskal-Wallis Test Results:\n")
cat(sprintf("  Chi-squared statistic: %.4f\n", kw_test$statistic))
cat(sprintf("  Degrees of freedom: %d\n", kw_test$parameter))
cat(sprintf("  P-value: %.6f\n", kw_test$p.value))

# Interpret results
alpha <- 0.05
if (kw_test$p.value < alpha) {
  cat(sprintf("\n✓ SIGNIFICANT RESULT (p < %.2f)\n", alpha))
  cat("  We reject the null hypothesis.\n")
  cat("  There are significant differences among the three promotions.\n")
  cat("  Proceeding with post-hoc pairwise comparisons...\n")
  perform_posthoc <- TRUE
} else {
  cat(sprintf("\n✗ NON-SIGNIFICANT RESULT (p >= %.2f)\n", alpha))
  cat("  We fail to reject the null hypothesis.\n")
  cat("  No significant differences detected among promotions.\n")
  perform_posthoc <- FALSE
}

################################################################################
# 5. POST-HOC PAIRWISE COMPARISONS
################################################################################
if (perform_posthoc) {
  cat("\n=== Post-Hoc Pairwise Comparisons ===\n")
  
  # Extract data by treatment groups
  promo_1 <- marketing_data$SalesInThousands[marketing_data$Treatment == "1"]
  promo_2 <- marketing_data$SalesInThousands[marketing_data$Treatment == "2"]
  promo_3 <- marketing_data$SalesInThousands[marketing_data$Treatment == "3"]
  
  # -------------------------------------------------------------------------
  # 5.1 Wilcoxon-Mann-Whitney Tests (Rank-Sum Tests)
  # -------------------------------------------------------------------------
  cat("\n--- Wilcoxon-Mann-Whitney U Tests ---\n")
  cat("Tests whether two independent samples have different distributions\n\n")
  
  # Perform three pairwise comparisons
  wmw_1vs2 <- wilcox.test(promo_1, promo_2, exact = FALSE, correct = TRUE)
  wmw_1vs3 <- wilcox.test(promo_1, promo_3, exact = FALSE, correct = TRUE)
  wmw_2vs3 <- wilcox.test(promo_2, promo_3, exact = FALSE, correct = TRUE)
  
  # Store raw p-values
  raw_pvalues <- c(
    "Promo 1 vs 2" = wmw_1vs2$p.value,
    "Promo 1 vs 3" = wmw_1vs3$p.value,
    "Promo 2 vs 3" = wmw_2vs3$p.value
  )
  
  cat("Raw p-values (uncorrected):\n")
  print(raw_pvalues)
  
  # -------------------------------------------------------------------------
  # 5.2 Multiple Testing Corrections
  # -------------------------------------------------------------------------
  cat("\n--- Multiple Testing Corrections ---\n")
  
  # Bonferroni correction: most conservative
  # Adjusts alpha by dividing by number of comparisons
  bonferroni_pvalues <- p.adjust(raw_pvalues, method = "bonferroni")
  
  cat("\nBonferroni-corrected p-values:\n")
  print(bonferroni_pvalues)
  
  # Holm correction: less conservative, controls family-wise error rate
  # Sequentially adjusts p-values based on rank
  holm_pvalues <- p.adjust(raw_pvalues, method = "holm")
  
  cat("\nHolm-corrected p-values:\n")
  print(holm_pvalues)
  
  # Create comparison table
  comparison_results <- data.frame(
    Comparison = names(raw_pvalues),
    Raw_P = raw_pvalues,
    Bonferroni_P = bonferroni_pvalues,
    Holm_P = holm_pvalues,
    Bonf_Sig = ifelse(bonferroni_pvalues < alpha, "Yes", "No"),
    Holm_Sig = ifelse(holm_pvalues < alpha, "Yes", "No")
  )
  
  cat("\n--- Summary of Pairwise Comparisons ---\n")
  print(comparison_results, row.names = FALSE)
  
  # -------------------------------------------------------------------------
  # 5.3 Sign Tests for Directional Verification
  # -------------------------------------------------------------------------
  cat("\n=== Sign Tests for Directional Verification ===\n")
  cat("Tests the direction of differences between paired/matched samples\n\n")
  
  # Note: Sign test requires paired data or matching
  # For unpaired data, we'll use median differences as a simplified approach
  # In practice, you might need actual paired observations
  
  cat("Computing median differences for directional analysis:\n")
  median_1 <- median(promo_1)
  median_2 <- median(promo_2)
  median_3 <- median(promo_3)
  
  cat(sprintf("  Median Sales (Promo 1): %.2f\n", median_1))
  cat(sprintf("  Median Sales (Promo 2): %.2f\n", median_2))
  cat(sprintf("  Median Sales (Promo 3): %.2f\n", median_3))
  cat("\n")
  cat(sprintf("  Promo 2 vs Promo 1: %+.2f (%.1f%% change)\n", 
              median_2 - median_1, ((median_2 - median_1)/median_1)*100))
  cat(sprintf("  Promo 3 vs Promo 1: %+.2f (%.1f%% change)\n", 
              median_3 - median_1, ((median_3 - median_1)/median_1)*100))
  cat(sprintf("  Promo 3 vs Promo 2: %+.2f (%.1f%% change)\n", 
              median_3 - median_2, ((median_3 - median_2)/median_2)*100))
  
  # For true sign test with paired data (if applicable)
  # Uncomment and modify if you have matched pairs:
  # sign_test_1vs2 <- SIGN.test(promo_1_paired, promo_2_paired)
  # print(sign_test_1vs2)
}

################################################################################
# 6. NON-PARAMETRIC CURVE FITTING
################################################################################
cat("\n=== Non-Parametric Curve Fitting ===\n")

# Prepare data for curve fitting: aggregate by treatment and time
# Calculate mean sales per week per treatment
curve_data <- marketing_data %>%
  group_by(Treatment, Week) %>%
  summarise(
    Mean_Sales = mean(SalesInThousands),
    Median_Sales = median(SalesInThousands),
    SD_Sales = sd(SalesInThousands),
    N = n(),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# 6.1 LOWESS (Locally Weighted Scatterplot Smoothing)
# -------------------------------------------------------------------------
cat("\n--- LOWESS Curve Fitting ---\n")
cat("LOWESS: Locally Weighted Scatterplot Smoothing\n")
cat("  - Non-parametric regression technique\n")
cat("  - Fits simple models to localized subsets of data\n")
cat("  - Builds up a function point by point\n")
cat("  - Robust to outliers\n\n")

# Apply LOWESS for each treatment group
lowess_fits <- list()
for (treat in levels(marketing_data$Treatment)) {
  subset_data <- marketing_data[marketing_data$Treatment == treat, ]
  # f parameter controls smoothing span (0.5-0.75 typical)
  lowess_fit <- lowess(subset_data$Week, subset_data$SalesInThousands, f = 0.5)
  lowess_fits[[treat]] <- data.frame(
    Treatment = treat,
    Week = lowess_fit$x,
    Fitted_Sales = lowess_fit$y
  )
}

lowess_combined <- do.call(rbind, lowess_fits)

# Visualization: LOWESS curves
p3 <- ggplot() +
  geom_point(data = marketing_data, 
             aes(x = Week, y = SalesInThousands, color = Treatment),
             alpha = 0.3, size = 2) +
  geom_line(data = lowess_combined,
            aes(x = Week, y = Fitted_Sales, color = Treatment),
            size = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "LOWESS Smoothing: Sales Trends by Promotion",
    subtitle = "Locally weighted regression with f = 0.5",
    x = "Week",
    y = "Sales (in Thousands)",
    color = "Promotion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  ) +
  facet_wrap(~ Treatment, ncol = 3)

print(p3)

# -------------------------------------------------------------------------
# 6.2 Smooth Splines
# -------------------------------------------------------------------------
cat("\n--- Smooth Spline Fitting ---\n")
cat("Smooth Splines: Piecewise polynomial regression\n")
cat("  - Minimizes penalized residual sum of squares\n")
cat("  - Automatic selection of smoothing parameter via cross-validation\n")
cat("  - Produces smooth, continuous curves\n\n")

# Apply smooth splines for each treatment group
spline_fits <- list()
for (treat in levels(marketing_data$Treatment)) {
  subset_data <- marketing_data[marketing_data$Treatment == treat, ]
  # smooth.spline with automatic lambda selection via CV
  spline_fit <- smooth.spline(subset_data$Week, subset_data$SalesInThousands, cv = TRUE)
  
  cat(sprintf("Spline fit for Promotion %s:\n", treat))
  cat(sprintf("  - Degrees of freedom: %.2f\n", spline_fit$df))
  cat(sprintf("  - Smoothing parameter (lambda): %.6f\n", spline_fit$lambda))
  
  # Generate predictions on a fine grid
  pred_weeks <- seq(min(subset_data$Week), max(subset_data$Week), length.out = 100)
  predictions <- predict(spline_fit, pred_weeks)
  
  spline_fits[[treat]] <- data.frame(
    Treatment = treat,
    Week = predictions$x,
    Fitted_Sales = predictions$y
  )
}

spline_combined <- do.call(rbind, spline_fits)

# Visualization: Smooth splines
p4 <- ggplot() +
  geom_point(data = marketing_data, 
             aes(x = Week, y = SalesInThousands, color = Treatment),
             alpha = 0.3, size = 2) +
  geom_line(data = spline_combined,
            aes(x = Week, y = Fitted_Sales, color = Treatment),
            size = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Smooth Spline Fitting: Sales Trends by Promotion",
    subtitle = "Cross-validated smoothing parameter selection",
    x = "Week",
    y = "Sales (in Thousands)",
    color = "Promotion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  ) +
  facet_wrap(~ Treatment, ncol = 3)

print(p4)

# -------------------------------------------------------------------------
# 6.3 Comparison: LOWESS vs Smooth Splines
# -------------------------------------------------------------------------
cat("\n--- Comparing LOWESS and Smooth Splines ---\n")

# Combined visualization
lowess_combined$Method <- "LOWESS"
spline_subset <- spline_combined %>%
  filter(Week %in% seq(min(Week), max(Week), length.out = 50))
spline_subset$Method <- "Smooth Spline"

combined_fits <- rbind(
  lowess_combined[, c("Treatment", "Week", "Fitted_Sales", "Method")],
  spline_subset[, c("Treatment", "Week", "Fitted_Sales", "Method")]
)

p5 <- ggplot() +
  geom_point(data = marketing_data, 
             aes(x = Week, y = SalesInThousands),
             alpha = 0.2, size = 1.5, color = "gray30") +
  geom_line(data = combined_fits,
            aes(x = Week, y = Fitted_Sales, color = Method, linetype = Method),
            size = 1) +
  scale_color_manual(values = c("LOWESS" = "#E41A1C", "Smooth Spline" = "#377EB8")) +
  scale_linetype_manual(values = c("LOWESS" = "solid", "Smooth Spline" = "dashed")) +
  labs(
    title = "Comparison: LOWESS vs Smooth Spline Fitting",
    subtitle = "Both methods capture non-linear trends in sales data",
    x = "Week",
    y = "Sales (in Thousands)",
    color = "Method",
    linetype = "Method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  ) +
  facet_wrap(~ Treatment, ncol = 3, scales = "free_y")

print(p5)

################################################################################
# 7. SAVE RESULTS
################################################################################
cat("\n=== Saving Results ===\n")

# Save summary statistics
write.csv(summary_stats, "summary_statistics.csv", row.names = FALSE)
cat("✓ Summary statistics saved to 'summary_statistics.csv'\n")

# Save Kruskal-Wallis results
kw_results <- data.frame(
  Test = "Kruskal-Wallis",
  Statistic = kw_test$statistic,
  DF = kw_test$parameter,
  P_Value = kw_test$p.value,
  Significant = ifelse(kw_test$p.value < alpha, "Yes", "No")
)
write.csv(kw_results, "kruskal_wallis_results.csv", row.names = FALSE)
cat("✓ Kruskal-Wallis results saved to 'kruskal_wallis_results.csv'\n")

# Save pairwise comparison results (if performed)
if (perform_posthoc) {
  write.csv(comparison_results, "pairwise_comparisons.csv", row.names = FALSE)
  cat("✓ Pairwise comparisons saved to 'pairwise_comparisons.csv'\n")
}

# Save curve fitting data
write.csv(lowess_combined, "lowess_fits.csv", row.names = FALSE)
write.csv(spline_combined, "spline_fits.csv", row.names = FALSE)
cat("✓ LOWESS fits saved to 'lowess_fits.csv'\n")
cat("✓ Spline fits saved to 'spline_fits.csv'\n")

# Save plots
ggsave("plot_boxplots.png", plot = p1, width = 10, height = 6, dpi = 300)
ggsave("plot_violins.png", plot = p2, width = 10, height = 6, dpi = 300)
ggsave("plot_lowess.png", plot = p3, width = 12, height = 4, dpi = 300)
ggsave("plot_splines.png", plot = p4, width = 12, height = 4, dpi = 300)
ggsave("plot_comparison.png", plot = p5, width = 12, height = 4, dpi = 300)
cat("✓ All plots saved as PNG files\n")

################################################################################
# 8. FINAL SUMMARY
################################################################################
cat("\n=== Analysis Complete ===\n")
cat("\nKey Findings:\n")
cat(sprintf("1. Kruskal-Wallis Test: %s (p = %.4f)\n",
            ifelse(kw_test$p.value < alpha, "SIGNIFICANT", "NOT SIGNIFICANT"),
            kw_test$p.value))

if (perform_posthoc) {
  sig_comparisons <- sum(comparison_results$Holm_Sig == "Yes")
  cat(sprintf("2. Significant pairwise differences (Holm-corrected): %d out of 3\n", 
              sig_comparisons))
  cat("3. Non-parametric curves successfully fitted using LOWESS and Smooth Splines\n")
}

cat("\nAll results and visualizations have been saved to the working directory.\n")
cat("\n" , rep("=", 70), "\n", sep = "")
