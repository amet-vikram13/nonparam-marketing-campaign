
################################################################################
# MODEL COMPARISON & PREDICTION FRAMEWORK
# Comparing LOWESS vs Smooth Splines with Numerical Metrics
################################################################################

# This code extends the main analysis to:
# 1. Numerically compare LOWESS and Smooth Splines
# 2. Select the best model based on performance metrics
# 3. Extract model information/coefficients
# 4. Make predictions on new data

################################################################################
# PART 1: GOODNESS-OF-FIT METRICS
################################################################################

cat("\n=== MODEL COMPARISON: LOWESS vs SMOOTH SPLINES ===\n")

# Function to calculate multiple goodness-of-fit metrics
calculate_gof_metrics <- function(observed, predicted, n_params = NULL) {
  # observed: actual values
  # predicted: fitted values
  # n_params: number of parameters (for AIC/BIC)

  n <- length(observed)
  residuals <- observed - predicted

  # 1. Root Mean Squared Error (RMSE)
  # Lower is better; same units as response variable
  rmse <- sqrt(mean(residuals^2))

  # 2. Mean Absolute Error (MAE)
  # More robust to outliers than RMSE
  mae <- mean(abs(residuals))

  # 3. Mean Absolute Percentage Error (MAPE)
  # Scale-independent metric (percentage)
  mape <- mean(abs(residuals / observed)) * 100

  # 4. R-squared (coefficient of determination)
  # Proportion of variance explained (0 to 1, higher is better)
  ss_res <- sum(residuals^2)
  ss_tot <- sum((observed - mean(observed))^2)
  r_squared <- 1 - (ss_res / ss_tot)

  # 5. Adjusted R-squared (if n_params provided)
  # Penalizes for number of parameters
  adj_r_squared <- NA
  if (!is.null(n_params) && n_params < n) {
    adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - n_params - 1))
  }

  # 6. Akaike Information Criterion (AIC)
  # Lower is better; balances fit and complexity
  aic <- NA
  if (!is.null(n_params)) {
    log_likelihood <- -n/2 * log(2 * pi) - n/2 * log(ss_res/n) - n/2
    aic <- 2 * n_params - 2 * log_likelihood
  }

  # 7. Bayesian Information Criterion (BIC)
  # More stringent penalty for parameters than AIC
  bic <- NA
  if (!is.null(n_params)) {
    log_likelihood <- -n/2 * log(2 * pi) - n/2 * log(ss_res/n) - n/2
    bic <- log(n) * n_params - 2 * log_likelihood
  }

  return(list(
    RMSE = rmse,
    MAE = mae,
    MAPE = mape,
    R_squared = r_squared,
    Adj_R_squared = adj_r_squared,
    AIC = aic,
    BIC = bic,
    n = n,
    n_params = n_params
  ))
}

################################################################################
# PART 2: FIT MODELS AND CALCULATE METRICS FOR EACH TREATMENT
################################################################################

# Initialize storage for results
comparison_results <- list()
model_storage <- list()  # Store fitted models for later use

for (treat in levels(marketing_data$Treatment)) {
  cat(sprintf("\n--- Analyzing Promotion %s ---\n", treat))

  # Extract treatment-specific data
  subset_data <- marketing_data %>%
    filter(Treatment == treat) %>%
    arrange(Week)

  x_data <- subset_data$Week
  y_data <- subset_data$SalesInThousands
  n_obs <- length(y_data)

  # -------------------------------------------------------------------------
  # FIT LOWESS MODEL
  # -------------------------------------------------------------------------
  cat("  Fitting LOWESS model...\n")

  # Fit LOWESS with span parameter f
  lowess_f <- 0.5  # Smoothing parameter (adjust if needed)
  lowess_model <- lowess(x_data, y_data, f = lowess_f)

  # Get fitted values (LOWESS returns smoothed y values)
  lowess_fitted <- lowess_model$y

  # Effective degrees of freedom for LOWESS (approximation)
  # More smoothing (higher f) = fewer effective parameters
  lowess_edf <- n_obs / lowess_f  # Rough approximation

  # Calculate metrics for LOWESS
  lowess_metrics <- calculate_gof_metrics(
    observed = y_data,
    predicted = lowess_fitted,
    n_params = lowess_edf
  )

  cat(sprintf("  LOWESS - RMSE: %.4f, R²: %.4f\n", 
              lowess_metrics$RMSE, lowess_metrics$R_squared))

  # -------------------------------------------------------------------------
  # FIT SMOOTH SPLINE MODEL
  # -------------------------------------------------------------------------
  cat("  Fitting Smooth Spline model...\n")

  # Fit smooth spline with cross-validation
  spline_model <- smooth.spline(x_data, y_data, cv = TRUE)

  # Get fitted values
  spline_fitted <- predict(spline_model, x_data)$y

  # Degrees of freedom (automatically selected by CV)
  spline_df <- spline_model$df

  # Calculate metrics for Smooth Spline
  spline_metrics <- calculate_gof_metrics(
    observed = y_data,
    predicted = spline_fitted,
    n_params = spline_df
  )

  cat(sprintf("  Spline - RMSE: %.4f, R²: %.4f, DF: %.2f\n", 
              spline_metrics$RMSE, spline_metrics$R_squared, spline_df))

  # -------------------------------------------------------------------------
  # STORE RESULTS
  # -------------------------------------------------------------------------
  comparison_results[[treat]] <- list(
    Treatment = treat,
    LOWESS = lowess_metrics,
    Spline = spline_metrics,
    n_observations = n_obs
  )

  # Store models for later prediction
  model_storage[[treat]] <- list(
    lowess = lowess_model,
    spline = spline_model,
    data_range = range(x_data)  # Important for extrapolation warnings
  )
}

################################################################################
# PART 3: CREATE COMPARISON TABLE
################################################################################

cat("\n=== COMPREHENSIVE MODEL COMPARISON ===\n\n")

# Build comparison data frame
comparison_df <- data.frame()

for (treat in names(comparison_results)) {
  res <- comparison_results[[treat]]

  # LOWESS row
  lowess_row <- data.frame(
    Treatment = treat,
    Method = "LOWESS",
    RMSE = res$LOWESS$RMSE,
    MAE = res$LOWESS$MAE,
    MAPE = res$LOWESS$MAPE,
    R_squared = res$LOWESS$R_squared,
    Adj_R_squared = res$LOWESS$Adj_R_squared,
    AIC = res$LOWESS$AIC,
    BIC = res$LOWESS$BIC,
    Eff_DF = res$LOWESS$n_params
  )

  # Spline row
  spline_row <- data.frame(
    Treatment = treat,
    Method = "Spline",
    RMSE = res$Spline$RMSE,
    MAE = res$Spline$MAE,
    MAPE = res$Spline$MAPE,
    R_squared = res$Spline$R_squared,
    Adj_R_squared = res$Spline$Adj_R_squared,
    AIC = res$Spline$AIC,
    BIC = res$Spline$BIC,
    Eff_DF = res$Spline$n_params
  )

  comparison_df <- rbind(comparison_df, lowess_row, spline_row)
}

# Display comparison table
print(comparison_df, digits = 4, row.names = FALSE)

# Save comparison table
write.csv(comparison_df, "model_comparison_metrics.csv", row.names = FALSE)
cat("\n✓ Comparison table saved to 'model_comparison_metrics.csv'\n")

################################################################################
# PART 4: STATISTICAL COMPARISON (Cross-Validation)
################################################################################

cat("\n=== CROSS-VALIDATION COMPARISON ===\n")
cat("Performing Leave-One-Out Cross-Validation (LOOCV)...\n\n")

# Function for LOOCV
perform_loocv <- function(x, y, method = "lowess", f = 0.5) {
  n <- length(x)
  predictions <- numeric(n)

  for (i in 1:n) {
    # Leave out observation i
    x_train <- x[-i]
    y_train <- y[-i]
    x_test <- x[i]

    if (method == "lowess") {
      # Fit LOWESS on training data
      fit <- lowess(x_train, y_train, f = f)
      # Predict for test point using linear interpolation
      predictions[i] <- approx(fit$x, fit$y, xout = x_test, rule = 2)$y
    } else if (method == "spline") {
      # Fit spline on training data
      fit <- smooth.spline(x_train, y_train, cv = FALSE, spar = NULL)
      # Predict for test point
      predictions[i] <- predict(fit, x_test)$y
    }
  }

  return(predictions)
}

# Perform LOOCV for each treatment
cv_results <- data.frame()

for (treat in levels(marketing_data$Treatment)) {
  subset_data <- marketing_data %>%
    filter(Treatment == treat) %>%
    arrange(Week)

  x_data <- subset_data$Week
  y_data <- subset_data$SalesInThousands

  cat(sprintf("Treatment %s:\n", treat))

  # LOWESS CV
  lowess_cv_pred <- perform_loocv(x_data, y_data, method = "lowess", f = 0.5)
  lowess_cv_rmse <- sqrt(mean((y_data - lowess_cv_pred)^2))
  lowess_cv_mae <- mean(abs(y_data - lowess_cv_pred))

  cat(sprintf("  LOWESS CV-RMSE: %.4f, CV-MAE: %.4f\n", 
              lowess_cv_rmse, lowess_cv_mae))

  # Spline CV
  spline_cv_pred <- perform_loocv(x_data, y_data, method = "spline")
  spline_cv_rmse <- sqrt(mean((y_data - spline_cv_pred)^2))
  spline_cv_mae <- mean(abs(y_data - spline_cv_pred))

  cat(sprintf("  Spline CV-RMSE: %.4f, CV-MAE: %.4f\n", 
              spline_cv_rmse, spline_cv_mae))

  # Store results
  cv_results <- rbind(cv_results, data.frame(
    Treatment = treat,
    Method = c("LOWESS", "Spline"),
    CV_RMSE = c(lowess_cv_rmse, spline_cv_rmse),
    CV_MAE = c(lowess_cv_mae, spline_cv_mae)
  ))

  # Determine winner
  winner <- ifelse(lowess_cv_rmse < spline_cv_rmse, "LOWESS", "Spline")
  cat(sprintf("  → Winner: %s (lower CV-RMSE)\n\n", winner))
}

# Save CV results
write.csv(cv_results, "cross_validation_results.csv", row.names = FALSE)
cat("✓ CV results saved to 'cross_validation_results.csv'\n")

################################################################################
# PART 5: OVERALL MODEL SELECTION
################################################################################

cat("\n=== OVERALL MODEL SELECTION ===\n\n")

# Aggregate metrics across all treatments
aggregate_metrics <- comparison_df %>%
  group_by(Method) %>%
  summarise(
    Avg_RMSE = mean(RMSE, na.rm = TRUE),
    Avg_MAE = mean(MAE, na.rm = TRUE),
    Avg_R_squared = mean(R_squared, na.rm = TRUE),
    Avg_AIC = mean(AIC, na.rm = TRUE),
    Avg_BIC = mean(BIC, na.rm = TRUE),
    .groups = "drop"
  )

print(aggregate_metrics)

# Determine best method based on multiple criteria
lowess_wins <- sum(
  aggregate_metrics[aggregate_metrics$Method == "LOWESS", "Avg_RMSE"] < 
  aggregate_metrics[aggregate_metrics$Method == "Spline", "Avg_RMSE"],
  aggregate_metrics[aggregate_metrics$Method == "LOWESS", "Avg_MAE"] < 
  aggregate_metrics[aggregate_metrics$Method == "Spline", "Avg_MAE"],
  aggregate_metrics[aggregate_metrics$Method == "LOWESS", "Avg_R_squared"] > 
  aggregate_metrics[aggregate_metrics$Method == "Spline", "Avg_R_squared"]
)

spline_wins <- sum(
  aggregate_metrics[aggregate_metrics$Method == "Spline", "Avg_RMSE"] < 
  aggregate_metrics[aggregate_metrics$Method == "LOWESS", "Avg_RMSE"],
  aggregate_metrics[aggregate_metrics$Method == "Spline", "Avg_MAE"] < 
  aggregate_metrics[aggregate_metrics$Method == "LOWESS", "Avg_MAE"],
  aggregate_metrics[aggregate_metrics$Method == "Spline", "Avg_R_squared"] > 
  aggregate_metrics[aggregate_metrics$Method == "LOWESS", "Avg_R_squared"]
)

cat("\nModel Selection Decision:\n")
cat(sprintf("  LOWESS wins on %d/3 key metrics (RMSE, MAE, R²)\n", lowess_wins))
cat(sprintf("  Spline wins on %d/3 key metrics (RMSE, MAE, R²)\n", spline_wins))

if (spline_wins > lowess_wins) {
  selected_method <- "Spline"
  cat("\n✓ RECOMMENDED METHOD: Smooth Spline\n")
  cat("  Reasons: Better predictive accuracy and smoother interpolation\n")
} else if (lowess_wins > spline_wins) {
  selected_method <- "LOWESS"
  cat("\n✓ RECOMMENDED METHOD: LOWESS\n")
  cat("  Reasons: More robust to outliers and local patterns\n")
} else {
  selected_method <- "Spline"  # Default to Spline for prediction
  cat("\n✓ RECOMMENDED METHOD: Smooth Spline (tie-breaker)\n")
  cat("  Reasons: Both methods perform similarly; Spline offers better extrapolation\n")
}

################################################################################
# PART 6: MODEL COEFFICIENTS AND PARAMETERS
################################################################################

cat("\n=== MODEL COEFFICIENTS AND PARAMETERS ===\n")

for (treat in levels(marketing_data$Treatment)) {
  cat(sprintf("\n--- Promotion %s Model Details ---\n", treat))

  models <- model_storage[[treat]]

  # -------------------------------------------------------------------------
  # LOWESS: No traditional coefficients, but we can extract key info
  # -------------------------------------------------------------------------
  cat("\nLOWESS Model:\n")
  cat("  Note: LOWESS is non-parametric and doesn't have traditional coefficients.\n")
  cat("  Instead, it stores fitted (x, y) pairs for interpolation.\n")

  lowess_model <- models$lowess
  cat(sprintf("  - Number of smoothed points: %d\n", length(lowess_model$x)))
  cat(sprintf("  - Smoothing parameter (f): %.2f\n", 0.5))  # The f value we used
  cat(sprintf("  - Data range: [%.2f, %.2f]\n", 
              min(lowess_model$x), max(lowess_model$x)))

  # Display first few fitted points
  cat("  - Sample fitted points (first 5):\n")
  lowess_sample <- data.frame(
    Week = head(lowess_model$x, 5),
    Fitted_Sales = head(lowess_model$y, 5)
  )
  print(lowess_sample, row.names = FALSE)

  # Save all LOWESS fitted points for this treatment
  lowess_full <- data.frame(
    Treatment = treat,
    Week = lowess_model$x,
    Fitted_Sales = lowess_model$y
  )
  write.csv(lowess_full, 
            sprintf("lowess_fitted_points_treatment_%s.csv", treat), 
            row.names = FALSE)

  # -------------------------------------------------------------------------
  # SMOOTH SPLINE: Can extract basis coefficients, but predict() is preferred
  # -------------------------------------------------------------------------
  cat("\nSmooth Spline Model:\n")

  spline_model <- models$spline

  # Key parameters
  cat(sprintf("  - Degrees of freedom (df): %.4f\n", spline_model$df))
  cat(sprintf("  - Smoothing parameter (lambda): %.8f\n", spline_model$lambda))
  cat(sprintf("  - Penalty: %.6f\n", spline_model$pen.crit))
  cat(sprintf("  - Cross-validation score: %.6f\n", spline_model$cv.crit))
  cat(sprintf("  - Number of unique knots: %d\n", length(unique(spline_model$x))))

  # The spline is defined by knot locations and coefficients
  # Extract spline fit object (internal structure)
  cat("\n  Spline Basis Information:\n")
  cat(sprintf("    - Knot sequence length: %d\n", length(spline_model$fit$knot)))
  cat(sprintf("    - Number of coefficients: %d\n", length(spline_model$fit$coef)))

  # Display first few coefficients
  cat("    - Sample coefficients (first 5):\n")
  coef_sample <- head(spline_model$fit$coef, 5)
  print(coef_sample)

  # Save full coefficient information
  spline_coefficients <- data.frame(
    Treatment = treat,
    Coefficient_Index = 1:length(spline_model$fit$coef),
    Coefficient_Value = spline_model$fit$coef,
    Knot = spline_model$fit$knot[1:length(spline_model$fit$coef)]
  )
  write.csv(spline_coefficients, 
            sprintf("spline_coefficients_treatment_%s.csv", treat), 
            row.names = FALSE)

  cat(sprintf("  ✓ Full coefficients saved to 'spline_coefficients_treatment_%s.csv'\n", treat))
}

################################################################################
# PART 7: PREDICTION ON NEW DATA
################################################################################

cat("\n=== PREDICTION ON NEW DATA ===\n\n")

# Define new time points for prediction (e.g., future weeks)
new_weeks <- data.frame(Week = seq(1, 6, by = 0.5))  # Weeks 1-6 at 0.5 intervals
cat(sprintf("Predicting sales for %d new time points (Weeks 1-6)...\n\n", 
            nrow(new_weeks)))

# Storage for predictions
all_predictions <- data.frame()

for (treat in levels(marketing_data$Treatment)) {
  cat(sprintf("Promotion %s predictions:\n", treat))

  models <- model_storage[[treat]]
  data_range <- models$data_range

  # -------------------------------------------------------------------------
  # LOWESS PREDICTIONS
  # -------------------------------------------------------------------------
  # LOWESS uses linear interpolation between fitted points
  # Cannot extrapolate beyond data range reliably

  lowess_model <- models$lowess

  # Use approx() for interpolation
  lowess_predictions <- approx(
    x = lowess_model$x,
    y = lowess_model$y,
    xout = new_weeks$Week,
    method = "linear",
    rule = 2  # rule=2: extrapolate using nearest data point
  )$y

  # Flag extrapolated points
  is_extrapolated <- new_weeks$Week < data_range[1] | new_weeks$Week > data_range[2]

  cat(sprintf("  LOWESS: %d predictions (%d extrapolated - USE WITH CAUTION)\n",
              length(lowess_predictions), sum(is_extrapolated)))

  # -------------------------------------------------------------------------
  # SMOOTH SPLINE PREDICTIONS
  # -------------------------------------------------------------------------
  # Splines can extrapolate more smoothly but still with caution

  spline_model <- models$spline
  spline_predictions <- predict(spline_model, new_weeks$Week)$y

  cat(sprintf("  Spline: %d predictions\n", length(spline_predictions)))

  # -------------------------------------------------------------------------
  # STORE PREDICTIONS
  # -------------------------------------------------------------------------
  treatment_predictions <- data.frame(
    Treatment = treat,
    Week = new_weeks$Week,
    LOWESS_Prediction = lowess_predictions,
    Spline_Prediction = spline_predictions,
    Extrapolated = is_extrapolated,
    Data_Range_Min = data_range[1],
    Data_Range_Max = data_range[2]
  )

  all_predictions <- rbind(all_predictions, treatment_predictions)

  # Display sample predictions
  cat("  Sample predictions (first 3):\n")
  print(head(treatment_predictions[, c("Week", "LOWESS_Prediction", 
                                       "Spline_Prediction", "Extrapolated")], 3),
        row.names = FALSE, digits = 3)
  cat("\n")
}

# Save all predictions
write.csv(all_predictions, "model_predictions_new_data.csv", row.names = FALSE)
cat("✓ All predictions saved to 'model_predictions_new_data.csv'\n")

################################################################################
# PART 8: PREDICTION VISUALIZATION
################################################################################

cat("\n=== GENERATING PREDICTION VISUALIZATIONS ===\n")

# Create visualization comparing predictions
library(ggplot2)

# Prepare original data for plotting
original_data <- marketing_data %>%
  select(Treatment, Week, SalesInThousands)

# Plot predictions vs original data
p_pred <- ggplot() +
  # Original data points
  geom_point(data = original_data,
             aes(x = Week, y = SalesInThousands),
             alpha = 0.4, size = 2, color = "gray40") +
  # LOWESS predictions
  geom_line(data = all_predictions,
            aes(x = Week, y = LOWESS_Prediction, color = "LOWESS"),
            linetype = "solid", size = 1) +
  # Spline predictions
  geom_line(data = all_predictions,
            aes(x = Week, y = Spline_Prediction, color = "Spline"),
            linetype = "dashed", size = 1) +
  # Mark extrapolated regions
  geom_vline(data = data.frame(Treatment = c("1", "2", "3"), 
                                xmin = 1, xmax = 4),
             aes(xintercept = xmin), linetype = "dotted", alpha = 0.5) +
  geom_vline(data = data.frame(Treatment = c("1", "2", "3"), 
                                xmin = 1, xmax = 4),
             aes(xintercept = xmax), linetype = "dotted", alpha = 0.5) +
  scale_color_manual(values = c("LOWESS" = "#E41A1C", "Spline" = "#377EB8")) +
  facet_wrap(~ Treatment, ncol = 3) +
  labs(
    title = "Model Predictions: LOWESS vs Smooth Spline",
    subtitle = "Dotted lines indicate original data range; predictions beyond are extrapolations",
    x = "Week",
    y = "Predicted Sales (in Thousands)",
    color = "Method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

print(p_pred)
ggsave("prediction_comparison.png", plot = p_pred, width = 12, height = 5, dpi = 300)
cat("✓ Prediction plot saved to 'prediction_comparison.png'\n")

################################################################################
# PART 9: PREDICTION FUNCTION FOR FUTURE USE
################################################################################

cat("\n=== CREATING REUSABLE PREDICTION FUNCTION ===\n")

# Define a function to predict sales for any new week(s) and treatment
predict_sales <- function(treatment, weeks, method = "spline") {
  # treatment: "1", "2", or "3"
  # weeks: vector of week numbers
  # method: "lowess" or "spline"

  if (!treatment %in% c("1", "2", "3")) {
    stop("Treatment must be '1', '2', or '3'")
  }

  if (!method %in% c("lowess", "spline")) {
    stop("Method must be 'lowess' or 'spline'")
  }

  # Get stored model
  model <- model_storage[[treatment]]
  data_range <- model$data_range

  # Check for extrapolation
  extrapolating <- any(weeks < data_range[1] | weeks > data_range[2])
  if (extrapolating) {
    warning(sprintf("Warning: Some weeks are outside training range [%.1f, %.1f]. Extrapolation may be unreliable.",
                    data_range[1], data_range[2]))
  }

  # Make predictions
  if (method == "lowess") {
    predictions <- approx(
      x = model$lowess$x,
      y = model$lowess$y,
      xout = weeks,
      method = "linear",
      rule = 2
    )$y
  } else {
    predictions <- predict(model$spline, weeks)$y
  }

  # Return data frame
  result <- data.frame(
    Treatment = treatment,
    Week = weeks,
    Predicted_Sales = predictions,
    Method = method,
    Extrapolated = weeks < data_range[1] | weeks > data_range[2]
  )

  return(result)
}

cat("✓ Prediction function 'predict_sales()' created and ready to use\n")
cat("\nUsage examples:\n")
cat("  predict_sales(treatment = '1', weeks = c(5, 6), method = 'spline')\n")
cat("  predict_sales(treatment = '3', weeks = seq(1, 4, 0.5), method = 'lowess')\n")

# Demonstrate the function
cat("\nDemo: Predicting Promotion 2 sales for weeks 5-7 using Spline:\n")
demo_pred <- predict_sales(treatment = "2", weeks = 5:7, method = "spline")
print(demo_pred, row.names = FALSE)

################################################################################
# PART 10: SUMMARY AND RECOMMENDATIONS
################################################################################

cat("\n=== FINAL SUMMARY ===\n\n")

cat("✓ Model Comparison Complete\n")
cat("✓ Selected Method:", selected_method, "\n")
cat("✓ Model coefficients/parameters extracted and saved\n")
cat("✓ Prediction function ready for deployment\n")
cat("\nGenerated Files:\n")
cat("  1. model_comparison_metrics.csv - Detailed metric comparison\n")
cat("  2. cross_validation_results.csv - CV performance\n")
cat("  3. spline_coefficients_treatment_*.csv - Spline coefficients (per treatment)\n")
cat("  4. lowess_fitted_points_treatment_*.csv - LOWESS fitted values\n")
cat("  5. model_predictions_new_data.csv - Predictions on new weeks\n")
cat("  6. prediction_comparison.png - Visualization\n")

cat("\nKey Takeaways:\n")
cat("  • Both methods are non-parametric (data-driven, not equation-based)\n")
cat("  • For LOWESS: Use fitted (x,y) pairs with interpolation\n")
cat("  • For Splines: Use predict() function with stored model\n")
cat("  • Extrapolation beyond training data should be interpreted cautiously\n")
cat("  • Cross-validation shows real-world predictive performance\n")

cat("\n", rep("=", 70), "\n", sep = "")
