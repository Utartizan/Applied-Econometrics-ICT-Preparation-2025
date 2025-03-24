# File: improved_confusion_matrix_logit_public.R
# Purpose: Build and analyze a confusion matrix for a Logit model using public obesity data
# Date: March 24, 2025
# Note: Improved model added at top; original below for comparison

# Load necessary packages
library(tidyverse)  # For data manipulation (dplyr, ggplot2) and piping (%>%)
library(caret)      # For confusionMatrix() function and performance metrics
library(palmerpenguins)  # Public dataset with body mass data as a proxy for obesity

# Load the penguins dataset
data("penguins", package = "palmerpenguins")



# --- Improved Model ---
# Purpose: Enhance prediction of obesity with more predictors, interaction, and validation




# Prepare data for improved model
penguins_improved <- penguins %>%
  mutate(obese = ifelse(body_mass_g > median(body_mass_g, na.rm = TRUE), 1, 0))  # Binary obesity

# Detailed Comments:
# - Same obesity definition as original: 1 if body mass > median, 0 otherwise
# - Computed within mutate() for efficiency; na.rm = TRUE handles missing values




penguins_improved <- na.omit(penguins_improved)  # Remove NA rows
# Detailed Comments:
# - Ensures clean dataset; ~333 rows remain after dropping NA

# Train-test split for validation (70% train, 30% test)

set.seed(123)  # Reproducible split
train_idx <- sample(1:nrow(penguins_improved), 0.7 * nrow(penguins_improved))
train_data <- penguins_improved[train_idx, ]
test_data <- penguins_improved[-train_idx, ]
# Detailed Comments:
# - train_idx: Randomly select 70% of rows (~233 rows) for training
# - train_data: Training set; test_data: Testing set (~100 rows)
# - Validates model performance on unseen data, avoiding overfitting



# Fit improved Logit model with additional predictors and interaction
improved_model <- glm(obese ~ flipper_length_mm * bill_length_mm + sex + species, 
                      data = train_data, 
                      family = binomial(link = "logit"))
# Detailed Comments:


# Model summary
summary(improved_model)
# Detailed Comments:
# - Coefficients: Effect sizes for each predictor and interaction
# - p-values: < 0.05 indicates significance; check interaction term especially
# - AIC: lower = better fit
# - Deviance: Drop from null to residual shows explanatory power




# Predict probabilities on test data
test_data$prob_obese <- predict(improved_model, newdata = test_data, type = "response")
# Detailed Comments:
# - newdata = test_data: Predict on unseen test set for realistic performance
# - type = "response": Outputs probabilities (0 to 1)






# Classify with lower threshold (0.4 instead of 0.5)
test_data$pred_obese <- ifelse(test_data$prob_obese > 0.4, 1, 0)
# Detailed Comments:
# - Threshold 0.4: Lowered to increase sensitivity (catch more obese cases)
# - pred_obese: Binary predictions based on adjusted threshold








# Create confusion matrix for improved model

improved_conf_matrix <- confusionMatrix(
  as.factor(test_data$pred_obese), 
  as.factor(test_data$obese), 
  positive = "1"
)

# Detailed Comments:
# - Evaluates predictions on test set
# - positive = "1": Focus on obese class for sensitivity/precision





# Display improved confusion matrix
print(improved_conf_matrix)
# Detailed Comments:
# - TN: Correct non-obese predictions
# - FP: Overpredicted obese cases
# - FN: Missed obese cases (should decrease with 0.4 threshold)
# - TP: Correct obese predictions (should increase)
# - Expected improvement: Higher sensitivity, possibly balanced accuracy

# Extract and interpret improved metrics
imp_accuracy <- improved_conf_matrix$overall["Accuracy"]
imp_sensitivity <- improved_conf_matrix$byClass["Sensitivity"]
imp_specificity <- improved_conf_matrix$byClass["Specificity"]
imp_precision <- improved_conf_matrix$byClass["Pos Pred Value"]
cat("Improved Accuracy:", round(imp_accuracy, 3), 
    "- Proportion correct;\n")
cat("Improved Sensitivity:", round(imp_sensitivity, 3), 
    "- Detection of obese;\n")
cat("Improved Specificity:", round(imp_specificity, 3), 
    "- Detection of non-obese;\n")
cat("Improved Precision:", round(imp_precision, 3), 
    "- Reliability of obese predictions;\n")






imp_conf_table <- as.table(improved_conf_matrix$table)
ggplot(as.data.frame(imp_conf_table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Improved Confusion Matrix: Obesity Prediction", 
       x = "Actual", y = "Predicted") +
  theme_minimal()




# Detailed Comments:

# ICT Implications:

