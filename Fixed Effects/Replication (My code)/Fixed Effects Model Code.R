# Load necessary libraries
library(palmerpenguins)  # Provides the penguins dataset
library(dplyr)          # For data manipulation

# Set seed for reproducibility
set.seed(123)

# Load and explore the dataset
data("penguins")  # Access the palmerpenguins dataset
head(penguins)    # Peek at the structure
summary(penguins) # Check ranges and missing values

# Create a binary 'obese' variable (since it’s not in the dataset)
# Define obesity as body_mass_g above the 75th percentile (econometrics often uses thresholds)
penguins <- penguins %>%
  mutate(obese = ifelse(body_mass_g > quantile(body_mass_g, 0.75, na.rm = TRUE), 1, 0))

# Verify the new variable
table(penguins$obese, useNA = "ifany")  # Check distribution of 0s and 1s

# Handle missing data (econometrics prefers complete cases for reliable estimation)
penguins_clean <- na.omit(penguins)

# Subset to 100 observations (to match your prior confusion matrix sample size)
train_data <- penguins_clean[sample(nrow(penguins_clean), 100), ]

# Center continuous predictors for better interpretability
# In econometrics, centering avoids extrapolation and stabilizes the intercept
train_data <- train_data %>%
  mutate(
    flipper_length_c = flipper_length_mm - mean(flipper_length_mm),
    bill_length_c = bill_length_mm - mean(bill_length_mm)
  )

# Build a fixed effects logistic regression model
# Use 'species' and 'island' as fixed effects to control for group-specific effects
fixed_effects_model <- glm(
  obese ~ flipper_length_c + bill_length_c + sex + species + island,
  family = binomial(link = "logit"),  # Logistic regression for binary outcome
  data = train_data
)

# Display model summary
summary(fixed_effects_model)

# LEARNING COMMENTS:
# 1. Fixed Effects Definition:
#    - 'species' (Adelie, Chinstrap, Gentoo) and 'island' (Biscoe, Dream, Torgersen) are fixed effects.
#    - Each level gets a coefficient (dummy variable), capturing group-specific intercepts.
#    - Example: 'speciesChinstrap' coefficient shows the difference in log-odds of obesity for Chinstrap vs. Adelie.
# 2. Econometrics Purpose:
#    - Fixed effects control for unobserved, time-invariant heterogeneity (e.g., species-specific diet, island climate).
#    - This reduces omitted variable bias, a key concern in econometrics.
# 3. Model Structure:
#    - Continuous predictors ('flipper_length_c', 'bill_length_c') are covariates.
#    - 'sex' is a fixed effect with two levels (female as reference).
#    - No interaction term here (simplified from your prior model) to focus on fixed effects.
# 4. Intercept:
#    - With centered predictors, the intercept is the log-odds of obesity for the reference group
#      (female, Adelie, Biscoe) at average flipper and bill lengths.
# 5. Logistic Regression:
#    - Coefficients represent changes in log-odds of obesity (obese = 1).
#    - Odds ratio = exp(Estimate); e.g., exp(1.2) = 3.32 means 3.32 times higher odds.

# Generate predictions
probs <- predict(fixed_effects_model, type = "response")  # Predicted probabilities
preds <- ifelse(probs >= 0.5, 1, 0)  # Classify using 0.5 threshold

# Create confusion matrix
conf_matrix <- table(Prediction = preds, Reference = train_data$obese)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy, 3), "\n")

# LEARNING COMMENTS:
# 6. Evaluation:
#    - Confusion matrix shows true positives (TP), false positives (FP), etc.
#    - Links to your prior work (e.g., 42/2/13/43 matrix) for performance comparison.
# 7. Econometrics Insight:
#    - Fixed effects improve prediction by accounting for group differences.
#    - Example: If Dream island has less food (unobserved), its fixed effect adjusts the baseline.
# 8. Threshold Choice:
#    - 0.5 is standard, but econometrics might explore other thresholds (e.g., ROC analysis) for cost-sensitive outcomes.

# Assess model fit
logLik(fixed_effects_model)  # Log-likelihood
AIC(fixed_effects_model)     # Akaike Information Criterion (lower = better fit)

# LEARNING COMMENTS:
# 9. Model Fit:
#    - Log-likelihood measures how well the model explains the data.
#    - AIC balances fit and complexity (penalizes extra parameters from fixed effects).
# 10. Fixed Effects Trade-Off:
#     - Pros: Controls for group-specific effects, reduces bias.
#     - Cons: Uses many parameters (e.g., 2 for species, 2 for island), risking overfitting with small samples.

# Optional: Compare to a model without fixed effects
simple_model <- glm(
  obese ~ flipper_length_c + bill_length_c + sex,
  family = binomial(link = "logit"),
  data = train_data
)
summary(simple_model)
AIC(simple_model)  # Compare AIC to fixed effects model

# LEARNING COMMENTS:
# 11. Comparison:
#     - Simple model omits 'species' and 'island', risking bias from unobserved group effects.
#     - If AIC is lower for fixed_effects_model, it justifies including fixed effects.
# 12. Econometrics Application:
#     - Fixed effects are like panel data models (e.g., firm or time fixed effects).
#     - Here, they mimic controlling for penguin species or island differences in a cross-sectional setting.