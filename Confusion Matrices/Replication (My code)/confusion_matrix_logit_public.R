# File: confusion_matrix_logit_public.R
# Purpose: Build and analyze a confusion matrix for a Logit model using public obesity data
# Date: March 24, 2025
# Note: Uses 'palmerpenguins' package as a proxy for obesity-related data since I don't know the exact data package that'll be used in the ICT.



# Load necessary packages
library(tidyverse)  # For data manipulation (dplyr, ggplot2) and piping (%>%)
library(caret)      # For confusionMatrix() function and performance metrics
install.packages("palmerpenguins")
library(palmerpenguins)  # Public dataset with body mass data as a proxy for obesity




# Load the penguins dataset (proxy for obesity-related data)
data("penguins", package = "palmerpenguins")



# Explore the dataset
head(penguins)  # View first 6 rows to understand structure
str(penguins)   # Check variable types and missing values
summary(penguins)  # Summary statistics for numeric variables



# Prepare the data: Create a binary 'obese' variable
median_mass <- median(penguins$body_mass_g, na.rm = TRUE)  # Calculate median body mass, ignoring NA

# Detailed Comments:

# - median_mass: Central tendency of body mass (around 4050g in this dataset)

# - na.rm = TRUE: Removes missing values from calculation to avoid errors





penguins <- penguins %>%
  mutate(obese = ifelse(body_mass_g > median_mass, 1, 0)) # 1 = obese (above median), 0 = not obese

# Detailed Comments:

# - mutate(): Adds new column 'obese' to the dataframe

# - ifelse(): Conditional statement; if body_mass_g > median_mass, assign 1, else 0

# - This binarises (0-1) the outcome for Logit modeling; threshold is arbitrary but reasonable for demo






# Remove rows with missing values for modeling
penguins_clean <- na.omit(penguins)  # Drop rows with any NA

# Detailed Comments:

# - na.omit(): Ensures no missing data in predictors or outcome, as glm() doesn’t handle NA well

# - Original rows: 344; after cleaning, expect ~333 (depending on NA count)







# Fit a Logit model: Predict obesity based on flipper_length_mm and bill_length_mm

logit_model <- glm(obese ~ flipper_length_mm + bill_length_mm, 
                   data = penguins_clean, 
                   family = binomial(link = "logit"))

# Detailed Comments:

# - glm(): Generalised Linear Model for binary outcome

# - obese ~ flipper_length_mm + bill_length_mm: Formula where obesity is predicted by flipper and bill length

# - family = binomial(link = "logit"): Specifies Logit model for binary data (log-odds transformation)

# - No intercept explicitly added; glm() includes it by default





# View model summary
summary(logit_model)

# Detailed Comments:

# - Coefficients: 

#   - Intercept: Baseline log-odds of obesity when predictors are 0 (not meaningful here due to scaling)

#   - flipper_length_mm: Change in log-odds per 1mm increase in flipper length

#   - bill_length_mm: Change in log-odds per 1mm increase in bill length

# - Std. Error: Measures precision of coefficient estimates; smaller = more precise

# - z value: Coefficient / Std. Error; tests significance

# - Pr(>|z|): p-value; < 0.05 indicates significant predictor at 5% level

# - AIC: Akaike Information Criterion; lower = better fit (compare to other models if needed)

# - Null deviance: Model fit with only intercept; Residual deviance: Fit with predictors; drop indicates improvement






# Predict probabilities
penguins_clean$prob_obese <- predict(logit_model, type = "response")

# Detailed Comments:
# - predict(): Generates predictions from the fitted model

# - type = "response": Converts log-odds to probabilities (0 to 1); default is log-odds

# - prob_obese: Probability each penguin is obese based on flipper and bill length







# Classify predictions: Use 0.5 threshold
penguins_clean$pred_obese <- ifelse(penguins_clean$prob_obese > 0.5, 1, 0)

# Detailed Comments:
# - ifelse(): Assigns 1 if prob_obese > 0.5, 0 otherwise

# - 0.5 threshold: Standard cutoff for binary classification; could adjust (e.g., 0.3) for sensitivity

# - pred_obese: Predicted class (1 = obese, 0 = not obese)





# Create confusion matrix
conf_matrix <- confusionMatrix(
  as.factor(penguins_clean$pred_obese),  # Predicted classes as factor
  as.factor(penguins_clean$obese),       # Actual classes as factor
  positive = "1"                         # Define "1" (obese) as positive class
)

# Detailed Comments:
# - confusionMatrix(): From caret package; compares predictions to actual outcomes

# - as.factor(): Converts numeric 0/1 to factors for caret compatibility

# - positive = "1": Sets "obese" as the event of interest for sensitivity/specificity






# Display confusion matrix
print(conf_matrix)
# Detailed Comments:

# - Table structure:

#   - Rows: Predicted (0, 1)
#   - Columns: Reference (actual 0, 1)



#   - Top-left: True Negatives (TN): Correctly predicted non-obese

#   - Top-right: False Positives (FP): Predicted obese, actually not obese

#   - Bottom-left: False Negatives (FN): Predicted not obese, actually obese

#   - Bottom-right: True Positives (TP): Correctly predicted obese




# - Accuracy: (TP + TN) / (TP + TN + FP + FN); overall correctness

# - Sensitivity: TP / (TP + FN); proportion of obese penguins correctly identified

# - Specificity: TN / (TN + FP); proportion of non-obese penguins correctly identified

# - Precision: TP / (TP + FP); proportion of obese predictions that are correct

# - Kappa: Agreement beyond chance; > 0.6 is good




# Extract and interpret key metrics
accuracy <- conf_matrix$overall["Accuracy"]

sensitivity <- conf_matrix$byClass["Sensitivity"]

specificity <- conf_matrix$byClass["Specificity"]

precision <- conf_matrix$byClass["Pos Pred Value"]

# Detailed Comments:

# - accuracy: Single number summarizing overall performance

# - sensitivity: Focus on detecting obesity; low value means missing cases

# - specificity: Focus on ruling out obesity; low value means overprediction

# - precision: Reliability of positive predictions; low value means many false positives




cat("Accuracy:", round(accuracy, 3), 
    "- Proportion of correct predictions; >0.7 is decent, <0.5 is poor\n")

cat("Sensitivity:", round(sensitivity, 3), 
    "- Ability to detect obese penguins; <0.5 means many obese cases missed\n")

cat("Specificity:", round(specificity, 3), 
    "- Ability to detect non-obese penguins; <0.5 means overpredicting obesity\n")

cat("Precision:", round(precision, 3), 
    "- Accuracy of obesity predictions; <0.5 means many false positives\n")

# Detailed Comments:

#to finish









# Visualise confusion matrix as heatmap (display directly)
conf_table <- as.table(conf_matrix$table)  # Convert matrix to table format
ggplot(as.data.frame(conf_table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +  # Create heatmap tiles
  geom_text(aes(label = Freq), vjust = 1) +  # Add frequency labels
  scale_fill_gradient(low = "white", high = "blue") +  # Color gradient
  labs(title = "Confusion Matrix: Obesity Prediction", 
       x = "Actual", y = "Predicted") +  # Axis labels
  theme_minimal() 

# Detailed Comments:
# to complete

# Implications for ICT:
# to complete/finish