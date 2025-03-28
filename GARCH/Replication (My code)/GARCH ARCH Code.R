# Load required libraries
library(tseries)      # For basic GARCH modeling
library(rugarch)      # For advanced GARCH specifications and diagnostics
library(dplyr)        # For data manipulation
library(FinTS)        # For ARCH-LM test (arch.test function)

# Set seed for reproducibility
set.seed(123)

# Simulate a time series dataset (since palmerpenguins is cross-sectional)
# Imagine daily body mass changes for a penguin over 200 days
n <- 200
body_mass_change <- numeric(n)
body_mass_change[1] <- 0  # Starting point
sigma <- numeric(n)       # Initialise volatility series
sigma[1] <- 0.1           # Initial volatility (small arbitrary value)

# Simulate AR(1) process with GARCH(1,1) volatility
for (t in 2:n) {
  # GARCH(1,1) variance equation: omega + alpha1 * past error^2 + beta1 * past variance
  sigma[t] <- sqrt(0.01 + 0.1 * body_mass_change[t-1]^2 + 0.8 * sigma[t-1]^2)
  # AR(1) mean equation with GARCH error
  body_mass_change[t] <- 0.02 + 0.3 * body_mass_change[t-1] + rnorm(1, 0, sigma[t])
}

# LEARNING COMMENTS:
# 1. Fix Explanation: 
#    - Added 'FinTS' library for arch.test() to check ARCH effects.
# 2. Simulation Structure:
#    - Mean: AR(1) with drift (0.02) and autoregression (0.3).
#    - Variance: GARCH(1,1) with omega (0.01), alpha1 (0.1), beta1 (0.8).
# 3. Why GARCH? Simulates volatility clustering (big changes follow big changes).
# 4. Econometrics Context: Adapted from financial series (e.g., returns) to biological data.

# Plot the series to visualize volatility
plot(body_mass_change, type = "l", main = "Simulated Body Mass Change",
     ylab = "Change (g)", xlab = "Day")
abline(h = 0, col = "red", lty = 2)

# Test for ARCH effects (prerequisite for GARCH)
ar1_model <- arima(body_mass_change, order = c(1, 0, 0))
arch_test <- ArchTest(ar1_model$residuals, lags = 1)  # Use ArchTest from FinTS
print(arch_test)

# LEARNING COMMENTS:
# 5. ARCH Test:
#    - Null: No ARCH effects (constant variance).
#    - p < 0.05 → Reject null, GARCH is appropriate.
# 6. Fix Note:
#    - Used 'ArchTest' (capitalised) from FinTS, applied to AR(1) residuals.
# 7. Purpose: Confirms heteroskedasticity, justifying GARCH.

# Fit a basic GARCH(1,1) model using tseries
garch_model <- garch(body_mass_change, order = c(1, 1), trace = FALSE)
summary(garch_model)

# LEARNING COMMENTS:
# 8. GARCH(1,1):
#    - 1 ARCH term (past squared errors), 1 GARCH term (past variance).
#    - Coefficients: a0 (constant), a1 (ARCH), b1 (GARCH).
# 9. Interpretation:
#    - a1 + b1 < 1 for stationarity.
#    - High b1 → persistent volatility.

# Advanced GARCH using rugarch
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),  # Standard GARCH(1,1)
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),     # AR(1) mean
  distribution.model = "norm"                                      # Normal errors
)
garch_fit <- ugarchfit(spec = spec, data = body_mass_change)
print(garch_fit)

# Extract key results
coef(garch_fit)         # Coefficients (mu, ar1, omega, alpha1, beta1)
sigma_garch <- sigma(garch_fit)  # Conditional volatility
fitted_values <- fitted(garch_fit)  # Fitted mean values

# LEARNING COMMENTS:
# 10. rugarch Advantages:
#     - Explicit mean and variance models, plus distribution choice.
# 11. Coefficients:
#     - mu: Mean intercept.
#     - ar1: AR(1) coefficient.
#     - omega: Variance constant.
#     - alpha1: ARCH effect.
#     - beta1: GARCH effect.
# 12. Stationarity:
#     - alpha1 + beta1 < 1 ensures bounded volatility.

# Plot conditional volatility
plot(sigma_garch, type = "l", main = "Conditional Volatility",
     ylab = "Volatility", xlab = "Day")

# Diagnostics: Check residuals
residuals <- residuals(garch_fit, standardize = TRUE)
acf(residuals^2, main = "ACF of Squared Standardized Residuals")  # No spikes → good fit
qqnorm(residuals, main = "QQ-Plot of Standardized Residuals"); qqline(residuals)  # Normality check

# LEARNING COMMENTS:
# 13. Diagnostics:
#     - ACF: No autocorrelation in squared residuals → volatility captured.
#     - QQ-Plot: Deviations suggest alternative distributions (e.g., t).
# 14. Exam Tip:
#     - Interpret coefficients, check stationarity, justify with diagnostics.
