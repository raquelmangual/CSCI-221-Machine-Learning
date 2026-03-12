# Load dataset
data(mtcars)

# Preview dataset
head(mtcars)

# Fit regression model
model <- lm(mpg ~ wt + hp, data = mtcars)

# Display summary
summary(model)

# Extract R-squared
r2 <- summary(model)$r.squared

# Convert to percentage
accuracy_percent <- r2 * 100

cat("Model Accuracy (R² as %):", round(accuracy_percent,2), "%\n")

# Generate predictions
predictions <- predict(model, mtcars)

# Calculate RMSE
rmse <- sqrt(mean((mtcars$mpg - predictions)^2))

# Calculate MAE
mae <- mean(abs(mtcars$mpg - predictions))

cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")