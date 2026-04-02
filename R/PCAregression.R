# Step 0: Load Libraries
library(datasets)
library(caret)
library(stats)

# Step 1: Load Dataset
data(mtcars)
X <- mtcars[, -1]  # predictors
y <- mtcars$mpg    # target

# Step 2: Data Cleaning
# Combine X and y into one dataframe
df <- mtcars

# Remove rows with missing values
df <- na.omit(df)

# Separate again
X <- df[, -1]
y <- df$mpg

# Step 3: Standardize Features
X_scaled <- scale(X)

# Step 4: Apply PCA
pca_model <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
summary(pca_model)
explained_var <- summary(pca_model)$importance[2,]
cum_var <- cumsum(explained_var)
num_components <- which(cum_var >= 0.95)[1]
X_pca <- pca_model$x[, 1:num_components]

# Step 5: Train/Test Split
set.seed(42)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X_pca[train_index, ]
X_test <- X_pca[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Step 6: Regression on PCs
model <- lm(y_train ~ ., data = as.data.frame(X_train))
summary(model)

# Step 7: Predict and Evaluate
y_pred <- predict(model, newdata = as.data.frame(X_test))
rmse <- sqrt(mean((y_test - y_pred)^2))
r2 <- 1 - sum((y_test - y_pred)^2)/sum((y_test - mean(y_test))^2)
cat(sprintf("RMSE: %.2f, R2: %.2f\n", rmse, r2))

# Step 8: Display Coefficients
coefficients <- coef(model)
print(coefficients)

# Optional: map back to original features
# Extract loadings
loadings <- pca_model$rotation[, 1:num_components]

# Convert PC coefficients to original feature space
original_coeffs <- loadings %*% coefficients[-1]

# Create readable table
original_coeffs_df <- data.frame(
  Feature = rownames(loadings),
  Coefficient = as.vector(original_coeffs)
)

print(original_coeffs_df)