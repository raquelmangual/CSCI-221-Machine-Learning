# Load dataset
data(mtcars)

# View structure
str(mtcars)

# Preview data
head(mtcars)

# Count missing values per column
colSums(is.na(mtcars))

# Check for duplicates
sum(duplicated(mtcars))

# Remove duplicates (if any)
mtcars_clean <- mtcars[!duplicated(mtcars), ]

# Handle categorical variables, convert to factors
mtcars_clean <- mtcars
mtcars_clean$cyl  <- as.factor(mtcars_clean$cyl)
mtcars_clean$vs   <- as.factor(mtcars_clean$vs)
mtcars_clean$am   <- as.factor(mtcars_clean$am)
mtcars_clean$gear <- as.factor(mtcars_clean$gear)
mtcars_clean$carb <- as.factor(mtcars_clean$carb)

# Keep only numeric variables
numeric_df <- mtcars_clean[, sapply(mtcars_clean, is.numeric)]

# Check for outliers
# Boxplot for quick visualization
boxplot(numeric_df, main = "Boxplot of Variables")

# Remove outliers using z-score
z_scores <- scale(numeric_df)

# Keep rows where all z-scores are within ±3
numeric_df_clean <- numeric_df[apply(abs(z_scores) < 3, 1, all), ]

# Scale the data
scaled_data <- scale(numeric_df_clean)

# Implement PCA
pca_model <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# View the results
summary(pca_model)

# Scree plot
# Base R scree plot
plot(pca_model, type = "l", main = "Scree Plot")

# Visualization to explain variance
explained_var <- pca_model$sdev^2
prop_var <- explained_var / sum(explained_var)

plot(prop_var, 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     type = "b")

# Loadings: How much each variable contributed to the new dataset
pca_model$rotation

# PCA scores
pca_scores <- pca_model$x
head(pca_scores)

# Visualize the PCAs
plot(pca_scores[,1], pca_scores[,2],
     xlab = "PC1",
     ylab = "PC2",
     main = "PCA Plot",
     pch = 19)
