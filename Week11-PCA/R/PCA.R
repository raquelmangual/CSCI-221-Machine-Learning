# Load libraries
library(dplyr)

# Load dataset
data(mtcars)

# Preview the data
str(mtcars)
summary(mtcars)

# Always clean the data first
# Duplicates
# Count duplicate rows
sum(duplicated(mtcars))
# View duplicate rows
mtcars[duplicated(mtcars), ]
# Remove duplicates
mtcars <- mtcars %>% distinct()
# Confirm any duplicates have been removed
sum(duplicated(mtcars))

# Missing Data
# Count missing data per column
colSums(is.na(mtcars))
# Total of missing data
sum(is.na(mtcars))

# Outliers
# Calculate z-scores
z_scores <- scale(mtcars)
# Identify outliers
outliers <- abs(z_scores) > 3
# Count outliers per column
colSums(outliers)
# Remove outliers
mtcars_clean <- mtcars[!apply(outliers, 1, any), ]
# Confirm outliers have been removed
z_scores_clean <- scale(mtcars_clean)
outliers_clean <- abs(z_scores_clean) > 3
colSums(outliers_clean)

# Start PCA
# Step 1: Standardize data
mtcars_scaled <- scale(mtcars_clean) #we already did this step above, but just following procedure

# Step 2: Apply PCA
pca_model <- prcomp(mtcars_scaled, center = TRUE, scale. = TRUE)

# Step 3: Summary
summary(pca_model)

# Step 4: Scree Plot
plot(pca_model, type = "l")

# Step 5: Biplot
biplot(pca_model)