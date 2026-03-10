# Load dataset
data(airquality)

# Remove missing values
airquality_clean <- na.omit(airquality)

# Calculate z-scores
z_scores <- scale(airquality_clean$Ozone)

# Identify outliers
outliers <- airquality_clean[abs(z_scores) > 3, ]

# Count outliers
cat("Number of outliers:", nrow(outliers), "\n")

# List outliers
print(outliers)

# Remove outliers
airquality_no_outliers <- airquality_clean[abs(z_scores) <= 3, ]

# Compare dataset sizes
cat("Original dataset rows:", nrow(airquality_clean), "\n")
cat("Rows after removing outliers:", nrow(airquality_no_outliers), "\n")