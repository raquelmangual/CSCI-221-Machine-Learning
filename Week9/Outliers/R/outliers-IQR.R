# Load dataset
data(mtcars)

# Calculate quartiles
Q1 <- quantile(mtcars$mpg, 0.25)
Q3 <- quantile(mtcars$mpg, 0.75)

# Calculate IQR
IQR_value <- IQR(mtcars$mpg)

# Determine bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify outliers
outliers <- mtcars[mtcars$mpg < lower_bound | mtcars$mpg > upper_bound, ]

# Count outliers
cat("Number of outliers:", nrow(outliers), "\n")

# List outliers
print(outliers)

# Remove outliers
mtcars_clean <- mtcars[mtcars$mpg >= lower_bound & mtcars$mpg <= upper_bound, ]

# Compare sizes
cat("Original dataset rows:", nrow(mtcars), "\n")
cat("Rows after removing outliers:", nrow(mtcars_clean), "\n")