# install.packages("e1071") -> This only needs to be installed once

# Load the library, can also be selected with a checkbox if using RStudio
library(e1071)
# Load built-in dataset
data("airquality")

# 1. Identify Skewness (requires 'e1071' library for numeric skew value)
# Calculate skewness for a single column (removing NAs is essential)
skew_val <- skewness(airquality$Ozone, na.rm = TRUE)
print(skew_val)

# Visual check
hist(airquality$Ozone, main="Original Ozone Distribution", col="lightblue")

# 2. Imputation: Using Median for missing Ozone values
# Mean would be biased by the right tail
median_ozone <- median(airquality$Ozone, na.rm = TRUE)
airquality$Ozone[is.na(airquality$Ozone)] <- median_ozone

# 3. Transformation: Log Transformation
# We add 1 to avoid log(0) if necessary
airquality$Ozone_log <- log(airquality$Ozone + 1)

# Compare results
par(mfrow=c(1,2))
hist(airquality$Ozone, main="After Median Imputation")
hist(airquality$Ozone_log, main="After Log Transform", col="lightgreen")

# 4. Box-Cox in R (requires 'MASS' library)
# library(MASS)
# boxcox_result <- boxcox(Ozone ~ 1, data = airquality)