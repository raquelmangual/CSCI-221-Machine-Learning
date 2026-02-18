# Install if needed
# Packages only need to be installed once per session
# Make the following lines a comment after installation
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("caret")   # For train/test split

# Load libraries
# Libraries need to be loaded every time the script is run
library(rpart)
library(rpart.plot)
library(caret)

# Load  dataset
df <- read.csv("AmesHousing.csv")

# View structure
str(df)

# View first few rows
head(df)

# Check dimensions
dim(df)

# Keep only numeric columns
numeric_df <- df[, sapply(df, is.numeric)]

# Check remaining columns
colnames(numeric_df)

# Define target and features
# Separate target variable
y <- numeric_df$SalePrice

# Remove SalePrice from predictors
X <- numeric_df[, !(colnames(numeric_df) %in% "SalePrice")]

# Impute missing values
# Replace NA values with column medians
for(i in 1:ncol(X)){
  X[is.na(X[,i]), i] <- median(X[,i], na.rm = TRUE)
}

# Recombine into dataframe
clean_df <- data.frame(X, SalePrice = y)

# Split data into 80/20 train/test sets
set.seed(42)

train_index <- createDataPartition(clean_df$SalePrice, 
                                   p = 0.8, 
                                   list = FALSE)

train_data <- clean_df[train_index, ]
test_data <- clean_df[-train_index, ]

# Train the model
method = "anova"
tree_model <- rpart(
  SalePrice ~ ., 
  data = train_data,
  method = "anova",
  control = rpart.control(maxdepth = 5)  # Control complexity
)

summary(tree_model)

# Create a visualization of the Decision Tree
rpart.plot(tree_model)