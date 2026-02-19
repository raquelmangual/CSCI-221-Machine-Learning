#Install the necessary packages and libraries
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("caret")

library(rpart)
library(rpart.plot)
library(caret)

# Load the data
data <- read.csv("Titanic/Titanic-Dataset.csv")
# Preview the data
head(data)

#Preprocess the data
# Select relevant columns
data <- data[, c("Survived", "Pclass", "Sex", "Age", "Fare")]

# Convert categorical variables
data$Sex <- as.factor(data$Sex)
data$Survived <- as.factor(data$Survived)

#Check for missing values
colSums(is.na(data))

# Impute missing Age values with median
data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)

#Confirm values have been imputed
colSums(is.na(data))

#Split the data into sets for training and testing
set.seed(42)

trainIndex <- createDataPartition(data$Survived, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Create and train the decision tree classifier
tree_model <- rpart(
  Survived ~ Pclass + Sex + Age + Fare,
  data = trainData,
  method = "class"
)

# Visualize the decision tree
"Tree before pruning"
rpart.plot(tree_model)

# Make some predictions
predictions <- predict(tree_model, testData, type = "class")

# Evaluate the model
confusionMatrix(predictions, testData$Survived)

# Overfitting and pruning
# Show complexity parameter
printcp(tree_model)
plotcp(tree_model)

# Prune the model
pruned_tree <- prune(tree_model, cp = 0.03) #Will this make it better?
# Visualize the pruned model
printcp(pruned_tree)
rpart.plot(pruned_tree)