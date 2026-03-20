# Load dataset
winedata <- read.csv("wine-clustering/wine-clustering.csv")

# View the data structure
str(winedata)

# Preview the data
head(winedata)

# View summary statistics
summary(winedata)

# Clean the data first!
# Count duplicate rows
sum(duplicated(winedata))

# Count missing values in each column
colSums(is.na(winedata))

# Calculate z-scores for outliers
z_scores <- as.data.frame(scale(winedata))

# Count outliers per column
outlier_counts <- colSums(abs(z_scores) > 3)

outlier_counts

# Keep rows where all z-scores are within the threshold
winedata_clean <- winedata[apply(abs(z_scores) <= 3, 1, all), ]

# View result
nrow(winedata)        # original rows
nrow(winedata_clean)  # cleaned rows

# Scale the data
scaled_data <- scale(winedata_clean)

# Find the ideal number of clusters
# Elbow method
wss <- numeric(10)

for (k in 1:10) {
  model <- kmeans(scaled_data, centers = k, nstart = 10)
  wss[k] <- model$tot.withinss
}

plot(1:10, wss, type="b",
     xlab="Number of Clusters",
     ylab="Within Sum of Squares",
     main="Elbow Method for Wine Data")

# Implement k-means
# Apply K = 3
model <- kmeans(scaled_data, centers = 3, nstart = 10)

# Add cluster labels to the cleaned dataset and preview
winedata_clean$Cluster <- model$cluster
head(winedata_clean)

# Visualize the clusters
plot(winedata_clean$Color_Intensity, winedata_clean$Alcohol,
     col = winedata_clean$Cluster,
     pch = 19,
     xlab = "Color_Intensity",
     ylab = "Alcohol",
     main = "K-Means Clustering (R)")