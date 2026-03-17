# Load dataset
data(iris)

# View structure
head(iris)

# Remove species column (unsupervised)
df <- iris[, -5]

# Scale data
scaled_data <- scale(df)

# Elbow method
wss <- numeric(10)

for (k in 1:10) {
  model <- kmeans(scaled_data, centers = k, nstart = 10)
  wss[k] <- model$tot.withinss
}

plot(1:10, wss, type="b",
     xlab="Number of Clusters",
     ylab="Within Sum of Squares",
     main="Elbow Method")

# Implement k-means
# Apply K = 3
model <- kmeans(scaled_data, centers = 3, nstart = 10)

# View clusters
model$cluster

# Add cluster labels
iris$Cluster <- model$cluster

head(iris)

# Visualize the clusters
plot(iris$Sepal.Length, iris$Sepal.Width,
     col = iris$Cluster,
     pch = 19,
     xlab = "Sepal Length",
     ylab = "Sepal Width",
     main = "K-Means Clustering (R)")