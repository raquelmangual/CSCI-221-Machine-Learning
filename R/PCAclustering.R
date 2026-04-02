# Install package
#install.packages("factoextra")

# Load libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)

# ---------------------------
# Step 1: Load and Clean Data
# ---------------------------
data(iris)
df <- iris
df <- distinct(df)  # remove duplicates

# Select numerical columns
X <- df %>% select(where(is.numeric))

# Scale features
X_scaled <- scale(X)

# ---------------------------
# Step 2: PCA
# ---------------------------
pca <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
summary(pca)  # explained variance

# Scree plot
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

# Reduce to 2 principal components for clustering
X_pca <- as.data.frame(pca$x[,1:2])

# ---------------------------
# Step 3: Elbow Method for K-Means
# ---------------------------
set.seed(42)
wss <- numeric(10)  # within-cluster sum of squares
for (k in 1:10) {
  wss[k] <- kmeans(X_pca, centers=k, nstart=25)$tot.withinss
}

# Plot Elbow Method
plot(1:10, wss, type="b", pch=19, frame=FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow Method for Optimal K")

# Choose k based on elbow (4)
optimal_k <- 4
kmeans_result <- kmeans(X_pca, centers = optimal_k, nstart = 25)
X_pca$Cluster <- as.factor(kmeans_result$cluster)

# ---------------------------
# Step 4: Visualize Clusters
# ---------------------------
ggplot(X_pca, aes(x=PC1, y=PC2, color=Cluster)) +
  geom_point(size=3) +
  labs(title=paste("K-Means Clusters on PCA-reduced Data (k=", optimal_k, ")", sep="")) +
  theme_minimal()