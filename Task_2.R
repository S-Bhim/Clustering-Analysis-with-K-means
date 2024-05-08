# GRIP Task 2 Prediction Using Unsupervised ML with R
# prepared by - Soumabha Bhim

# Load necessary libraries
library(datasets)
library(cluster)

# Load the iris dataset
data(iris)

# Convert the iris dataset to a dataframe
iris_df <- data.frame(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)
colnames(iris_df) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

# Finding the optimum number of clusters for k-means classification
wcss <- vector()
for (i in 1:10) {
  kmeans_result <- kmeans(iris_df, centers = i, nstart = 10)
  wcss[i] <- kmeans_result$tot.withinss
}

# Plotting the elbow method
plot(1:10, wcss, type = "b", main = "Elbow Method for Optimal K",
     xlab = "Number of clusters (K)", ylab = "Within-cluster Sum of Squares (WCSS)",
     col = "blue", lwd = 2, pch = 19)
lines(1:10, wcss, type = "b", col = "blue", lwd = 2)
points(3, wcss[3], col = "red", pch = 19, cex = 1.5)
text(4, wcss[3], "Optimal K = 3", pos = 3.5, col = "red", cex = 0.8)
abline(v = 3, col = "red", lty = 2)

# Applying kmeans to the dataset / Creating the kmeans classifier
kmeans_model <- kmeans(iris_df, centers = 3, nstart = 10)

# Visualising the clusters - On the first two columns
plot(iris_df[, c(1, 2)], col = kmeans_model$cluster, pch = 20, 
     main = 'Clusters of Iris Species', xlab = 'Sepal Length', ylab = 'Sepal Width',cex = 1.5)
points(kmeans_model$centers[, c(1, 2)], col = 1:3, pch = 4, cex = 3)

# Adjusting legend position to avoid overlap
legend("bottomright", legend = c("Setosa", "Versicolor", "Virginica", "Centroids"), 
       col = c(1:3, "black"), pch = c(20, 20, 20, 4), 
       cex = 0.8,text.width = 0.5)

