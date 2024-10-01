#Lab 2 Part 2 Meena

#Code he provided 

library("e1071")

classifier<-naiveBayes(iris[,1:4], iris[,5]) 

table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted', 'actual'))

classifier$tables$Petal.Length

# one class
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species") 

# another class
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue") 

# the final class
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")








#Exercise 1

# Step 1!!!!

#• Repeat the naïve bayes analysis using the abalone dataset.



library("e1071")

abalone <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab2Part2/abalone/abalone.data", header = FALSE)


# Assign column names
colnames(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")



# Step 3: Convert 'rings' into a categorical factor (young, adult, old) as in the example
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, breaks = c(-1, 8, 11, 35), labels = c("young", "adult", "old"))
abalone$rings <- as.factor(abalone$rings)


abalone$sex <- NULL


classifier <- naiveBayes(abalone[, 1:7], abalone$rings)


predictions <- predict(classifier, abalone[, 1:7])
table(predictions, abalone$rings, dnn = list('predicted', 'actual'))

classifier$tables$length

plot(function(x) dnorm(x, mean(abalone$length), sd(abalone$length)), 0, 1, col="red", main="Length distribution for abalone classes")


curve(dnorm(x, mean(abalone$diameter), sd(abalone$diameter)), add = TRUE, col="blue")
curve(dnorm(x, mean(abalone$whole_weight), sd(abalone$whole_weight)), add = TRUE, col="green")







# Exercise 1 PART 2
# • Try 3 different subsets of features not just all features at once.



library("e1071")

abalone <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab2Part2/abalone/abalone.data", header = FALSE)

colnames(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")

abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, breaks = c(-1, 8, 11, 35), labels = c("young", "adult", "old"))
abalone$rings <- as.factor(abalone$rings)

abalone$sex <- NULL



# Define subsets of features
feature_set_1 <- c("length", "diameter", "height")          # Subset 1
feature_set_2 <- c("whole_weight", "shucked_weight")        # Subset 2
feature_set_3 <- c("viscera_weight", "shell_weight")        # Subset 3


# Create a function to run Naive Bayes and print results
run_naive_bayes <- function(features) {
  classifier <- naiveBayes(abalone[, features], abalone$rings)
  predictions <- predict(classifier, abalone[, features])
  
  cat("Features:", paste(features, collapse = ", "), "\n")
  print(table(predictions, abalone$rings, dnn = list('predicted', 'actual')))
  cat("\n")
}

# Run Naive Bayes on different subsets
run_naive_bayes(feature_set_1)
run_naive_bayes(feature_set_2)
run_naive_bayes(feature_set_3)








# Exercise 1 Part 3
#• Compare models using contingency tables.

library("e1071")

abalone <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab2Part2/abalone/abalone.data", header = FALSE)

colnames(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")

abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, breaks = c(-1, 8, 11, 35), labels = c("young", "adult", "old"))
abalone$rings <- as.factor(abalone$rings)

abalone$sex <- NULL

# Define subsets of features
feature_set_1 <- c("length", "diameter", "height")          # Subset 1
feature_set_2 <- c("whole_weight", "shucked_weight")        # Subset 2
feature_set_3 <- c("viscera_weight", "shell_weight")        # Subset 3



# Create a function to run Naive Bayes and return predictions
run_naive_bayes <- function(features) {
  classifier <- naiveBayes(abalone[, features], abalone$rings)
  predictions <- predict(classifier, abalone[, features])
  return(predictions)
}

# Run Naive Bayes on different subsets and store predictions
predictions_1 <- run_naive_bayes(feature_set_1)
predictions_2 <- run_naive_bayes(feature_set_2)
predictions_3 <- run_naive_bayes(feature_set_3)


# Compare models using contingency tables
cat("Contingency Table for Feature Set 1:\n")
print(table(predictions_1, abalone$rings, dnn = list('predicted', 'actual')))

cat("\nContingency Table for Feature Set 2:\n")
print(table(predictions_2, abalone$rings, dnn = list('predicted', 'actual')))

cat("\nContingency Table for Feature Set 3:\n")
print(table(predictions_3, abalone$rings, dnn = list('predicted', 'actual')))













# Exercise 1 Part 4
# • Plot the distribution of classes along 3 different features.

library("e1071")
library("ggplot2")

abalone <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab2Part2/abalone/abalone.data", header = FALSE)


colnames(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")


abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, breaks = c(-1, 8, 11, 35), labels = c("young", "adult", "old"))
abalone$rings <- as.factor(abalone$rings)

abalone$sex <- NULL


# Plotting distributions
# 1. Distribution of classes along 'length'
ggplot(abalone, aes(x = length, fill = rings)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of Rings by Length", x = "Length", y = "Count") +
  theme_minimal()


# 2. Distribution of classes along 'diameter'
ggplot(abalone, aes(x = diameter, fill = rings)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of Rings by Diameter", x = "Diameter", y = "Count") +
  theme_minimal()


# 3. Distribution of classes along 'whole_weight'
ggplot(abalone, aes(x = whole_weight, fill = rings)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of Rings by Whole Weight", x = "Whole Weight", y = "Count") +
  theme_minimal()



















# Exercise 2 

# His Code

# Slide 1
library(class)

# Read dataset
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")

# Rename columns
colnames(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")

# Add new column abalone$age.group with 3 values based on the number of rings
abalone$age.group <- cut(abalone$rings, breaks = c(0, 8, 11, 35), labels = c("young", "adult", "old"))

# Drop the sex column (categorical variable)
abalone.norm <- abalone[, -1]

# Slide 2
# Sample 2924 from 4177 (~70%)
set.seed(123)  # Set seed for reproducibility
s_abalone <- sample(nrow(abalone), 2924)

# Create train & test sets based on sampled indexes
abalone.train <- abalone[s_abalone, ]
abalone.test <- abalone[-s_abalone, ]

# Check structure and sizes
str(abalone.train)
str(abalone.test)

# Check if either dataset is empty
if (nrow(abalone.train) == 0 || nrow(abalone.test) == 0) {
  stop("One of the datasets is empty. Please check your data.")
}

# Prepare data for KNN
k = 5  # Set your value for k
train_data <- abalone.train[, c("length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight")]
test_data <- abalone.test[, c("length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight")]

KNNpred <- knn(train = train_data, test = test_data, cl = abalone.train$age.group, k = k)

contingency.table <- table(KNNpred, abalone.test$age.group)

# Print predictions and the contingency table
print(KNNpred)
print(contingency.table)

# Slide 3
# Assuming 'contingency.table' is defined and valid
contingency.matrix <- as.matrix(contingency.table)

# Check if abalone.test is not empty
if (nrow(abalone.test) == 0) {
  stop("The test set is empty. Please check your data.")
}

# Calculate initial accuracy
initial_accuracy <- sum(diag(contingency.matrix)) / length(abalone.test$age.group)
print(paste("Initial Accuracy:", initial_accuracy))

# Initialize accuracy vector
accuracy <- c()
ks <- c(35, 45, 55, 65, 75, 85, 95, 105)  # Using original k values

# Loop through each k value for KNN
for (k in ks) {
  # Ensure k does not exceed the number of training samples
  if (k > nrow(abalone.train)) {
    next  # Skip this iteration if k is too large
  }
  
  # KNN prediction
  KNNpred <- knn(train = train_data, test = test_data, cl = abalone.train$age.group, k = k)
  
  # Create confusion matrix
  cm <- as.matrix(table(Actual = abalone.test$age.group, Predicted = KNNpred, dnn = list('predicted', 'actual')))
  
  # Check if confusion matrix is valid
  if (nrow(cm) == 0 || ncol(cm) == 0) {
    next  # Skip this iteration if the confusion matrix is empty
  }
  
  # Calculate and store accuracy
  accuracy <- c(accuracy, sum(diag(cm)) / length(abalone.test$age.group))  # Use length of test group for total counts
}

# Plot accuracy vs k values
plot(ks, accuracy, type = "b", ylim = c(0.67, 0.69), xlab = "Number of Neighbors (k)", ylab = "Accuracy", main = "KNN Accuracy by k")















#Exercise 2:
#Repeat the kNN analysis using the iris dataset.




# Load necessary libraries
library(class)  # For kNN
library(caret)  # For data splitting

# Read dataset
data(iris)

# Rename columns (if necessary, but it's already named correctly)
# colnames(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

# Prepare the data: Create a stratified sample (~70%)
set.seed(123)  # Set seed for reproducibility
trainIndex <- createDataPartition(iris$Species, p = .7, 
                                  list = FALSE, 
                                  times = 1)

# Create train & test sets based on sampled indexes
irisTrain <- iris[trainIndex, ]
irisTest <- iris[-trainIndex, ]

# Check structure and sizes
str(irisTrain)
str(irisTest)

# Check if either dataset is empty
if (nrow(irisTrain) == 0 || nrow(irisTest) == 0) {
  stop("One of the datasets is empty. Please check your data.")
}

# Prepare data for KNN
k <- 3  # Set your value for k

train_data <- irisTrain[, -5]  # Exclude the Species column
test_data <- irisTest[, -5]    # Exclude the Species column

# Perform KNN
KNNpred <- knn(train = train_data, test = test_data, cl = irisTrain$Species, k = k)

# Create a contingency table
contingency.table <- table(KNNpred, irisTest$Species)

# Print predictions and the contingency table
print(KNNpred)
print(contingency.table)

# Calculate initial accuracy
contingency.matrix <- as.matrix(contingency.table)

# Check if irisTest is not empty
if (nrow(irisTest) == 0) {
  stop("The test set is empty. Please check your data.")
}

# Calculate initial accuracy
initial_accuracy <- sum(diag(contingency.matrix)) / length(irisTest$Species)
print(paste("Initial Accuracy:", initial_accuracy))

# Initialize accuracy vector
accuracy <- c()
ks <- seq(1, 15, by = 2)  # Using odd k values for diversity

# Loop through each k value for KNN
for (k in ks) {
  # Ensure k does not exceed the number of training samples
  if (k > nrow(irisTrain)) {
    next  # Skip this iteration if k is too large
  }
  
  # KNN prediction
  KNNpred <- knn(train = train_data, test = test_data, cl = irisTrain$Species, k = k)
  
  # Create confusion matrix
  cm <- as.matrix(table(Actual = irisTest$Species, Predicted = KNNpred))
  
  # Check if confusion matrix is valid
  if (nrow(cm) == 0 || ncol(cm) == 0) {
    next  # Skip this iteration if the confusion matrix is empty
  }
  
  # Calculate and store accuracy
  accuracy <- c(accuracy, sum(diag(cm)) / length(irisTest$Species))  # Use length of test group for total counts
}

# Plot accuracy vs k values
plot(ks, accuracy, type = "b", ylim = c(0, 1), xlab = "Number of Neighbors (k)", ylab = "Accuracy", main = "KNN Accuracy by k for Iris Dataset")








# Exercise 2 - KNN with Two Different Subsets of Features

# Load necessary libraries
library(class)  # For kNN
library(caret)  # For data splitting
library(ggplot2)  # For enhanced plotting

# Read dataset
data(iris)

# Prepare the data: Create a stratified sample (~70%)
set.seed(123)  # Set seed for reproducibility
trainIndex <- createDataPartition(iris$Species, p = .7, 
                                  list = FALSE, 
                                  times = 1)

# Create train & test sets based on sampled indexes
irisTrain <- iris[trainIndex, ]
irisTest <- iris[-trainIndex, ]

# Function to perform KNN with a given feature subset
perform_knn <- function(train_data, test_data, k_values) {
  accuracy <- c()  # Initialize accuracy vector
  
  # Loop through each k value for KNN
  for (k in k_values) {
    # KNN prediction
    KNNpred <- knn(train = train_data, test = test_data, cl = irisTrain$Species, k = k)
    
    # Create confusion matrix
    cm <- as.matrix(table(Actual = irisTest$Species, Predicted = KNNpred))
    
    # Calculate and store accuracy
    accuracy <- c(accuracy, sum(diag(cm)) / nrow(test_data))  # Use length of test group for total counts
  }
  
  return(list(accuracy = accuracy, cm = cm))
}

# Define k values
ks <- seq(1, 15, by = 2)  # Using odd k values for diversity

# Subset 1: Sepal.Length and Sepal.Width
train_data1 <- irisTrain[, c("Sepal.Length", "Sepal.Width")]
test_data1 <- irisTest[, c("Sepal.Length", "Sepal.Width")]

# Perform KNN for Subset 1
results1 <- perform_knn(train_data1, test_data1, ks)

# Subset 2: Petal.Length and Petal.Width
train_data2 <- irisTrain[, c("Petal.Length", "Petal.Width")]
test_data2 <- irisTest[, c("Petal.Length", "Petal.Width")]

# Perform KNN for Subset 2
results2 <- perform_knn(train_data2, test_data2, ks)





# Exercise 2 - Bullet 3: Compare models using contingency tables and accuracy plots

# Print contingency tables for both subsets
cat("Contingency Table for Subset 1:\n")
print(results1$cm)

cat("\nContingency Table for Subset 2:\n")
print(results2$cm)

# Combine accuracy results into a data frame for ggplot
accuracy_df <- data.frame(k = rep(ks, 2), 
                          Accuracy = c(results1$accuracy, results2$accuracy), 
                          Subset = rep(c("Sepal Features", "Petal Features"), each = length(ks)))

# Create a combined accuracy plot using ggplot2
ggplot(accuracy_df, aes(x = k, y = Accuracy, color = Subset)) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  labs(title = "KNN Accuracy Comparison", 
       x = "Number of Neighbors (k)", 
       y = "Accuracy") +
  theme_minimal()
































# Exercise 3 
# Slide 1

# Plot iris petal length vs. petal width, color by species
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()
# set seed for random number generator
set.seed(123)
# run k-means
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()



# Exercise 3
# Slide 2
wss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris.km$tot.withinss)
}
plot(ks,wss,type = "b")


# Exercise 3
# Slide 3

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "setosa"
labeled.clusters[labeled.clusters==2] <- "versivolor"
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, iris[,5])









# Exercise 3
# BUllet 1
# Run k-means analysis using the abalone & iris datasets.



# Exercise 3
# Bullet 1
# Run k-means analysis using the abalone & iris datasets.

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the iris dataset
data(iris)

# Plot iris petal length vs. petal width, color by species
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

# Set seed for random number generator
set.seed(123)

# Run k-means for the iris dataset (assuming 3 clusters)
iris.km <- kmeans(iris[, -5], centers = 3)  # Exclude Species column
assigned.clusters <- as.factor(iris.km$cluster)

# Visualize clustering results for the Iris dataset
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point() +
  labs(title = "K-means Clustering of Iris Dataset")

# Exercise 3 - Slide 2: Calculate WSS for different k values
wss <- c()
ks <- c(2, 3, 4, 5)
for (k in ks) {
  iris.km <- kmeans(iris[, -5], centers = k)
  wss <- c(wss, iris.km$tot.withinss)
}
plot(ks, wss, type = "b", 
     main = "Total Within-Cluster Sum of Squares for Iris Dataset",
     xlab = "Number of Clusters (k)",
     ylab = "WSS")

# Load the Abalone dataset from the .data file
abalone <- read.table("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab2Part2/abalone/abalone.data", 
                      sep = ",", header = FALSE)

# Assign appropriate column names based on the abalone.names file
colnames(abalone) <- c("Sex", "Length", "Diameter", "Height", 
                       "WholeWeight", "ShuckedWeight", 
                       "VisceraWeight", "ShellWeight", "Rings")

# Display the structure of the dataset
str(abalone)

# Prepare the abalone dataset: Exclude the non-numeric column 'Sex'
abalone_numeric <- abalone %>% select(-Sex)

# Optionally normalize the data
abalone_numeric <- scale(abalone_numeric)

# Set seed for random number generator
set.seed(123)

# Run k-means for the abalone dataset (assuming 3 clusters)
abalone.km <- kmeans(abalone_numeric, centers = 3)
assigned.clusters_abalone <- as.factor(abalone.km$cluster)

# Visualize clustering results for the Abalone dataset
ggplot(abalone, aes(x = Length, y = Diameter, colour = assigned.clusters_abalone)) +
  geom_point() +
  labs(title = "K-means Clustering of Abalone Dataset")

# Exercise 3 - Slide 3: Label clusters for the Iris dataset
labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters == 1] <- "setosa"
labeled.clusters[labeled.clusters == 2] <- "versicolor"
labeled.clusters[labeled.clusters == 3] <- "virginica"

# Compare labeled clusters with actual species
comparison_table <- table(labeled.clusters, iris[, 5])
print(comparison_table)

# If you want to do the same for the Abalone dataset, you might label the clusters
# based on your specific analysis needs (if you have labels for them).






# Exercise 3
# Bullet 2
# Try different values of k for both


# Exercise 3
# Bullet 2
# Try different values of k for both the Iris and Abalone datasets

# Load necessary libraries
library(ggplot2)
library(dplyr)

#### ---- Bullet 1 Code for Iris Dataset ---- ####

# Load the Iris dataset
data(iris)

# Plot iris petal length vs. petal width, color by species
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

# Set seed for random number generator
set.seed(123)

# Run k-means for the Iris dataset (assuming 3 clusters)
iris.km <- kmeans(iris[, -5], centers = 3)  # Exclude Species column
assigned.clusters <- as.factor(iris.km$cluster)

# Visualize clustering results for the Iris dataset
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point() +
  labs(title = "K-means Clustering of Iris Dataset")

#### ---- Bullet 2 Code for Iris Dataset ---- ####

# WSS Calculation for different values of k (Iris Dataset)
wss_iris <- c()
ks_iris <- c(2, 3, 4, 5, 6)  # Trying multiple k values

for (k in ks_iris) {
  iris.km <- kmeans(iris[, -5], centers = k)  # Exclude Species column
  wss_iris <- c(wss_iris, iris.km$tot.withinss)
}

# Plot WSS to find the optimal k
plot(ks_iris, wss_iris, type = "b", 
     main = "WSS for different k values (Iris Dataset)", 
     xlab = "Number of Clusters (k)", 
     ylab = "Total Within-Cluster Sum of Squares")

# Visualizing Clustering Results for different k values (Iris Dataset)
for (k in ks_iris) {
  iris.km <- kmeans(iris[, -5], centers = k)  # Exclude Species column
  assigned.clusters <- as.factor(iris.km$cluster)
  
  # Plot results for each k
  ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
    geom_point() +
    labs(title = paste("K-means Clustering for Iris Dataset (k =", k, ")")) +
    theme_minimal()
}

#### ---- Bullet 1 Code for Abalone Dataset ---- ####

# Load the Abalone dataset
abalone <- read.table("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab2Part2/abalone/abalone.data", 
                      sep = ",", header = FALSE)

# Assign appropriate column names
colnames(abalone) <- c("Sex", "Length", "Diameter", "Height", 
                       "WholeWeight", "ShuckedWeight", 
                       "VisceraWeight", "ShellWeight", "Rings")

# Display the structure of the dataset
str(abalone)

# Prepare the abalone dataset by excluding the non-numeric column 'Sex'
abalone_numeric <- abalone %>% select(-Sex)

# Optionally normalize the data
abalone_numeric <- scale(abalone_numeric)

# Set seed for random number generator
set.seed(123)

# Run k-means for the Abalone dataset (assuming 3 clusters)
abalone.km <- kmeans(abalone_numeric, centers = 3)
assigned.clusters_abalone <- as.factor(abalone.km$cluster)

# Visualize clustering results for the Abalone dataset
ggplot(abalone, aes(x = Length, y = Diameter, colour = assigned.clusters_abalone)) +
  geom_point() +
  labs(title = "K-means Clustering of Abalone Dataset")

#### ---- Bullet 2 Code for Abalone Dataset ---- ####

# WSS Calculation for different values of k (Abalone Dataset)
wss_abalone <- c()
ks_abalone <- c(2, 3, 4, 5, 6)  # Trying multiple k values

for (k in ks_abalone) {
  abalone.km <- kmeans(abalone_numeric, centers = k)
  wss_abalone <- c(wss_abalone, abalone.km$tot.withinss)
}

# Plot WSS to find the optimal k
plot(ks_abalone, wss_abalone, type = "b", 
     main = "WSS for different k values (Abalone Dataset)", 
     xlab = "Number of Clusters (k)", 
     ylab = "Total Within-Cluster Sum of Squares")

# Visualizing Clustering Results for different k values (Abalone Dataset)
for (k in ks_abalone) {
  abalone.km <- kmeans(abalone_numeric, centers = k)
  assigned.clusters_abalone <- as.factor(abalone.km$cluster)
  
  # Plot results for each k
  ggplot(abalone, aes(x = Length, y = Diameter, colour = assigned.clusters_abalone)) +
    geom_point() +
    labs(title = paste("K-means Clustering for Abalone Dataset (k =", k, ")")) +
    theme_minimal()
}








# Exercise 3
# bullet 3
# • Evaluate clustering using Plot the best clustering output for both.

# Install necessary libraries if not already installed
# install.packages("factoextra")

# Load necessary library
library(factoextra)

# ------------ Clustering and Plotting for Iris Dataset ------------

# Iris dataset: Load data
iris_data <- iris[, -5]  # Remove the Species column for clustering

# Set the best k for Iris (assumed to be k = 3 based on WSS plot)
best_k_iris <- 3

# Perform K-means clustering with the best k for Iris
set.seed(123)  # Setting seed for reproducibility
iris_kmeans_best <- kmeans(iris_data, centers = best_k_iris)

# Plot the best clustering for Iris dataset
fviz_cluster(iris_kmeans_best, data = iris_data, 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),  # Custom color palette
             geom = "point",  # Use points for plotting
             ellipse.type = "euclid",  # Add ellipses for clusters
             ggtheme = theme_minimal(),  # Minimal theme for better visualization
             main = "Best Clustering for Iris Dataset (k = 3)")

# ------------ Clustering and Plotting for Abalone Dataset ------------

# Assuming abalone_data_scaled is already scaled
# Abalone dataset: Load or use preprocessed data (abalone_data_scaled)

# Set the best k for Abalone (assumed to be k = 4 based on WSS plot)
best_k_abalone <- 4

# Perform K-means clustering with the best k for Abalone
set.seed(123)  # Setting seed for reproducibility
abalone_kmeans_best <- kmeans(abalone_data_scaled, centers = best_k_abalone)

# Plot the best clustering for Abalone dataset
fviz_cluster(abalone_kmeans_best, data = abalone_data_scaled, 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),  # Custom color palette
             geom = "point",  # Use points for plotting
             ellipse.type = "euclid",  # Add ellipses for clusters
             ggtheme = theme_minimal(),  # Minimal theme for better visualization
             main = "Best Clustering for Abalone Dataset (k = 4)")
