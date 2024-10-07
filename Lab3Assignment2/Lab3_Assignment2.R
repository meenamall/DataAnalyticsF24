#First Slide
#1. Derive 2 subsets each for a particular region


setwd("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab3Assignment2")

epi_data <- read.csv("epi2024results_DA_F24_lab03.csv")

head(epi_data)


# Subset for "Latin America & Caribbean"
latin_america_data <- subset(epi_data, region == "Latin America & Caribbean")

# Subset for "Eastern Europe"
eastern_europe_data <- subset(epi_data, region == "Eastern Europe")

# Check the first few rows of each subset to make sure it worked
head(latin_america_data)
head(eastern_europe_data)







#First Slide
#1.1. Plot histograms for a variable of your choice for both regions with density lines overlayed

install.packages("ggplot2")
library(ggplot2)



# Latin America & Caribbean histogram with density line
ggplot(latin_america_data, aes(x = EPI)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1.2) +
  ggtitle("EPI Distribution - Latin America & Caribbean") +
  xlab("EPI") +
  ylab("Density") +
  theme_minimal()


# Eastern Europe histogram with density line
ggplot(eastern_europe_data, aes(x = EPI)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1.2) +
  ggtitle("EPI Distribution - Eastern Europe") +
  xlab("EPI") +
  ylab("Density") +
  theme_minimal()






#First Slide
#1.2. Plot QQ plots for both variables compared to known probability distributions.

# Load required libraries
library(ggplot2)


# QQ plot for Latin America & Caribbean
ggplot(latin_america_data, aes(sample = EPI)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("QQ Plot - Latin America & Caribbean") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

# QQ plot for Eastern Europe
ggplot(eastern_europe_data, aes(sample = EPI)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  ggtitle("QQ Plot - Eastern Europe") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()



#FIRST SLIDE DONE!!!



##########################################################




#Second Slide

#2.1. Choose a subset of 5 variables (excluding EPI) and using the formula EPI~VAR1+VAR2+VAR3+
#VAR4+VAR5, fit a linear model and identify which variable most significantly influences EPI. Plot that variable
#with another and overlay the fitted line.

# Load necessary libraries
library(ggplot2)

# Fit the linear model
model <- lm(EPI ~ PAR + BDH + TBN + PSU + CCH, data = epi_data)

summary(model)

# Load necessary libraries
library(ggplot2)

# Plot CCH against EPI
ggplot(epi_data, aes(x = CCH, y = EPI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "EPI vs CCH",
       x = "Climate Change Health Score (CCH)",
       y = "EPI")




# Second Slide
# Second BUllet
# 2.2. Repeat the previous model with a subset of 1 region and in 1-2 sentences explain which model is a
#better fit and why you think that is the case.


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Filter data for Latin America & Caribbean
latin_america_data <- epi_data %>% filter(region == "Latin America & Caribbean")

# Fit the linear model using the same variables as before
model_latin_america <- lm(EPI ~ PAR + BDH + TBN + PSU + CCH, data = latin_america_data)

# Summary of the model
summary(model_latin_america)




#Paragraph
#The overall model has a higher R-squared value (0.8418) than the Latin America & Caribbean model (0.5481), indicating that it fits the data better and explains more variation in EPI scores. 
#While the regional model offers useful insights, the overall model is more effective for understanding environmental performance indicators.










##########################################################

# Slide 3
# Bullet 1

#3.1. Choose a subset of 5 variables and filter the subset by region keeping 3 regions out of 8 (representing 3
#classes), then train a kNN model to predict the region based on these variables. Evaluate the model using a
#contingency matrix and calculate the accuracy of correct classifications.


# Step 1: Filter the dataset for the chosen regions
selected_regions <- c("Southern Asia", "Eastern Europe", "Latin America & Caribbean")
filtered_data <- epi_data %>% filter(region %in% selected_regions)

# Step 2: Select the subset of 5 variables and the class label (region)
subset_data <- filtered_data %>% select(EPI, PAR, BDH, TBN, PSU, CCH, region)

# Step 3: Remove rows with missing values in the subset_data
subset_data <- na.omit(subset_data)  # Remove rows with NAs

# Ensure the number of rows after removing NAs is consistent
if (nrow(subset_data) == 0) {
  stop("No data available after removing missing values.")
}

# Step 4: Prepare the data for kNN
# Get class labels before normalizing
class_labels <- subset_data$region  # Get the class labels

# Normalizing the data (excluding the region)
normalized_data <- scale(subset_data[, -ncol(subset_data)])  # Exclude the last column (region)

# Step 5: Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(normalized_data), size = 0.7 * nrow(normalized_data))  # 70% for training

# Subset data and labels according to the training indices
train_data <- normalized_data[train_index, ]
test_data <- normalized_data[-train_index, ]
train_labels <- class_labels[train_index]
test_labels <- class_labels[-train_index]

# Check lengths to ensure they match
cat("Length of training data:", nrow(train_data), "\n")  # Number of rows in training data
cat("Length of training labels:", length(train_labels), "\n")  # Number of training labels



# Step 6: Load the required library for kNN
library(class)  # Ensure this is loaded for the kNN function

# Step 7: Train the kNN model and make predictions
k_value <- 3  # You can choose an appropriate value for k
predictions <- knn(train_data, test_data, train_labels, k = k_value)

# Step 8: Create a contingency matrix
contingency_matrix <- table(Predicted = predictions, Actual = test_labels)
print("Contingency Matrix:")
print(contingency_matrix)

# Step 9: Calculate accuracy
accuracy <- sum(diag(contingency_matrix)) / sum(contingency_matrix)
cat("Accuracy of the kNN model:", accuracy * 100, "%\n")




# Slide 3
# Bullet 2!

#3.2. Repeat the previous model with the same variables for another set of 3 other regions and evaluate. In 1-
#2 sentences explain which model is better and why you think that is the case.


# Bullet 3.2

# Step 1: Choose a new set of 3 regions
new_selected_regions <- c("North America", "Sub-Saharan Africa", "Western Europe")  # Example set


# Step 2: Filter the dataset for the new chosen regions
new_filtered_data <- epi_data %>% filter(region %in% new_selected_regions)


# Step 3: Select the same 5 variables and the class label (region)
new_subset_data <- new_filtered_data %>% select(EPI, PAR, BDH, TBN, PSU, CCH, region)

# Step 4: Remove rows with missing values in the new_subset_data
new_subset_data <- na.omit(new_subset_data)

# Step 5: Prepare the data for kNN
new_class_labels <- new_subset_data$region  # Get the class labels
new_normalized_data <- scale(new_subset_data[, -ncol(new_subset_data)])  # Normalize data excluding the region column


# Step 6: Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
new_train_index <- sample(1:nrow(new_normalized_data), size = 0.7 * nrow(new_normalized_data))  # 70% for training

# Subset data and labels for training/testing sets
new_train_data <- new_normalized_data[new_train_index, ]
new_test_data <- new_normalized_data[-new_train_index, ]
new_train_labels <- new_class_labels[new_train_index]
new_test_labels <- new_class_labels[-new_train_index]

# Step 7: Train the kNN model and make predictions
new_predictions <- knn(new_train_data, new_test_data, new_train_labels, k = 3)

# Step 8: Create a contingency matrix
new_contingency_matrix <- table(Predicted = new_predictions, Actual = new_test_labels)
print("Contingency Matrix for new regions:")
print(new_contingency_matrix)

# Step 9: Calculate accuracy
new_accuracy <- sum(diag(new_contingency_matrix)) / sum(new_contingency_matrix)
cat("Accuracy of the new kNN model:", new_accuracy * 100, "%\n")

# Step 10: Compare the two models' accuracies
cat("Previous model accuracy:", accuracy * 100, "%\n")
cat("New model accuracy:", new_accuracy * 100, "%\n")


#The new model performed better than the previous one, with a perfect accuracy of 100%. This might be because the regions chosen (North America, Sub-Saharan Africa, and Western Europe) have more noticeable differences in their environmental data, which made it easier for the kNN model to correctly classify them compared to the regions used in the previous model.






#Slide 4
#Bullet 1

#Clustering (3%)
#Using the EPI results dataset to perform the following:
  #1. Fit a k-means model for a subset of 5 variables for 2 different groups of regions (3 each)
#1.1. Compare the performance of the models using their within cluster sum of squares.



# Load necessary libraries
library(dplyr)

# Step 1: Choose two groups of 3 regions
group1_regions <- c("Latin America & Caribbean", "Greater Middle East", "Southern Asia")
group2_regions <- c("Asia-Pacific", "Former Soviet States", "Eastern Europe")

# Step 2: Filter the dataset for each group of regions
group1_data <- epi_data %>% filter(region %in% group1_regions)
group2_data <- epi_data %>% filter(region %in% group2_regions)

# Step 3: Select a subset of 5 variables for clustering
variables_subset <- c("EPI", "PAR", "BDH", "TBN", "PSU")  # Use the correct variable names

# Step 4: Subset the data and remove rows with NA values
group1_subset <- group1_data %>% select(all_of(variables_subset)) %>% na.omit()
group2_subset <- group2_data %>% select(all_of(variables_subset)) %>% na.omit()

# Check number of distinct data points
print(paste("Number of distinct points in Group 2:", nrow(group2_subset)))

# Step 5: Fit k-means models for both groups with a set number of clusters (k)
set.seed(123)  # For reproducibility
k_group1 <- 3  # Set the number of clusters for group 1

# Fit k-means model for group 1
kmeans_group1 <- kmeans(group1_subset, centers = k_group1, nstart = 25)
wcss_group1 <- kmeans_group1$tot.withinss  # Get the within-cluster sum of squares

# For group 2, adjust the number of clusters based on distinct points
k_group2 <- min(3, nrow(group2_subset))  # Adjust cluster count to number of distinct points

if (nrow(group2_subset) > 0) {
  # Fit k-means model for group 2 if there are data points
  kmeans_group2 <- kmeans(group2_subset, centers = k_group2, nstart = 25)
  wcss_group2 <- kmeans_group2$tot.withinss  # Get the within-cluster sum of squares
  print(paste("WCSS for Group 2:", wcss_group2))
} else {
  print("No data points available for Group 2.")
}

# Step 6: Compare the performance of the models using their within cluster sum of squares
print(paste("WCSS for Group 1:", wcss_group1))









#Slide 4
# Bullet 2
#1.2. In a loop fit kmeans models for both subsets using multiple values of k. Plot WCSS across k values. In
#1-2 sentences explain which model is better and why you think that is the case


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 1: Define a range of k values to test
k_values <- 1:10  # You can adjust this range if necessary

# Initialize vectors to store WCSS for each k value
wcss_group1 <- numeric(length(k_values))
wcss_group2 <- numeric(length(k_values))

# Fit k-means models for both groups using multiple k values
for (k in k_values) {
  if (nrow(group1_subset) > 0) {
    kmeans_group1 <- kmeans(group1_subset, centers = k, nstart = 25)
    wcss_group1[k] <- kmeans_group1$tot.withinss
  }
  
  if (nrow(group2_subset) > 0) {
    kmeans_group2 <- kmeans(group2_subset, centers = k, nstart = 25)
    wcss_group2[k] <- kmeans_group2$tot.withinss
  }
}

# Step 2: Create a data frame for plotting
wcss_data <- data.frame(
  k = rep(k_values, 2),
  WCSS = c(wcss_group1, wcss_group2),
  Group = rep(c("Group 1", "Group 2"), each = length(k_values))
)

# Step 3: Plot WCSS across k values
ggplot(wcss_data, aes(x = k, y = WCSS, color = Group)) +
  geom_line() +
  geom_point() +
  labs(title = "WCSS Across Different k Values",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal()

# Optionally, print the best k for each group based on the elbow method
best_k_group1 <- which.min(diff(diff(wcss_group1)))
best_k_group2 <- which.min(diff(diff(wcss_group2)))

print(paste("Best k for Group 1:", best_k_group1))
print(paste("Best k for Group 2:", best_k_group2))

