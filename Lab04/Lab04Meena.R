#Lab 04
#Meena Mall


# Lab 04
# Meena Mall



#Compute the PCs and plot the dataset using the 1st and 2nd PC.



# Step 1: Load the data with assigned column names
col_names <- c("Class", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash", "Magnesium", 
               "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols", "Proanthocyanins", 
               "Color_Intensity", "Hue", "OD280_OD315", "Proline")

# Load the dataset and assign column names
wine_data <- read.csv("wine.data", header = FALSE, col.names = col_names)

# Step 2: Remove the Class column for PCA as it is categorical
wine_data_numeric <- wine_data[, -1]

# Step 3: Compute the Principal Components
pca_result <- prcomp(wine_data_numeric, center = TRUE, scale. = TRUE)

# Step 4: Plot the dataset using the first and second principal components
# Extract the first two principal components
pc_data <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2], Class = wine_data$Class)

# Load ggplot2 for visualization
library(ggplot2)

# Plot the first two PCs
ggplot(pc_data, aes(x = PC1, y = PC2, color = factor(Class))) +
  geom_point() +
  labs(title = "Wine Dataset PCA Plot", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal() +
  scale_color_discrete(name = "Wine Class")




###############################################




#● Identify the variables that contribute the most to the 1st PC.



# Step 5: Identify the variables that contribute the most to the 1st PC
# Extract the loadings (rotation) for PC1
loadings_PC1 <- pca_result$rotation[, 1]

# Create a data frame of variable names and their absolute contributions to PC1
loadings_df <- data.frame(Variable = colnames(wine_data_numeric), Contribution = abs(loadings_PC1))

# Sort by contribution in descending order
loadings_df <- loadings_df[order(-loadings_df$Contribution), ]

# View the top contributors to PC1
print("Top variables contributing to the 1st Principal Component:")
print(loadings_df)








#####################################################


#● Train a classifier model to predict wine type using the 13 attributes.

# Install randomForest package if not already installed
install.packages("randomForest")
library(randomForest)
library(caret)

# Ensure 'Class' is a factor for classification
wine_data$Class <- as.factor(wine_data$Class)

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(wine_data$Class, p = 0.7, list = FALSE)

train_data <- wine_data[trainIndex, ]
test_data <- wine_data[-trainIndex, ]

# Train a Random Forest model to predict wine type
rf_model <- randomForest(Class ~ ., data = train_data, importance = TRUE)

# Print the model summary
print(rf_model)

# Predict on the test data
predictions <- predict(rf_model, newdata = test_data)

# Ensure the predictions are factors with the same levels as the actual data
predictions <- factor(predictions, levels = levels(test_data$Class))

# Confusion matrix to evaluate the performance
confusion_matrix <- confusionMatrix(predictions, test_data$Class)

# Print the confusion matrix and accuracy
print(confusion_matrix)

# Plot variable importance
importance(rf_model)
varImpPlot(rf_model)







##############################################################


#Train a classifier model to predict wine type using the data projected into the first 3 PCs.

# Step 1: Perform PCA on the dataset (excluding the 'Class' column)
# Scale the data and perform PCA
wine_data_no_class <- wine_data[, -which(names(wine_data) == "Class")]  # Remove Class column for PCA
pca_result <- prcomp(wine_data_no_class, center = TRUE, scale. = TRUE)

# Get the first 3 principal components (PCs)
pca_data <- as.data.frame(pca_result$x[, 1:3])  # Extract the first 3 PCs

# Add the 'Class' column back to the PCA data
pca_data$Class <- wine_data$Class

# Step 2: Split the PCA-transformed data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(pca_data$Class, p = 0.7, list = FALSE)
train_data_pca <- pca_data[trainIndex, ]
test_data_pca <- pca_data[-trainIndex, ]

# Step 3: Train a Random Forest model using the first 3 PCs
rf_model_pca <- randomForest(Class ~ ., data = train_data_pca, importance = TRUE)

# Print the model summary
print(rf_model_pca)

# Step 4: Make predictions on the test data
predictions_pca <- predict(rf_model_pca, newdata = test_data_pca)

# Ensure the predictions are factors with the same levels as the actual data
predictions_pca <- factor(predictions_pca, levels = levels(test_data_pca$Class))

# Step 5: Confusion matrix to evaluate the performance
confusion_matrix_pca <- confusionMatrix(predictions_pca, test_data_pca$Class)

# Print the confusion matrix and accuracy
print(confusion_matrix_pca)

# Step 6: Plot variable importance based on the first 3 PCs
importance(rf_model_pca)
varImpPlot(rf_model_pca)







########################################################################



#Drop the variables least contributing to the 1st PC and rerun PCA.

# Step 1: Perform PCA on the dataset (excluding the 'Class' column)
wine_data_no_class <- wine_data[, -which(names(wine_data) == "Class")]  # Remove Class column for PCA
pca_result <- prcomp(wine_data_no_class, center = TRUE, scale. = TRUE)

# Step 2: Check the loadings of each variable on the first principal component (PC1)
loadings_pc1 <- pca_result$rotation[, 1]  # Loadings for the first PC

# Step 3: Identify variables that contribute the least to the first principal component
# Set a threshold to consider small contributions (e.g., the lowest 25% of contributions)
threshold <- quantile(abs(loadings_pc1), 0.25)  # Get the bottom 25% of contributions

# Step 4: Drop variables with loadings below the threshold
vars_to_drop <- names(loadings_pc1[abs(loadings_pc1) < threshold])
wine_data_reduced <- wine_data[, !(names(wine_data) %in% vars_to_drop)]  # Remove the least contributing variables

# Step 5: Perform PCA again on the reduced dataset
pca_result_reduced <- prcomp(wine_data_reduced[, -which(names(wine_data_reduced) == "Class")], center = TRUE, scale. = TRUE)

# Step 6: Print the summary of the new PCA to check how the variance is explained by the first components
summary(pca_result_reduced)

# You can also plot the cumulative variance to see how well the reduced PCA explains the data
plot(cumsum(pca_result_reduced$sdev^2) / sum(pca_result_reduced$sdev^2), xlab = "Number of Principal Components", ylab = "Cumulative Proportion of Variance")












############################################################


# Train a classifier model to predict wine type using the data projected into the first 3 PCs
# after rerunning PCA.



# Step 1: Extract the first 3 principal components (PCs) from the reduced PCA result
pca_data_3pcs <- pca_result_reduced$x[, 1:3]  # First 3 principal components

# Step 2: Add the 'Class' column back to the dataset (wine type) for prediction
pca_data_with_class <- cbind(pca_data_3pcs, Class = wine_data$Class)

# Step 3: Split the dataset into training and testing sets
set.seed(42)  # For reproducibility
train_index <- sample(1:nrow(pca_data_with_class), size = 0.7 * nrow(pca_data_with_class))  # 70% for training
train_data <- pca_data_with_class[train_index, ]
test_data <- pca_data_with_class[-train_index, ]

# Convert to data frame (this step ensures compatibility with the lda function)
train_data <- as.data.frame(train_data)
test_data <- as.data.frame(test_data)

# Step 4: Convert 'Class' column to a factor in both train_data and test_data
train_data$Class <- as.factor(train_data$Class)
test_data$Class <- as.factor(test_data$Class)

# Step 5: Train a classifier (LDA classifier in this case)
library(MASS)  # For the 'lda' function, which is a Linear Discriminant Analysis classifier
model <- lda(Class ~ PC1 + PC2 + PC3, data = train_data)  # LDA classifier on the first 3 PCs

# Step 6: Make predictions on the test set
predictions <- predict(model, newdata = test_data)$class

# Step 7: Convert predictions to a factor with the same levels as 'Class' in test_data
predictions <- factor(predictions, levels = levels(test_data$Class))

# Step 8: Evaluate the model performance
library(caret)  # For confusionMatrix function
confusion <- confusionMatrix(predictions, test_data$Class)
print(confusion)

# Optionally, visualize the performance or plot the results

















##############################################################


#Compare the 3 classification models using contingency tables and prevision/recall/f1
#metrics




# Function to calculate Precision, Recall, and F1-Score
calc_metrics <- function(conf_matrix) {
  # Ensure that all classes are included in the matrix (even if they aren't present in the confusion matrix)
  all_classes <- levels(factor(c(conf_matrix[, 1], conf_matrix[, 2])))  # Combine predicted and actual values
  precision <- diag(conf_matrix) / colSums(conf_matrix)
  recall <- diag(conf_matrix) / rowSums(conf_matrix)
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  # Add NAs for any missing classes to ensure consistency
  precision <- precision[match(all_classes, names(precision))]
  recall <- recall[match(all_classes, names(recall))]
  f1 <- f1[match(all_classes, names(f1))]
  
  # Return the metrics as a list
  list(precision = precision, recall = recall, f1 = f1)
}

# Calculate metrics for all models
lda_metrics <- calc_metrics(lda_conf_matrix)
logistic_metrics <- calc_metrics(logistic_conf_matrix)
tree_metrics <- calc_metrics(tree_conf_matrix)

# Ensure the lengths of metrics match for all models
metrics_list <- list(lda_metrics, logistic_metrics, tree_metrics)
names(metrics_list) <- c("LDA", "Logistic Regression", "Decision Tree")

# Combine metrics into a data frame, filtering out NAs
metrics_df <- data.frame(
  Model = character(),
  Metric = character(),
  Value = numeric()
)

for (model_name in names(metrics_list)) {
  model_metrics <- metrics_list[[model_name]]
  for (metric_name in names(model_metrics)) {
    metric_values <- model_metrics[[metric_name]]
    valid_values <- metric_values[!is.na(metric_values)]  # Remove NAs
    metrics_df <- rbind(metrics_df, data.frame(
      Model = rep(model_name, length(valid_values)),
      Metric = rep(metric_name, length(valid_values)),
      Value = valid_values
    ))
  }
}

# Print out the metrics data frame
print(metrics_df)

# Plot Precision, Recall, F1-Score for all models
library(ggplot2)

ggplot(metrics_df, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Metric) +
  theme_minimal() +
  labs(title = "Model Metrics Comparison", y = "Score", x = "Model")
