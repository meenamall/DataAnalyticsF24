#Meena

#Lab 05





#Part 1 

# Wine Dataset


#Train 2 SVM classifiers to predict the type of wine using a subset of the other 13
#variables. You may choose the subset based on previous analysis. One using a linear
#kernel and another of your choice.


# Load the required library
library(e1071)

# Set the working directory to the folder containing the 'wine' folder
setwd("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab05/wine")

# Load the wine dataset (wine.data is the main data file)
wine_data <- read.csv("wine.data.data", header = FALSE)

# Assign column names based on the 'wine.names' file (adjust if necessary)
colnames(wine_data) <- c("Type", paste0("Feature", 1:13))

# Prepare predictors (X) and response variable (Y)
X <- wine_data[, 2:14]  # Features (columns 2 to 14)
Y <- as.factor(wine_data$Type)  # Type (column 1), convert to factor for classification

# Check the dataset
summary(wine_data)

# Train an SVM with a Linear Kernel using tune.svm
linear_tune <- tune.svm(
  x = X, y = Y,
  kernel = "linear",
  cost = 10^(-1:2)  # Range of cost values
)

# Print results and best model for linear kernel
print(linear_tune)
best_linear <- linear_tune$best.model
summary(best_linear)

# Train an SVM with an RBF Kernel using tune.svm
rbf_tune <- tune.svm(
  x = X, y = Y,
  kernel = "radial",
  cost = 10^(-1:2),  # Range of cost values
  gamma = c(0.01, 0.1, 1)  # Range of gamma values
)

# Print results and best model for RBF kernel
print(rbf_tune)
best_rbf <- rbf_tune$best.model
summary(best_rbf)

# Evaluate both models on the training dataset
linear_predictions <- predict(best_linear, X)
rbf_predictions <- predict(best_rbf, X)

# Confusion Matrix for Linear Kernel
cat("Confusion Matrix - Linear Kernel:\n")
print(table(Actual = Y, Predicted = linear_predictions))

# Confusion Matrix for RBF Kernel
cat("Confusion Matrix - RBF Kernel:\n")
print(table(Actual = Y, Predicted = rbf_predictions))









###################################################


#Choose another classification method (kNN, NaiveBayes, etc.) and train a classifier
#based on the same features.


# Load required library
library(class)

# Load the wine dataset (assuming it's already preprocessed into X and Y)
# X: Feature matrix
# Y: Target variable (class labels)

# Scale the feature matrix
X_scaled <- scale(X)

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)  # For reproducibility
train_idx <- sample(1:nrow(X), size = 0.7 * nrow(X))  # Training indices
X_train <- X_scaled[train_idx, ]
X_test <- X_scaled[-train_idx, ]
Y_train <- Y[train_idx]
Y_test <- Y[-train_idx]

# Train a kNN classifier
k <- 5  # Number of neighbors (can be tuned)
knn_predictions <- knn(train = X_train, test = X_test, cl = Y_train, k = k)

# Evaluate the kNN classifier
# Generate a confusion matrix
confusion_matrix_knn <- table(Predicted = knn_predictions, Actual = Y_test)
print("Confusion Matrix (kNN):")
print(confusion_matrix_knn)

# Calculate accuracy
accuracy_knn <- sum(diag(confusion_matrix_knn)) / sum(confusion_matrix_knn)
print(paste("Accuracy (kNN):", accuracy_knn))






#########################################################



#Compare the performance of the 2 models (Precision, Recall, F1)





# Load necessary libraries
library(caret)  # For calculating Precision, Recall, F1-Score
library(class)   # For kNN

# Assuming X (features) and Y (target) are already defined

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)  # For reproducibility
train_idx <- sample(1:nrow(X), size = 0.7 * nrow(X))  # Training indices
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
Y_train <- Y[train_idx]
Y_test <- Y[-train_idx]

# 1. Train Logistic Regression model
log_reg_model <- glm(Y_train ~ ., data = X_train, family = binomial)

# Generate Logistic Regression predictions
log_reg_predictions <- predict(log_reg_model, newdata = X_test, type = "response")
log_reg_predictions <- ifelse(log_reg_predictions > 0.5, 1, 0)  # Convert probabilities to class labels

# Ensure the levels of predictions and actual labels match
log_reg_predictions <- factor(log_reg_predictions, levels = levels(Y_test))

# 2. Train kNN model
X_scaled <- scale(X)  # Scale the features (important for kNN)
X_train_scaled <- X_scaled[train_idx, ]
X_test_scaled <- X_scaled[-train_idx, ]

k <- 5  # Number of neighbors for kNN
knn_predictions <- knn(train = X_train_scaled, test = X_test_scaled, cl = Y_train, k = k)

# Ensure the levels of kNN predictions and actual labels match
knn_predictions <- factor(knn_predictions, levels = levels(Y_test))

# 3. Calculate Precision, Recall, and F1-Score for Logistic Regression using confusionMatrix
conf_matrix_log_reg <- confusionMatrix(log_reg_predictions, Y_test)
cat("Logistic Regression Metrics:\n")
cat("Precision: ", conf_matrix_log_reg$byClass["Pos Pred Value"], "\n")
cat("Recall: ", conf_matrix_log_reg$byClass["Sensitivity"], "\n")
cat("F1-Score: ", conf_matrix_log_reg$byClass["F1"], "\n")

# 4. Calculate Precision, Recall, and F1-Score for kNN using confusionMatrix
conf_matrix_knn <- confusionMatrix(knn_predictions, Y_test)
cat("\nkNN Metrics:\n")
cat("Precision: ", conf_matrix_knn$byClass["Pos Pred Value"], "\n")
cat("Recall: ", conf_matrix_knn$byClass["Sensitivity"], "\n")
cat("F1-Score: ", conf_matrix_knn$byClass["F1"], "\n")









###############################################################



# Part 2


# Lab 05


# NY Dataset




#Train a SVM regression model to predict PRICE based on Square Footage and plot
#predicted price vs. real price.




# Load the dataset as a CSV file
ny_housing <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab05/NY-House-Dataset.csv")

# Inspect the first few rows of the dataset to understand its structure
head(ny_housing)
summary(ny_housing)

# Check for missing values
sum(is.na(ny_housing))

# Remove rows with missing values (if needed)
ny_housing <- na.omit(ny_housing)

# Check the cleaned data
summary(ny_housing)








##

# Load necessary libraries
library(e1071)  # For SVM
library(ggplot2)  # For plotting

# Assuming the NY housing dataset is already loaded as 'ny_housing'

# Train the SVM regression model
svm_model <- svm(PRICE ~ PROPERTYSQFT, data = ny_housing, type = "eps-regression")

# Make predictions using the trained model
predicted_prices <- predict(svm_model, ny_housing)


ggplot(ny_housing, aes(x = PRICE, y = predicted_prices)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted Price vs Real Price", x = "Real Price", y = "Predicted Price") +
  theme_minimal() +
  # Set custom limits for x-axis
  xlim(c(min(ny_housing$PRICE), max(ny_housing$PRICE) * 0.1)) +  # Limit the x-axis range
  ylim(c(min(predicted_prices), max(predicted_prices)))  # Set y-axis range to match the predictions














#####################################################



#● Train a linear model using the same formula and plot predicted price vs. real price.



# Load necessary libraries
library(ggplot2)  # For plotting

# Assuming the NY housing dataset is already loaded as 'ny_housing'

# Train the linear regression model
lm_model <- lm(PRICE ~ PROPERTYSQFT, data = ny_housing)

# Make predictions using the trained model
predicted_prices_lm <- predict(lm_model, ny_housing)


# Plot predicted price vs. real price with a smaller x-axis range
ggplot(ny_housing, aes(x = PRICE, y = predicted_prices_lm)) +
  geom_point(color = "blue") +  # Scatter plot for real vs predicted prices
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Line of perfect prediction
  labs(title = "Predicted Price vs Real Price (Linear Model)", x = "Real Price", y = "Predicted Price") +
  theme_minimal() +
  # Set custom limits for x-axis (adjust range as needed)
  xlim(c(min(ny_housing$PRICE), max(ny_housing$PRICE) * 0.1))  # Shrinks x-axis by 20%



