#MEENA MALL

#Assignment 7

#Data Analytics


##########################################3

# Load the dataset (change the file path to the new location)
file_path <- "C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment7/estimationofobesity/ObesityDataSet_raw_and_data_sinthetic.csv"

# Load the dataset into R
obesity_data <- read.csv(file_path)

# View the first few rows of the dataset to ensure it loaded correctly
head(obesity_data)

# Get a concise overview of the data (alternative to str)
library(dplyr)
glimpse(obesity_data)

# Summary statistics for all columns
summary(obesity_data)

# Check for missing values
sum(is.na(obesity_data))

# Remove rows with missing values (or impute based on the column as needed)
obesity_data_clean <- na.omit(obesity_data)

# Create a BMI column (ensure weight is in kg and height is in meters)
obesity_data_clean$BMI <- obesity_data_clean$Weight / (obesity_data_clean$Height^2)

# Check the summary of BMI and other statistics
summary(obesity_data_clean$BMI)

# Calculate the standard deviation for key numeric variables
sd(obesity_data_clean$BMI)
sd(obesity_data_clean$Age)
sd(obesity_data_clean$Height)
sd(obesity_data_clean$Weight)

# Outlier detection: Boxplot for BMI (to visually check for outliers)
ggplot(obesity_data_clean, aes(y = BMI)) + 
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Boxplot of BMI")

# Histogram for BMI
ggplot(obesity_data_clean, aes(x = BMI)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of BMI", x = "BMI", y = "Frequency")

# Correlation matrix for numerical features
cor_matrix <- cor(obesity_data_clean[, c("Age", "Height", "Weight", "BMI")])
print(cor_matrix)

# Categorical data analysis - explore relationships with BMI or weight
ggplot(obesity_data_clean, aes(x = NObeyesdad, y = BMI)) + 
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "BMI by Obesity Level", x = "Obesity Level", y = "BMI")

# Explore gender distribution with BMI
ggplot(obesity_data_clean, aes(x = Gender, y = BMI)) + 
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "BMI by Gender", x = "Gender", y = "BMI")




IQR_bmi <- IQR(obesity_data_clean$BMI)
lower_bound <- quantile(obesity_data_clean$BMI, 0.25) - 1.5 * IQR_bmi
upper_bound <- quantile(obesity_data_clean$BMI, 0.75) + 1.5 * IQR_bmi
outliers <- obesity_data_clean$BMI[obesity_data_clean$BMI < lower_bound | obesity_data_clean$BMI > upper_bound]
print(outliers)

z_scores <- scale(obesity_data_clean$BMI)
outliers <- obesity_data_clean$BMI[abs(z_scores) > 3]
print(outliers)






































###############################################



#2. Model Development, Validation and Optimization (10%) Choose two (4000-level*) or
#three (6000-level) or more digerent models (the models must include at least 2 objectives
#                                            from regression, classification & clustering). The choice of independent and response
#variables is up to you.
#Explain why you chose them. Construct the models, test/ validate them. Explain the
#validation approach. You can use any method(s) covered in the course. Include your code
#in your submission. Compare model results if applicable. Report the results of the model
#(fits, coegicients, trees, other measures of fit/ importance, etc., predictors and summary
#  statistics). Min. 2 pages of text + graphics (required).







# Install packages (if not already installed)
install.packages("caret")
install.packages("cluster")
install.packages("ggplot2")
install.packages("readxl")  # if reading Excel files

# Load the necessary libraries
library(tidyverse)
library(caret)
library(cluster)
library(readxl)  # If reading Excel files

# Load the dataset (change the file path to the new location)
file_path <- "C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment7/estimationofobesity/ObesityDataSet_raw_and_data_sinthetic.csv"

# Read in the dataset (since it's a CSV file)
data <- read.csv(file_path)

# Check the first few rows to understand the dataset structure
head(data)

# Create BMI column: BMI = Weight / (Height^2)
# Make sure to convert Height to meters by dividing by 100 (if it’s in cm)
data$BMI <- data$Weight / (data$Height^2)

# Now, check the first few rows again to confirm the BMI column is added
head(data)





# Select independent variables (predictors) and dependent variable (BMI)
# Let's use Age, Gender, Weight, and family_history_with_overweight as predictors
X <- data[, c('Age', 'Gender', 'Weight', 'family_history_with_overweight')]  # Adjusted column names
y <- data$BMI  # Dependent variable: BMI

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(42)
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Fit the linear regression model
model_lr <- lm(BMI ~ Age + Gender + Weight + family_history_with_overweight, data = train_data)

# View the summary of the model
summary(model_lr)

# Make predictions on the test set
y_pred <- predict(model_lr, newdata = test_data)

# Calculate Mean Squared Error (MSE)
mse <- mean((test_data$BMI - y_pred)^2)
cat("Mean Squared Error:", mse)

# Calculate R-squared value
rsq <- summary(model_lr)$r.squared
cat("R-squared:", rsq)

# Cross-validation using caret (using the training data)
train_control <- trainControl(method = "cv", number = 10)
cv_model_lr <- train(BMI ~ Age + Gender + Weight + family_history_with_overweight, 
                     data = train_data, method = "lm", trControl = train_control)

# Print the cross-validation results
print(cv_model_lr)

















#second model


# Create a binary variable (obese vs. non-obese)
data$Obese <- ifelse(data$BMI >= 30, 1, 0)  # Obese = 1, Non-Obese = 0

# Select predictors
X <- data[, c('Age', 'Gender', 'Weight', 'family_history_with_overweight')] 
y <- data$Obese  # Dependent variable: Obesity status

# Split data
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Fit logistic regression model
model_logistic <- glm(Obese ~ Age + Gender + Weight + family_history_with_overweight, 
                      data = train_data, family = binomial())

# View the summary of the model
summary(model_logistic)

# Make predictions on the test set (probabilities)
y_pred_logistic <- predict(model_logistic, newdata = test_data, type = "response")

# Convert probabilities to binary outcomes
y_pred_class <- ifelse(y_pred_logistic > 0.5, 1, 0)

# Evaluate model performance
conf_matrix <- table(Predicted = y_pred_class, Actual = test_data$Obese)
print(conf_matrix)
















### third model




# Check the structure of the 'family_history_with_overweight' column
str(data$family_history_with_overweight)

# Convert 'family_history_with_overweight' to numeric (if it's a factor or character)
data$family_history_with_overweight <- as.numeric(factor(data$family_history_with_overweight))

# Convert 'Gender' to numeric (1 for Male, 2 for Female)
data$Gender <- as.numeric(factor(data$Gender))

# Check again for missing values
sum(is.na(data[, c('Age', 'Gender', 'Weight', 'family_history_with_overweight')]))

# Remove rows with missing values (optional)
data <- na.omit(data)

# Scale the predictors now that all columns are numeric
scaled_data <- scale(data[, c('Age', 'Gender', 'Weight', 'family_history_with_overweight')])

# Apply K-means clustering to the scaled data
set.seed(42)  # For reproducibility
kmeans_model <- kmeans(scaled_data, centers = 3, nstart = 25)  # Using 3 clusters

# View the clustering results
print(kmeans_model)

# Add the cluster assignments to the original dataset
data$Cluster <- as.factor(kmeans_model$cluster)

# Visualizing the clusters
library(ggplot2)
ggplot(data, aes(x = Age, y = Weight, color = Cluster)) + 
  geom_point() + 
  labs(title = "K-means Clustering of Obesity Data",
       x = "Age", 
       y = "Weight") +
  theme_minimal()

# Assess the clustering result
table(data$Cluster)  # See how many instances are in each cluster

# Optionally, check cluster centers
kmeans_model$centers
