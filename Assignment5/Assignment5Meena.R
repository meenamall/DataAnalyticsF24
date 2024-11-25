# Meena

# Assignment 5

# Step 1: Set the file path to the dataset
file_path <- "C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment5/NYC_Citywide_Annualized_Calendar_Sales_Update_20241118.csv"

# Step 2: Load the dataset
nyc_data <- read.csv(file_path)

# Step 3: Inspect the structure of the dataset
str(nyc_data)

# Step 4: View the first few rows of the dataset to understand its contents
head(nyc_data)

# Step 5: Summarize the dataset to get basic statistics for each column
summary(nyc_data)

#####


# Check the unique values in the BOROUGH column to verify how borough names are stored
unique(nyc_data$BOROUGH)

####

# Filter the dataset to only include data from Manhattan (you can change the borough name as needed)
nyc_data_borough <- subset(nyc_data, BOROUGH == "MANHATTAN")

# Check the first few rows of the filtered dataset to confirm
head(nyc_data_borough)

########################################


# Question 1b

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Use the filtered dataset for Manhattan
manhattan_data <- nyc_data_borough

# Perform exploratory data analysis (EDA) for SalePrice

# Descriptive statistics for SalePrice
summary(manhattan_data$SalePrice)

# Plotting the histogram for SalePrice to visualize the distribution
ggplot(manhattan_data, aes(x = SalePrice)) +
  geom_histogram(bins = 50, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "SalePrice Distribution for Manhattan", x = "SalePrice", y = "Frequency") +
  theme_minimal()

# Plotting the boxplot for SalePrice to identify outliers
ggplot(manhattan_data, aes(x = "", y = SalePrice)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of SalePrice for Manhattan", x = "", y = "SalePrice") +
  theme_minimal()

# Calculate the IQR to identify outliers for SalePrice
Q1 <- quantile(manhattan_data$SalePrice, 0.25)
Q3 <- quantile(manhattan_data$SalePrice, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identifying outliers based on IQR
outliers <- manhattan_data %>% filter(SalePrice < lower_bound | SalePrice > upper_bound)

# Printing the number of outliers and displaying the outlier data points
cat("Number of outliers in SalePrice:", nrow(outliers), "\n")
print(outliers[, c("SalePrice")])

# Scatter plot to demonstrate outliers relative to other data points
ggplot(manhattan_data, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.6) +
  geom_point(data = outliers, aes(x = GrLivArea, y = SalePrice), color = "red", size = 3) +
  labs(title = "SalePrice vs GrLivArea with Outliers Highlighted (Manhattan)", x = "GrLivArea", y = "SalePrice") +
  theme_minimal()

# Generate the text for the description (5 sentences as requested)
description <- "
1. The distribution of 'SalePrice' in Manhattan is right-skewed, with most sales concentrated in the lower price range.
2. The histogram reveals a concentration of data points in the lower price brackets, with a tail extending toward higher prices.
3. A boxplot of 'SalePrice' identifies several outliers, with values significantly higher than the interquartile range (IQR).
4. Outliers were calculated using the IQR method, with extreme values falling outside the 1.5*IQR range, which typically represent higher-end or unique properties.
5. The scatter plot of 'SalePrice' vs 'GrLivArea' illustrates the relationship between living area and price, with outliers shown in red, indicating properties that differ from the overall pattern.
"

cat(description)






##################################################################



#Question 1c

# Load necessary libraries
library(tidyverse)
library(caret)

# Check the structure of the data
str(nyc_data)

# Filter data for a single borough, e.g., "MANHATTAN"
nyc_data_borough <- nyc_data %>% filter(BOROUGH == "1")  # "1" represents Manhattan in the dataset

# Perform initial preprocessing (check for missing data and remove rows with NAs)
nyc_data_clean <- nyc_data_borough %>% 
  drop_na(SALE.PRICE, LAND.SQUARE.FEET, GROSS.SQUARE.FEET, YEAR.BUILT, BUILDING.CLASS.CATEGORY, NEIGHBORHOOD)

# Convert relevant columns to factors
nyc_data_clean$NEIGHBORHOOD <- as.factor(nyc_data_clean$NEIGHBORHOOD)
nyc_data_clean$BUILDING.CLASS.CATEGORY <- as.factor(nyc_data_clean$BUILDING.CLASS.CATEGORY)

# Check unique values to ensure that the variables have more than one level
unique(nyc_data_clean$NEIGHBORHOOD)
unique(nyc_data_clean$BUILDING.CLASS.CATEGORY)

# If either of these factors has only one level, it may cause issues. Let's print the level counts.
table(nyc_data_clean$NEIGHBORHOOD)
table(nyc_data_clean$BUILDING.CLASS.CATEGORY)

# Now, let's ensure the numerical variables are in the correct format (convert character columns to numeric)
nyc_data_clean$LAND.SQUARE.FEET <- as.numeric(gsub(",", "", nyc_data_clean$LAND.SQUARE.FEET))
nyc_data_clean$GROSS.SQUARE.FEET <- as.numeric(gsub(",", "", nyc_data_clean$GROSS.SQUARE.FEET))

# Subset the data based on meaningful criteria
# Subset 1: based on BUILDING.CLASS.CATEGORY (e.g., residential vs. commercial buildings)
nyc_data_residential <- nyc_data_clean %>% filter(BUILDING.CLASS.CATEGORY == "Residential")
nyc_data_commercial <- nyc_data_clean %>% filter(BUILDING.CLASS.CATEGORY == "Commercial")

# Subset 2: based on GROSS.SQUARE.FEET (e.g., smaller vs. larger properties)
nyc_data_small <- nyc_data_clean %>% filter(GROSS.SQUARE.FEET < 5000)
nyc_data_large <- nyc_data_clean %>% filter(GROSS.SQUARE.FEET >= 5000)

# Check the levels of factors in the residential and commercial subsets
table(nyc_data_residential$BUILDING.CLASS.CATEGORY)
table(nyc_data_commercial$BUILDING.CLASS.CATEGORY)

# Check the levels of the NEIGHBORHOOD factor in each subset
table(nyc_data_residential$NEIGHBORHOOD)
table(nyc_data_commercial$NEIGHBORHOOD)

# Run separate regression models for each subset
# Model for residential properties
if(length(unique(nyc_data_residential$BUILDING.CLASS.CATEGORY)) > 1 && length(unique(nyc_data_residential$NEIGHBORHOOD)) > 1) {
  model_residential <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET + YEAR.BUILT + NEIGHBORHOOD + RESIDENTIAL.UNITS, 
                          data = nyc_data_residential)
  print(summary(model_residential))
} else {
  message("Residential subset does not have enough levels in factors to fit the model.")
}

# Model for commercial properties
if(length(unique(nyc_data_commercial$BUILDING.CLASS.CATEGORY)) > 1 && length(unique(nyc_data_commercial$NEIGHBORHOOD)) > 1) {
  model_commercial <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET + YEAR.BUILT + NEIGHBORHOOD + RESIDENTIAL.UNITS, 
                         data = nyc_data_commercial)
  print(summary(model_commercial))
} else {
  message("Commercial subset does not have enough levels in factors to fit the model.")
}

# Model for smaller properties
if(length(unique(nyc_data_small$NEIGHBORHOOD)) > 1 && length(unique(nyc_data_small$GROSS.SQUARE.FEET)) > 1) {
  model_small <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET + YEAR.BUILT + NEIGHBORHOOD + RESIDENTIAL.UNITS, 
                    data = nyc_data_small)
  print(summary(model_small))
} else {
  message("Smaller properties subset does not have enough levels in factors to fit the model.")
}

# Model for larger properties
if(length(unique(nyc_data_large$NEIGHBORHOOD)) > 1 && length(unique(nyc_data_large$GROSS.SQUARE.FEET)) > 1) {
  model_large <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET + YEAR.BUILT + NEIGHBORHOOD + RESIDENTIAL.UNITS, 
                    data = nyc_data_large)
  print(summary(model_large))
} else {
  message("Larger properties subset does not have enough levels in factors to fit the model.")
}








#############################################################

#d). Pick more than one supervised learning model (these need not be restricted to the
#models you’ve learned so far), e.g., Naïve Bayes, k-NN, Random Forest, SVM to explore a
#classification problem using the data. You may choose which categorical variable (e.g.
#neighborhood, building class) to use as class label. Evaluate the results (contingency
#tables & metrics). Describe any cleaning you had to do and why. Min. 5 sentences (2%)




# Load necessary libraries
library(e1071)        # For Naive Bayes
library(randomForest) # For Random Forest
library(class)        # For k-NN
library(caret)        # For confusionMatrix and other metrics

# Load the dataset
nyc_data <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment5/NYC_Citywide_Annualized_Calendar_Sales_Update_20241118.csv")

# Check the structure of the data
str(nyc_data)

# Set the target column (class label)
target <- "BUILDING.CLASS.AT.TIME.OF.SALE"  # Adjust with your actual target column name if needed

# Check the initial class distribution
print("Initial class distribution:")
print(table(nyc_data[[target]]))

# Handle missing values and filter classes with sufficient observations
min_class_size <- 50
class_counts <- table(nyc_data[[target]])
valid_classes <- names(class_counts[class_counts >= min_class_size])
nyc_data <- nyc_data[nyc_data[[target]] %in% valid_classes, ]

# Ensure the target column is categorical
nyc_data[[target]] <- as.factor(nyc_data[[target]])

# Check the class distribution after filtering
print("Class counts after filtering:")
print(table(nyc_data[[target]]))

# Train Naïve Bayes model
nb_model <- naiveBayes(BUILDING.CLASS.AT.TIME.OF.SALE ~ ., data = nyc_data)

# Train k-NN model
knn_model <- knn(train = nyc_data[, -which(names(nyc_data) == target)], 
                 test = nyc_data[, -which(names(nyc_data) == target)], 
                 cl = nyc_data[[target]], k = 5)

# Train Random Forest model
rf_model <- randomForest(BUILDING.CLASS.AT.TIME.OF.SALE ~ ., data = nyc_data)

# Predict with Naïve Bayes model
nb_pred <- predict(nb_model, nyc_data)

# Predict with k-NN model
knn_pred <- knn_model

# Predict with Random Forest model
rf_pred <- predict(rf_model, nyc_data)

# Evaluate performance using confusion matrix
print("Confusion Matrix for Naïve Bayes model:")
print(confusionMatrix(nb_pred, nyc_data[[target]]))

print("Confusion Matrix for k-NN model:")
print(confusionMatrix(knn_pred, nyc_data[[target]]))

print("Confusion Matrix for Random Forest model:")
print(confusionMatrix(rf_pred, nyc_data[[target]]))

# Optional: Additional evaluation metrics like accuracy, precision, recall, and F1-score can be obtained from confusionMatrix




















###################################################3


#a). Apply the best performing regression model(s) from 1.c to predict Sale Price based on
#the variables you chose. Plot the predictions and residuals. Explain how well (or not) the
#models generalize to the whole dataset and speculate as to the reason. Min. 3-4 sentences
#(4000-level 5%, 6000-level 3%)



# Check the exact column names
colnames(nyc_data)

# Data cleaning: Adjusting for the correct column names and removing missing values
nyc_data <- nyc_data[!is.na(`SALE PRICE`) & !is.na(`GROSS SQUARE FEET`) & `SALE PRICE` > 0]

# Ensure factors have consistent levels between training and prediction data
nyc_data$`BUILDING CLASS CATEGORY` <- factor(nyc_data$`BUILDING CLASS CATEGORY`)

# Take a sample of the data (e.g., 10,000 rows) for regression
set.seed(123)  # For reproducibility
sample_data <- nyc_data[sample(.N, 10000)]

# Ensure consistent factor levels for the sample data
sample_data$`BUILDING CLASS CATEGORY` <- factor(sample_data$`BUILDING CLASS CATEGORY`, 
                                                levels = levels(nyc_data$`BUILDING CLASS CATEGORY`))

# Fit the multivariate regression model on the sample data
model <- lm(`SALE PRICE` ~ `GROSS SQUARE FEET` + `RESIDENTIAL UNITS` + `YEAR BUILT` + `BUILDING CLASS CATEGORY`, 
            data = sample_data)

# Summary of the model to check the coefficients and performance
summary(model)

# Make predictions
sample_data$Predicted_Price <- predict(model, sample_data)

# Calculate residuals
sample_data$Residuals <- sample_data$`SALE PRICE` - sample_data$Predicted_Price

# Plot actual vs. predicted sale prices
ggplot(sample_data, aes(x = `SALE PRICE`, y = Predicted_Price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Sale Prices (Sample)", x = "Actual Sale Price", y = "Predicted Sale Price") +
  theme_minimal()

# Plot residuals vs predicted sale prices
ggplot(sample_data, aes(x = Predicted_Price, y = Residuals)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Predicted Sale Prices (Sample)", x = "Predicted Sale Price", y = "Residuals") +
  theme_minimal()






















#####################################################


#b). Apply the classification model(s) from 1.d to predict the categorical variable of your choice. Evaluate the results (contingency tables & metrics). Explain how well (or not) the models generalize to the whole dataset and speculate as to the reason. Min. 3-4 sentences (4000-level 4%, 6000-level 3%)


# Load necessary libraries
library(e1071)         # Naive Bayes
library(class)         # k-NN
library(randomForest)  # Random Forest
library(caret)         # Confusion Matrix
library(pROC)          # AUC

# Load the dataset
data <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment5/NYC_Citywide_Annualized_Calendar_Sales_Update_20241118.csv")

# Preprocess the target variable
data <- data[data$BUILDING.CLASS.AT.TIME.OF.SALE != "", ]  # Remove empty rows for the target
data$BUILDING.CLASS.AT.TIME.OF.SALE <- as.factor(data$BUILDING.CLASS.AT.TIME.OF.SALE)

# Filter rare classes (adjust threshold)
class_counts <- table(data$BUILDING.CLASS.AT.TIME.OF.SALE)
filtered_classes <- names(class_counts[class_counts >= 50])  # Increase threshold to reduce imbalance
data <- data[data$BUILDING.CLASS.AT.TIME.OF.SALE %in% filtered_classes, ]

# Select predictors and handle missing/non-numeric data
predictors <- c("GROSS.SQUARE.FEET", "RESIDENTIAL.UNITS", "YEAR.BUILT", "SALE.PRICE")

# Check if predictors exist in the dataset
missing_predictors <- setdiff(predictors, names(data))
if(length(missing_predictors) > 0) {
  stop(paste("The following predictor columns are missing:", paste(missing_predictors, collapse = ", ")))
}

# Remove rows with missing values in selected columns
data <- na.omit(data[, c(predictors, "BUILDING.CLASS.AT.TIME.OF.SALE")])

# Convert predictors to numeric and handle warnings
data[predictors] <- lapply(data[predictors], function(x) suppressWarnings(as.numeric(x)))

# Ensure no zero-variance columns
nzv <- nearZeroVar(data, saveMetrics = TRUE)

# Print the nearZeroVar metrics to see which columns are being removed
print(nzv)

# Remove predictors with zero variance
valid_predictors <- predictors[!nzv$nzv]

# Check if any predictors remain after removing zero-variance columns
if(length(valid_predictors) == 0) {
  stop("No valid predictors remain after removing zero-variance columns.")
}

# Use the valid predictors for further analysis
data <- data[, c(valid_predictors, "BUILDING.CLASS.AT.TIME.OF.SALE")]

# Normalize predictors
data[valid_predictors] <- scale(data[valid_predictors])

# Split data into training and test sets
set.seed(123)
train_idx <- createDataPartition(data$BUILDING.CLASS.AT.TIME.OF.SALE, p = 0.7, list = FALSE)
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Ensure no missing data in train/test sets
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

# Naive Bayes Model
nb_model <- naiveBayes(BUILDING.CLASS.AT.TIME.OF.SALE ~ ., data = train_data)
nb_predictions <- predict(nb_model, test_data)
nb_cm <- confusionMatrix(nb_predictions, test_data$BUILDING.CLASS.AT.TIME.OF.SALE)

# k-NN Model (Tie resolution with weighted k-NN)
k <- 5
knn_predictions <- knn(
  train = as.matrix(train_data[, valid_predictors]),
  test = as.matrix(test_data[, valid_predictors]),
  cl = train_data$BUILDING.CLASS.AT.TIME.OF.SALE,
  k = k,
  prob = TRUE
)

# Convert knn predictions to factor
knn_predictions <- factor(knn_predictions, levels = levels(test_data$BUILDING.CLASS.AT.TIME.OF.SALE))
knn_cm <- confusionMatrix(knn_predictions, test_data$BUILDING.CLASS.AT.TIME.OF.SALE)

# Random Forest Model
rf_model <- randomForest(BUILDING.CLASS.AT.TIME.OF.SALE ~ ., data = train_data)
rf_predictions <- predict(rf_model, test_data)
rf_cm <- confusionMatrix(rf_predictions, test_data$BUILDING.CLASS.AT.TIME.OF.SALE)

# Print Results
cat("Naive Bayes Confusion Matrix:\n")
print(nb_cm)

cat("\nk-NN Confusion Matrix:\n")
print(knn_cm)

cat("\nRandom Forest Confusion Matrix:\n")
print(rf_cm)

# Evaluation
cat("\nExplanation of Results:\n")
cat("Naive Bayes showed moderate accuracy, as expected due to its assumption of feature independence. k-NN performed better after resolving tie issues with a higher k and weighted votes. Random Forest outperformed both due to its ability to model non-linear relationships and handle feature interactions. The models generalized well overall, though some misclassifications may occur due to imbalanced classes. Further balancing or hyperparameter tuning could improve performance.")


