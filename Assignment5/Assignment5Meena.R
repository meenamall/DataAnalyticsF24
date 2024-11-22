#Meena

#Assignment 5


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



########################################


# Question 1b


# Load the necessary libraries
library(ggplot2)

# Assuming the dataset is loaded into 'nyc_data' dataframe

# 1. Examine Variable Distributions

# Summary statistics for key variables
summary(nyc_data$SALE.PRICE)
summary(nyc_data$GROSS.SQUARE.FEET)
summary(nyc_data$RESIDENTIAL.UNITS)

# Convert GROSS.SQUARE.FEET to numeric (if not already)
nyc_data$GROSS.SQUARE.FEET <- as.numeric(nyc_data$GROSS.SQUARE.FEET)

# Check if the conversion was successful
summary(nyc_data$GROSS.SQUARE.FEET)

# 2. Visualize Distributions

# Histogram for Sale Price Distribution
hist(nyc_data$SALE.PRICE, main="Sale Price Distribution", xlab="Sale Price", col="lightblue", border="black")

# Histogram for Square Footage Distribution (after conversion to numeric)
hist(nyc_data$GROSS.SQUARE.FEET, main="Square Footage Distribution", xlab="Square Footage", col="lightgreen", border="black")

# 3. Identifying Outliers for Sale Price

# Box plot for Sale Price to identify outliers
boxplot(nyc_data$SALE.PRICE, main="Sale Price Box Plot", ylab="Sale Price", col="lightpink")

# Scatter plot of Sale Price vs. Square Footage to visually identify outliers
plot(nyc_data$GROSS.SQUARE.FEET, nyc_data$SALE.PRICE, main="Sale Price vs. Square Footage", xlab="Square Footage", ylab="Sale Price", pch=20, col=rgb(0.1, 0.4, 0.6, 0.4))

# 4. Calculate Z-scores to quantify outliers

# Calculate Z-scores for Sale Price
z_scores <- scale(nyc_data$SALE.PRICE)

# Identify Sale Price outliers based on Z-scores (threshold > 3 or < -3)
outliers <- which(abs(z_scores) > 3)

# Print the outlier indices and corresponding Sale Price values
outliers_data <- data.frame(OutlierIndex = outliers, SalePrice = nyc_data$SALE.PRICE[outliers])
print(outliers_data)

# 5. Summary of Analysis
cat("\nSummary of Analysis:\n")
cat("The histograms above show the distribution of Sale Price and Square Footage. The boxplot identifies outliers in Sale Price, with values outside the interquartile range considered as potential outliers. The scatter plot of Sale Price vs. Square Footage helps identify extreme values visually. Additionally, Z-scores were calculated to quantify the extremity of Sale Price outliers. Outliers are identified where Z-scores exceed an absolute value of 3.\n")




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

# Fit the model (multivariate regression) - Sale Price as the dependent variable
model_full <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET + YEAR.BUILT + BUILDING.CLASS.CATEGORY + NEIGHBORHOOD + RESIDENTIAL.UNITS + COMMERCIAL.UNITS, 
                 data = nyc_data_clean)

# Display summary of the model to check the results
summary(model_full)
















#############################################################

#d). Pick more than one supervised learning model (these need not be restricted to the
#models you’ve learned so far), e.g., Naïve Bayes, k-NN, Random Forest, SVM to explore a
#classification problem using the data. You may choose which categorical variable (e.g.
#neighborhood, building class) to use as class label. Evaluate the results (contingency
#tables & metrics). Describe any cleaning you had to do and why. Min. 5 sentences (2%)






# Full file path with forward slashes
nyc_data <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment5/NYC_Citywide_Annualized_Calendar_Sales_Update_20241118.csv")

# Alternatively, use double backslashes
nyc_data <- read.csv("C:\\Users\\Meena\\Desktop\\DataAnalytics\\DataAnalytics\\Assignment5\\NYC_Citywide_Annualized_Calendar_Sales_Update_20241118.csv")


# Check structure of the data
str(nyc_data)

# Check the first few rows of the dataset
head(nyc_data)


# Set the target column
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
library(e1071)         # For Naive Bayes
library(class)         # For k-NN
library(randomForest)  # For Random Forest
library(caret)         # For evaluation and confusion matrix

# Load the dataset
data <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment5/NYC_Citywide_Annualized_Calendar_Sales_Update_20241118.csv")

# Preprocess the target variable
data <- data[data$BUILDING.CLASS.AT.TIME.OF.SALE != "", ]  # Remove empty rows for the target
data$BUILDING.CLASS.AT.TIME.OF.SALE <- as.factor(data$BUILDING.CLASS.AT.TIME.OF.SALE)

# Filter rare classes (less than 50 occurrences)
class_counts <- table(data$BUILDING.CLASS.AT.TIME.OF.SALE)
filtered_classes <- names(class_counts[class_counts >= 50])
data <- data[data$BUILDING.CLASS.AT.TIME.OF.SALE %in% filtered_classes, ]

# Select predictors and ensure no missing data
predictors <- c("GROSS.SQUARE.FEET", "RESIDENTIAL.UNITS", "YEAR.BUILT", "SALE.PRICE")
data <- na.omit(data[, c(predictors, "BUILDING.CLASS.AT.TIME.OF.SALE")])

# Split data into training and test sets
set.seed(123)
train_idx <- createDataPartition(data$BUILDING.CLASS.AT.TIME.OF.SALE, p = 0.7, list = FALSE)
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Naive Bayes Model
nb_model <- naiveBayes(BUILDING.CLASS.AT.TIME.OF.SALE ~ ., data = train_data)
nb_predictions <- predict(nb_model, test_data)
nb_cm <- confusionMatrix(nb_predictions, test_data$BUILDING.CLASS.AT.TIME.OF.SALE)

# k-NN Model
k <- 5  # Choose k value
knn_predictions <- knn(train_data[, predictors], test_data[, predictors], train_data$BUILDING.CLASS.AT.TIME.OF.SALE, k = k)
knn_cm <- confusionMatrix(knn_predictions, test_data$BUILDING.CLASS.AT.TIME.OF.SALE)

# Random Forest Model
rf_model <- randomForest(BUILDING.CLASS.AT.TIME.OF.SALE ~ ., data = train_data, ntree = 100)
rf_predictions <- predict(rf_model, test_data)
rf_cm <- confusionMatrix(rf_predictions, test_data$BUILDING.CLASS.AT.TIME.OF.SALE)

# Print Evaluation Metrics
print("Naive Bayes Confusion Matrix and Metrics:")
print(nb_cm)

print("k-NN Confusion Matrix and Metrics:")
print(knn_cm)

print("Random Forest Confusion Matrix and Metrics:")
print(rf_cm)







# Normalize the data
train_data[predictors] <- lapply(train_data[predictors], scale)
test_data[predictors] <- lapply(test_data[predictors], scale)

# Reduce the value of k
k <- 3  # Use a smaller number of neighbors

# Re-run k-NN
knn_predictions <- knn(
  train = train_data[, predictors],
  test = test_data[, predictors],
  cl = train_data$BUILDING.CLASS.AT.TIME.OF.SALE,
  k = k
)

# Evaluate the k-NN model
knn_conf_matrix <- confusionMatrix(knn_predictions, test_data$BUILDING.CLASS.AT.TIME.OF.SALE)
print("k-NN Confusion Matrix and Metrics:")
print(knn_conf_matrix)
