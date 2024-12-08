#MEENA MALL

# Assignemnt 6

#Quidditch 2 Datasets




#########################################
# Load Libraries and Datasets

library(tidyverse)

# Set file paths
individual_stats_path <- "C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment6/2022 Statistics - Individual.csv"
team_standings_path <- "C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment6/Standings_2022.csv"

# Read the datasets
individual_stats <- read.csv(individual_stats_path)
team_standings <- read.csv(team_standings_path)




###################################
# Inspect and Clean the Data

# Check structure of datasets
str(individual_stats)
str(team_standings)

# Summarize datasets
summary(individual_stats)
summary(team_standings)

# Identify missing values
sum(is.na(individual_stats))
sum(is.na(team_standings))

# Replace missing values with the median (example)
individual_stats <- individual_stats %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .))

team_standings <- team_standings %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .))

# Clean column names using make.names
colnames(individual_stats) <- make.names(colnames(individual_stats), unique = TRUE)
colnames(team_standings) <- make.names(colnames(team_standings), unique = TRUE)

# Create Total Contribution metric
individual_stats <- individual_stats %>% 
  mutate(TotalContribution = Goals + Assists - Turnovers)

# Team name mapping for full names
team_name_mapping <- c(
  "DET" = "Detroit",
  "KC" = "Kansas City",
  "CLE" = "Cleveland",
  "LC" = "Los Angeles",
  "CLT" = "Charlotte",
  "MPLS" = "Minneapolis",
  "OTT" = "Ottawa",
  "NO" = "New Orleans",
  "SA" = "San Antonio",
  "TOR" = "Toronto",
  "NYC" = "New York",
  "ROC" = "Rochester",
  "IND" = "Indianapolis",
  "BOS" = "Boston",
  "AUS" = "Austin",
  "WAS" = "Washington"
)

# Recode the Team names in individual_stats to match the full names in team_standings
individual_stats$Team <- recode(individual_stats$Team, !!!team_name_mapping)

# Combine the datasets based on the 'Team' column
combined_data_clean <- merge(individual_stats, team_standings, by = "Team")

# Drop columns with 'NA..' in their names
combined_data_clean <- combined_data_clean %>%
  select(-contains("NA.."))

# Check the cleaned data structure
str(combined_data_clean)





##########################################
# Statistical Analysis and Visualizations
install.packages("ggplot2")  # Only install once
library(ggplot2)  # Load the library



# Histogram of Total Contribution
ggplot(combined_data_clean, aes(x = TotalContribution)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Player Contributions (Positive and Negative)",
       x = "Total Contribution (Positive and Negative)",
       y = "Number of Players") +
  theme_minimal()


#The negative values in the TotalContribution column are definitely valid data points, and this explains why you see negative values on the x-axis of your histogram. These negative values likely represent players with a total contribution that is less than zero, perhaps due to a calculation that subtracts various statistics like turnovers, penalties, or other negative events from positive contributions (goals, assists, etc.).






library(ggplot2)  # Load the library

library(dplyr)

combined_data_clean <- combined_data_clean %>%
  mutate(Rank = rank(-TotalContribution))  # Rank in descending order of TotalContribution

# Histogram of Rank
ggplot(combined_data_clean, aes(x = Rank)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribution of Player Rankings Based on Total Contribution",
       x = "Player Rank (0 is lowest)",
       y = "Number of Players") +
  theme_minimal()








# Correlation Analysis
correlation <- cor(combined_data_clean$TotalContribution, combined_data_clean$Rank, use = "complete.obs")
print(paste("Correlation between Total Contribution and Rank:", round(correlation, 2)))



#The output you received, showing a correlation of -0.95, indicates a very strong negative relationship between these two variables.

#A correlation of -0.95 means that as TotalContribution increases, Rank tends to decrease (i.e., higher contributions are associated with better (lower) rankings). This suggests a strong negative relationship.
ggplot(combined_data_clean, aes(x = TotalContribution, y = Rank)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Total Contribution vs. Rank with Regression Line", 
       x = "Total Contribution", 
       y = "Rank") +
  theme_minimal()







# Scatter plot with regression line
ggplot(combined_data_clean, aes(x = TotalContribution, y = Rank)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Correlation Between Player Contributions and Team Rank",
       x = "Total Contribution", y = "Team Rank") +
  theme_minimal()

##########################################
# Linear Regression Analysis

# Fit a linear regression model
model <- lm(Rank ~ TotalContribution, data = combined_data_clean)
summary(model)

# Plot the regression line
ggplot(combined_data_clean, aes(x = TotalContribution, y = Rank)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regression Analysis: Total Contribution vs Team Rank",
       x = "Total Contribution", y = "Team Rank") +
  theme_minimal()

##########################################










































######################################################
#4. Model Development and Application of model(s) (12%) 

#######
#Random Forest Regression

# Install and load the randomForest package if you haven't already
install.packages("randomForest")
library(randomForest)


# Build Random Forest Model
random_forest_model <- randomForest(Rank ~ TotalContribution, data = combined_data_clean, 
                                    ntree = 500, mtry = 2, importance = TRUE)

# Print the model summary
print(random_forest_model)






# View importance of variables
importance(random_forest_model)

# Plot the model's error rate over trees
plot(random_forest_model)

# Predict using the random forest model
predictions <- predict(random_forest_model, newdata = combined_data_clean)

# Compare predictions to actual ranks
actual_vs_pred <- data.frame(Actual = combined_data_clean$Rank, Predicted = predictions)
head(actual_vs_pred)




# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - combined_data_clean$Rank)^2))
print(paste("RMSE:", round(rmse, 2)))

# Calculate MAE (Mean Absolute Error)
mae <- mean(abs(predictions - combined_data_clean$Rank))
print(paste("MAE:", round(mae, 2)))




# Scatter plot of actual vs predicted ranks
ggplot(data = actual_vs_pred, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Random Forest Model: Actual vs Predicted Ranks", x = "Actual Rank", y = "Predicted Rank") +
  theme_minimal()

















########
#Support Vector Machines (SVM)

install.packages("ggplot2")
install.packages("e1071")

library(e1071)
library(ggplot2)


svm_model <- svm(Rank ~ TotalContribution, data = combined_data_clean, 
                 kernel = "radial", cost = 1, scale = TRUE)



print(svm_model)



svm_predictions <- predict(svm_model, newdata = combined_data_clean)

actual_vs_pred_svm <- data.frame(Actual = combined_data_clean$Rank, Predicted = svm_predictions)
head(actual_vs_pred_svm)


# Calculate RMSE
rmse_svm <- sqrt(mean((svm_predictions - combined_data_clean$Rank)^2))
print(paste("RMSE:", round(rmse_svm, 2)))

# Calculate MAE
mae_svm <- mean(abs(svm_predictions - combined_data_clean$Rank))
print(paste("MAE:", round(mae_svm, 2)))


ggplot(data = actual_vs_pred_svm, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "SVM Model: Actual vs Predicted Ranks", x = "Actual Rank", y = "Predicted Rank") +
  theme_minimal()














#########
#cluster

install.packages("tidyverse")


## Load necessary library for clustering
install.packages("cluster")
library(cluster)

# Load necessary libraries
library(tidyverse)  # This will load dplyr and other useful packages

# Select relevant variables for clustering
clustering_data <- combined_data_clean %>%
  select(TotalContribution, Rank)


# Scale the data (important for clustering algorithms)
clustering_data_scaled <- scale(clustering_data)

# Perform K-means clustering (you can experiment with different k values, here I will start with k = 3)
set.seed(123)  # for reproducibility
kmeans_model <- kmeans(clustering_data_scaled, centers = 3, nstart = 25)

# View the cluster centers
print(kmeans_model$centers)

# Assign the clusters to the dataset
combined_data_clean$Cluster <- as.factor(kmeans_model$cluster)

# Plot the clusters
ggplot(combined_data_clean, aes(x = TotalContribution, y = Rank, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering of Teams Based on Total Contribution and Rank",
       x = "Total Contribution", y = "Rank", color = "Cluster") +
  theme_minimal()

# Evaluate clustering with within-cluster sum of squares (WSS)
wss <- sum(kmeans_model$withinss)
print(paste("Within-cluster sum of squares (WSS):", round(wss, 2)))

# Silhouette analysis for cluster quality
silhouette_result <- silhouette(kmeans_model$cluster, dist(clustering_data_scaled))
plot(silhouette_result, main = "Silhouette Plot for K-means Clustering")

# You can also compare actual vs predicted (although clustering is unsupervised, it doesn't have "true" labels for comparison)
combined_data_clean$PredictedCluster <- kmeans_model$cluster























######
#Decision Tree 


# Install and load necessary libraries
install.packages("rpart")
install.packages("rpart.plot")
install.packages("tidyverse")

library(rpart)
library(rpart.plot)
library(tidyverse)

# Select relevant variables (TotalContribution and Rank)
decision_tree_data <- combined_data_clean %>%
  select(TotalContribution, Rank)

# Fit the decision tree model
decision_tree_model <- rpart(Rank ~ TotalContribution, data = decision_tree_data, method = "class")

# Plot the decision tree
rpart.plot(decision_tree_model, main = "Decision Tree for Rank Prediction")

# View the model summary
print(summary(decision_tree_model))

# Make predictions (optional)
predictions <- predict(decision_tree_model, decision_tree_data, type = "class")

# Add predictions to the dataset
decision_tree_data$Predictions <- predictions

# View the predictions
head(decision_tree_data)









