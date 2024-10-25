#Meena Mall

#Assignment 3

#Data Analytics

#THANK YOU!!!!

#a). Create boxplots for the “Cases” and “Deaths” variables comparing the variables
#between the 2 datasets, i.e. two figures (one for each variable) with 2 boxplots (for the 2
#different datasets) in each. Describe and run summary statistics on the two chosen variables and explain them in your words. min. 2-3 sentences (2%)



# Set working directory
setwd("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment3")

data_2020 <- read.csv("us-counties-2020.csv")
data_2021 <- read.csv("us-counties-2021.csv")



# Boxplot for "Cases"
boxplot(data_2020$cases, data_2021$cases, 
        names = c("2020", "2021"), 
        main = "Comparison of COVID-19 Cases (2020 vs 2021)",
        ylab = "Number of Cases", 
        col = c("blue", "green"))

# Boxplot for "Deaths"
boxplot(data_2020$deaths, data_2021$deaths, 
        names = c("2020", "2021"), 
        main = "Comparison of COVID-19 Deaths (2020 vs 2021)",
        ylab = "Number of Deaths", 
        col = c("lightcoral", "lightgoldenrod"))


summary(data_2020$cases)
summary(data_2021$cases)

summary(data_2020$deaths)
summary(data_2021$deaths)


# Cases: The number of COVID-19 cases rose sharply from 2020 to 2021. The median cases increased from 228 in 2020 to 2,778 in 2021, with the mean rising from 1,952 to 11,160. This growth likely reflects the emergence of new variants and relaxed restrictions.
# Deaths: COVID-19 deaths increased from 2020 to 2021, with the median rising from 4 to 52 and the mean from 53.6 to 193.6. This suggests that the higher case numbers in 2021 contributed to an increased death toll.







#########################################



#b). Create histograms for those two variables in the 2 datasets (you choose the histogram bin width). Describe the distributions in terms of known parametric distributions
#and similarities/ differences among them. Plot the distribution you think matches the
#histogram (e.g. normal, chis-square, gamma, t-distribution, etc.) overlayed on the
#histogram. min. 2-3 sentences (3%) "



setwd("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment3")

data_2020 <- read.csv("us-counties-2020.csv")
data_2021 <- read.csv("us-counties-2021.csv")

library(ggplot2)
library(scales)   
library(dplyr)   
library(MASS)     

data_2020 <- na.omit(data_2020)
data_2021 <- na.omit(data_2021)

data_2020 <- data_2020[data_2020$cases >= 0 & data_2020$deaths >= 0, ]
data_2021 <- data_2021[data_2021$cases >= 0 & data_2021$deaths >= 0, ]

# Calculate mean and standard deviation for cases
mean_cases_2020 <- mean(data_2020$cases)
sd_cases_2020 <- sd(data_2020$cases)
mean_cases_2021 <- mean(data_2021$cases)
sd_cases_2021 <- sd(data_2021$cases)

# Zoomed-in Histogram for "Cases"
ggplot() +
  geom_histogram(data = data_2020, aes(x = cases), bins = 30, fill = "blue", alpha = 0.6) +
  geom_histogram(data = data_2021, aes(x = cases), bins = 30, fill = "green", alpha = 0.6) +
  labs(title = "Histogram of COVID-19 Cases (2020 vs 2021)", x = "Number of Cases", y = "Frequency") +
  scale_y_continuous(labels = comma) +  # Format y-axis labels with commas
  xlim(0, 90000) +  # Zoom in to a more appropriate limit
  ylim(0, 200000) +  # Zoom in to a more appropriate limit
  theme_minimal() +
  stat_function(fun = dnorm, args = list(mean = mean_cases_2020, sd = sd_cases_2020), color = "darkblue", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mean_cases_2021, sd = sd_cases_2021), color = "darkgreen", size = 1)

# Calculate mean and standard deviation for deaths
mean_deaths_2020 <- mean(data_2020$deaths)
sd_deaths_2020 <- sd(data_2020$deaths)
mean_deaths_2021 <- mean(data_2021$deaths)
sd_deaths_2021 <- sd(data_2021$deaths)

# Histogram for "Deaths"  
ggplot() +
  geom_histogram(data = data_2020, aes(x = deaths), bins = 30, fill = "red", alpha = 0.6) +
  geom_histogram(data = data_2021, aes(x = deaths), bins = 30, fill = "purple", alpha = 0.6) +
  labs(title = "Histogram of COVID-19 Deaths (2020 vs 2021)", x = "Number of Deaths", y = "Frequency") +
  scale_y_continuous(labels = comma) +  # Format y-axis labels with commas
  xlim(0, 3000) +  # Adjust limits based on your data
  ylim(0, 400000) +  # Adjust limits based on your data
  theme_minimal() +
  stat_function(fun = dnorm, args = list(mean = mean_deaths_2020, sd = sd_deaths_2020), color = "darkred", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mean_deaths_2021, sd = sd_deaths_2021), color = "purple4", size = 1)


summary(data_2020)

summary(data_2021)


#The histograms for COVID-19 cases and deaths in 2020 and 2021 show a right-skewed distribution, starting high and gradually decreasing. This indicates that a few counties have very high counts compared to most. While the shape resembles a normal distribution, the presence of outliers and skewness points to a more complex distribution. The normal distribution overlay reinforces this, revealing significant variability and extreme values.


################################################



#c). Plot the ECDFs (Empirical Cumulative Distribution Function) for the two variables in
#both datasets. Plot the quantile-quantile distribution using a suitable parametric
#distribution you chose in 1b. Describe features of these plots. min. 2-3 sentences



setwd("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment3")

data_2020 <- read.csv("us-counties-2020.csv")
data_2021 <- read.csv("us-counties-2021.csv")

 
library(ggplot2)
library(dplyr)


data_2020 <- na.omit(data_2020)
data_2021 <- na.omit(data_2021)


data_2020 <- data_2020[data_2020$cases >= 0 & data_2020$deaths >= 0, ]
data_2021 <- data_2021[data_2021$cases >= 0 & data_2021$deaths >= 0, ]

# Plot ECDFs for Cases
ggplot() +
  stat_ecdf(data = data_2020, aes(x = cases), color = "blue") +
  stat_ecdf(data = data_2021, aes(x = cases), color = "green") +
  xlim(0, 1000000) +  # Adjust limits based on your data
  ylim(0, 1.50) +  # Adjust limits based on your data
  labs(title = "ECDF of COVID-19 Cases (2020 vs 2021)", x = "Number of Cases", y = "ECDF") +
  theme_minimal()

# Plot ECDFs for Deaths
ggplot() +
  stat_ecdf(data = data_2020, aes(x = deaths), color = "blue") +
  stat_ecdf(data = data_2021, aes(x = deaths), color = "orange") +
  xlim(0, 70000) +  # Adjust limits based on your data
  ylim(0, 1.20) +  # Adjust limits based on your data
  labs(title = "ECDF of COVID-19 Deaths (2020 vs 2021)", x = "Number of Deaths", y = "ECDF") +
  theme_minimal()

# Q-Q plot for Cases
qqnorm(data_2020$cases, main = "Q-Q Plot for COVID-19 Cases (2020)", col = "blue")
qqline(data_2020$cases, col = "darkblue")
qqnorm(data_2021$cases, main = "Q-Q Plot for COVID-19 Cases (2021)", col = "green")
qqline(data_2021$cases, col = "yellow")

# Q-Q plot for Deaths
qqnorm(data_2020$deaths, main = "Q-Q Plot for COVID-19 Deaths (2020)", col = "red")
qqline(data_2020$deaths, col = "darkred")
qqnorm(data_2021$deaths, main = "Q-Q Plot for COVID-19 Deaths (2021)", col = "purple")
qqline(data_2021$deaths, col = "purple4")



summary(data_2020$cases)
summary(data_2020$deaths)
summary(data_2021$cases)
summary(data_2021$deaths)






###########################################



#2. 6600-level question (3%). Filter the distributions you explored in Q1 by a number of
#states or counties. Repeat Q1b, Q1c and Q1d and draw any conclusions from this study.
#min. 3-4 sentences


setwd("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment3")


library(ggplot2)
library(dplyr)
library(MASS)


data_2020 <- read.csv("us-counties-2020.csv")
data_2021 <- read.csv("us-counties-2021.csv")


filtered_data_2020 <- data_2020 %>% filter(state %in% c("California", "New York"))
filtered_data_2021 <- data_2021 %>% filter(state %in% c("California", "New York"))


filtered_data_2020 <- filtered_data_2020 %>% filter(cases >= 0, deaths >= 0)
filtered_data_2021 <- filtered_data_2021 %>% filter(cases >= 0, deaths >= 0)

# Part 1b: Create histograms for the filtered data and overlay the distributions

# Calculate mean and standard deviation for filtered "Cases"
mean_cases_2020 <- mean(filtered_data_2020$cases)
sd_cases_2020 <- sd(filtered_data_2020$cases)
mean_cases_2021 <- mean(filtered_data_2021$cases)
sd_cases_2021 <- sd(filtered_data_2021$cases)

# Histogram for "Cases"  
ggplot() +
  geom_histogram(data = filtered_data_2020, aes(x = cases), bins = 30, fill = "blue", alpha = 0.6) +
  geom_histogram(data = filtered_data_2021, aes(x = cases), bins = 30, fill = "green", alpha = 0.6) +
  labs(title = "Histogram of COVID-19 Cases (Filtered States)", x = "Number of Cases", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  xlim(0, 500000) +  
  ylim(0, 10000) +  
  theme_minimal() +
  stat_function(fun = dnorm, args = list(mean = mean_cases_2020, sd = sd_cases_2020), color = "darkblue", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mean_cases_2021, sd = sd_cases_2021), color = "darkgreen", size = 1)

# Calculate mean and standard deviation for filtered "Deaths"
mean_deaths_2020 <- mean(filtered_data_2020$deaths)
sd_deaths_2020 <- sd(filtered_data_2020$deaths)
mean_deaths_2021 <- mean(filtered_data_2021$deaths)
sd_deaths_2021 <- sd(filtered_data_2021$deaths)

# Histogram for "Deaths" 
ggplot() +
  geom_histogram(data = filtered_data_2020, aes(x = deaths), bins = 30, fill = "red", alpha = 0.6) +
  geom_histogram(data = filtered_data_2021, aes(x = deaths), bins = 30, fill = "purple", alpha = 0.6) +
  labs(title = "Histogram of COVID-19 Deaths (Filtered States)", x = "Number of Deaths", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) +  
  xlim(0, 3000) +  
  ylim(0, 15000) +  
  theme_minimal() +
  stat_function(fun = dnorm, args = list(mean = mean_deaths_2020, sd = sd_deaths_2020), color = "darkred", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mean_deaths_2021, sd = sd_deaths_2021), color = "purple4", size = 1)

# Part 1c: Plot the ECDFs  

# ECDF for Cases  
ggplot() +
  stat_ecdf(data = filtered_data_2020, aes(x = cases), color = "blue") +
  stat_ecdf(data = filtered_data_2021, aes(x = cases), color = "green") +
  labs(title = "ECDF of COVID-19 Cases (Filtered States)", x = "Number of Cases", y = "ECDF") +
  theme_minimal()

# ECDF for Deaths  
ggplot() +
  stat_ecdf(data = filtered_data_2020, aes(x = deaths), color = "red") +
  stat_ecdf(data = filtered_data_2021, aes(x = deaths), color = "purple") +
  labs(title = "ECDF of COVID-19 Deaths (Filtered States)", x = "Number of Deaths", y = "ECDF") +
  theme_minimal()

# Q-Q plot for Cases  
qqnorm(filtered_data_2020$cases, main = "Q-Q Plot for COVID-19 Cases (2020 - Filtered States)", col = "blue")
qqline(filtered_data_2020$cases, col = "darkblue")
qqnorm(filtered_data_2021$cases, main = "Q-Q Plot for COVID-19 Cases (2021 - Filtered States)", col = "green")
qqline(filtered_data_2021$cases, col = "darkgreen")

# Q-Q plot for Deaths  
qqnorm(filtered_data_2020$deaths, main = "Q-Q Plot for COVID-19 Deaths (2020 - Filtered States)", col = "red")
qqline(filtered_data_2020$deaths, col = "darkred")
qqnorm(filtered_data_2021$deaths, main = "Q-Q Plot for COVID-19 Deaths (2021 - Filtered States)", col = "purple")
qqline(filtered_data_2021$deaths, col = "purple4")


summary(filtered_data_2020$cases)
summary(filtered_data_2020$deaths)
summary(filtered_data_2021$cases)
summary(filtered_data_2021$deaths)

















####################################################


#3. Using the NY house dataset:

#https://rpi.box.com/s/h3tfkjov93kga1b384mvjgz32915loj7

#a) Fit a linear model using the formula PRICE ~ BEDS + BATH + PROPERTYSQFT and
#identify the variable most significantly influencing house price. Produce a scatterplot of
#that variable with another and overlay the best fit line. Plot the residuals of the linear
#model. min. 2-3 sentences (2%)



setwd("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Assignment3")


library(ggplot2)
library(dplyr)

# Load the NY house dataset
ny_house_data <- read.csv("NY-House-Dataset.csv")


head(ny_house_data)

# Fit the linear model
linear_model <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny_house_data[ny_house_data$PRICE<195000000,])


summary(linear_model)



# Scatterplot with best fit line for PROPERTYSQFT
ggplot(ny_house_data[ny_house_data$PRICE<195000000,], aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatterplot of PRICE vs PROPERTYSQFT",
       x = "Property Square Footage (PROPERTYSQFT)",
       y = "Price (PRICE)") +
  theme_minimal()


# Remove rows with missing values in the relevant columns
cleaned_data <- na.omit(ny_house_data[, c("PRICE", "BEDS", "BATH", "PROPERTYSQFT")])

# Fit the linear model on the cleaned data
linear_model <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = cleaned_data)

# Calculate residuals
cleaned_data$residuals <- resid(linear_model)

# Plot residuals
ggplot(cleaned_data, aes(x = fitted(linear_model), y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals of the Linear Model",
       x = "Fitted Values",
       y = "Residuals") +
    xlim(0, 40000000) +  
    ylim(0, 30000000) +  
  theme_minimal()



#The linear regression analysis reveals that property square footage (PROPERTYSQFT) is the most significant factor influencing house price, with a p-value of 4.38e-09 indicating a strong relationship. The scatterplot shows that as property size increases, so does the price. The residual plot indicates consistent variance around zero across the fitted values.






############################






#b) Derive a subset of the dataset according to any criteria (e.g. PRICE > VALUE or BEDS <
#NUMBER) and repeat the linear model with its plots. Explain how the significance of the
#input variables changes and your interpretation of the change. min. 2-3 sentences (3%)



value_threshold <- 300000  # Define your price threshold
subset_data <- ny_house_data %>% filter(PRICE > value_threshold)


subset_model <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = subset_data)


summary(subset_model)

# Scatterplot with best fit line for PROPERTYSQFT in the subset
ggplot(subset_data, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatterplot of PRICE vs PROPERTYSQFT (Subset)",
       x = "Property Square Footage (PROPERTYSQFT)",
       y = "Price (PRICE)") +
  xlim(0, 40000) +  
  ylim(0, 30000000) + 
  theme_minimal()

# Calculate residuals for the subset
subset_data$residuals <- resid(subset_model)

# Plot residuals for the subset
ggplot(subset_data, aes(x = fitted(subset_model), y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals of the Linear Model (Subset)",
       x = "Fitted Values",
       y = "Residuals") +
  xlim(0, 400000) +  
  ylim(0, 2000000) + 
  theme_minimal()


#In our analysis of houses over $300,000, we found that size (PROPERTYSQFT) is still very important for price, with a strong p-value of 3.22e-08. The number of bedrooms (BEDS) isn't significant anymore, with a p-value of 0.1401. However, the number of bathrooms (BATH) is significant, with a p-value of 0.0216, indicating that for more expensive homes, size and bathrooms are more important than the number of bedrooms.
