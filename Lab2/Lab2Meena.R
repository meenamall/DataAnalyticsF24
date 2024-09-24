library(ggplot2)

setwd("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab2/")


epi.results <- read.csv("epi2024results06022024.csv", header=TRUE)
epi.weights <- read.csv("epi2024weights.csv")
epi.countries <- read.csv("countries_populations_2023.csv")


View(epi.results)
View(epi.weights)
View(epi.countries)



 #### Exploratory Analysis ####

epi.results$EPI.new
epi.results[1,5]
attach(epi.results)
EPI.new
EPI.new[1]
## NA values
na.indices <- is.na(EPI.new) 

## drop NAs
Epi.new.compl <- EPI.new[!na.indices]

## convert to data frame and add country
Epi.new.compl <- data.frame(Country = country[!na.indices], EPI = EPI.new[!na.indices])



#BDH !!!!! 
BDH <- epi.results$BDH  # Assuming BDH is the column name in epi.results
names(epi.results)
BDH.new <- epi.results$BDH.new  # Assigning the BDH.new column to BDH.new
BDH.new <- BDH.new[!is.na(BDH.new)]  # Remove NA values

# ECS !!!!!
ECS <- epi.results$ECS  # Assuming ECS is the column name in epi.results
names(epi.results)
ECS.new <- epi.results$ECS.new  # Assigning the ECS.new column to ECS
ECS.new <- ECS.new[!is.na(ECS.new)]  # Remove NA values


#starting exercise 1 (slides he provided)
help("qqnorm") #read the Rstudio documentation for qqnorm
qqnorm(EPI.new); qqline(EPI.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

plot(ecdf(EPI.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(EPI.new))

#Your exercise(1): do the same exploration and fitting for another 2 variables

#BDH
help("qqnorm")  
qqnorm(BDH.new); qqline(BDH.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),BDH.new)
qqline(BDH.new)

plot(ecdf(BDH.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(BDH.new))

# Chi-squared Q-Q plot for BDH
#USING qchisq
qqplot(qbeta(ppoints(200), shape1=2, shape2=5), BDH.new) # Beta distribution with shape parameters
qqline(BDH.new)



#ECS
help("qqnorm") 
qqnorm(ECS.new); qqline(ECS.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),ECS.new)
qqline(ECS.new)

plot(ecdf(ECS.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) 
lines(ecdf(ECS.new))

qqplot(qweibull(ppoints(200), shape=2, scale=5), ECS.new)  # Adjust shape and scale parameters as needed
qqline(ECS.new)

#FINISHED EXERCISE 1



#Comparing Distributions
boxplot(EPI.old, EPI.new, names=c("EPI.old","EPI.new"))

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE)
lines(ecdf(EPI.new))

plot(ecdf(EPI.old), do.points=FALSE, main="EPI.old vs. EPI.new ECDF")
lines(ecdf(EPI.new))



#EXERCISE 2

library(ggplot2)  # For plotting


#inputing slide 1 code


# read data
populations_2023 <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab2/countries_populations_2023.csv")

# drop countries not in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

# sort populations by country
populations <- populations[order(populations$Country),]

# drop countries not in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

# sort epi results by country
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

# only keep necessary columns
epi.results.sub <- epi.results.sub[,c("country","EPI.old","EPI.new")]

# convert population to numeric
epi.results.sub$population <- as.numeric(populations$Population)

# compute population log base 10
epi.results.sub$population_log <- log10(epi.results.sub$population)



#Moving onto slide 2 code 

attach(epi.results.sub)

lin.mod.epinew <- lm(EPI.new ~ population_log, epi.results.sub)

plot(EPI.new ~ population_log)
abline(lin.mod.epinew)


summary(lin.mod.epinew)
plot(lin.mod.epinew)

ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")


ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')




# Exercise 2 ECO!!!!!!!

library(ggplot2)  # For plotting

populations_2023 <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab2/countries_populations_2023.csv")

populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

populations <- populations[order(populations$Country),]

epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

epi.results.sub <- epi.results.sub[,c("country","ECO.old","ECO.new")]

epi.results.sub$population <- as.numeric(populations$Population)

epi.results.sub$population_log <- log10(epi.results.sub$population)



attach(epi.results.sub)

lin.mod.eco <- lm(ECO.new ~ population_log, epi.results.sub)

plot(ECO.new ~ population_log)
abline(lin.mod.eco)

summary(lin.mod.eco)

ggplot(epi.results.sub, aes(x = population_log, y = ECO.new)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title = "ECO vs Population Log", x = "Log of Population", y = "ECO")

ggplot(lin.mod.eco, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = 'Residual vs. Fitted Values Plot for ECO', x = 'Fitted Values', y = 'Residuals')





# Exercise 2 BDH!!!!!!!

length(epi.results.sub$BDH.new)
length(epi.results.sub$population_log)

sum(is.na(epi.results.sub$BDH.new))
sum(is.na(epi.results.sub$population_log))

epi.results.sub <- na.omit(epi.results.sub)


library(ggplot2)  # For plotting

populations_2023 <- read.csv("C:/Users/Meena/Desktop/DataAnalytics/DataAnalytics/Lab2/countries_populations_2023.csv")

populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

populations <- populations[order(populations$Country),]

epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

epi.results.sub <- epi.results.sub[, c("country", "BDH.old", "BDH.new")]

epi.results.sub$population <- as.numeric(populations$Population)

epi.results.sub$population_log <- log10(epi.results.sub$population)


attach(epi.results.sub)

lin.mod.bdh <- lm(BDH.new ~ population_log, data = epi.results.sub)

plot(BDH.new ~ population_log, data = epi.results.sub)
abline(lin.mod.bdh)

summary(lin.mod.bdh)

ggplot(epi.results.sub, aes(x = population_log, y = BDH.new)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title = "BDH vs Population Log", x = "Log of Population", y = "BDH")


ggplot(lin.mod.bdh, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = 'Residual vs. Fitted Values Plot for BDH', x = 'Fitted Values', y = 'Residuals')