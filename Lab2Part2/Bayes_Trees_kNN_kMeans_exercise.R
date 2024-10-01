library("e1071")

insta

iris

iris.df <- iris

?naiveBayes

classifier <- naiveBayes(iris[,1:4], iris[,5])

classifier

prediction <- predict(classifier, iris[,-5])

prediction

table(prediction, iris[,5], dnn=list('predicted','actual'))

classifier$apriori

classifier$tables$Petal.Length

plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species") 

curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue") 

curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")

library(rpart)

library(rpart.plot)

dim(iris)

s_iris <- sample(150,100)

s_iris

iris_train <-iris[s_iris,]

iris_test <-iris[-s_iris,] 

dim(iris_test)
dim(iris_train) 

?rpart

dectionTreeModel <- rpart(Species~., iris_train, method = "class") 

dectionTreeModel

rpart.plot(dectionTreeModel) 


############################
### Abelone ####

abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")

colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old')) 
abalone$rings <- as.factor(abalone$rings)

z <- abalone
aba <- abalone
aba$sex <- NULL 

normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }

aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))

summary(aba$shucked_wieght)

ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)

library(class)

KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)

KNNpred
table(KNNpred,KNNtest$rings)

