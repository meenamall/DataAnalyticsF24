# Set the working directory to your specific folder
setwd("C:\Users\Meena\Desktop\DataAnalytics\DataAnalytics")
list.files()  


# Read the CSV file into R
EPI_data <- read.csv("epi2024results06022024.csv")

# View the data
View(EPI_data)









attach(EPI_data) # sets the ‘default’ object
EPI.new # prints out values EPI_data$EPI.new
[1] "30.7" "52.1" "41.9" "39.7" "55.5" "46.8" "44.7" "63" "69" "40.4" "56" "35.9"
[13] "27.8" "53.1" "58.1" "66.7" "47.4" "37.4" "43.3" "44.9" "45.6" "49" "53" "48.5"
[25] "56.3" "41.5" "33" "37.9" "31" "38.1" "61.1" "38.3" "35.2" "50" "35.5" "49.4"
[37] "37.9" "55.5" "42.5" "62.6" "52.3" "54" "65.6" "39" "67.9" "32.2" "49.2" "47.6"
[49] "51.2" "43.8" "41.5" "41.6" "28.6" "75.3" "38.5" "35.8" "45.8" "73.7" "67.1" "53.1"
tf <- is.na(EPI.new) # records True values if the value is NA
E <- EPI.new[!tf] # filters out NA values, new array


summary(EPI.new) # stats
fivenum(EPI.new, na.rm=TRUE)
stem(EPI.new) # stem and leaf plot
hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw=1)) # or try bw="SJ"
rug(EPI.new)
#Use help(<command>), e.g. > help(stem) 


boxplot(EPI.new, APO.new)



hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines (density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)


hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw="SJ"))
rug(EPI.new) 



x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q) 




plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 

qqnorm(EPI.new); qqline(EPI.new) 

qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new)

qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn")
qqline(EPI.new)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#NEW SECTION FOR APO !!!!!
#APO.NEW

APO.new # prints out values from EPI_data$APO.new
tf_APO <- is.na(APO.new) # records True values if the value is NA
APO_filtered <- APO.new[!tf_APO] # filters out NA values, new array


range(APO.new, na.rm = TRUE)


summary(APO.new) # stats
fivenum(APO.new, na.rm=TRUE)
stem(APO.new) # stem and leaf plot
hist(APO.new)


summary(APO.new)
hist(APO.new, seq(0,100.,10), prob=TRUE) #THIS WORKS KEEP CONTINUING!!
lines(density(APO.new, na.rm=TRUE, bw=1)) # or try bw="SJ"
rug(APO.new)

boxplot(APO.new, APO.new)

hist(APO.new, seq(0,100.,10), prob=TRUE)
lines (density(APO.new,na.rm=TRUE,bw=1.))
rug(APO.new)

hist(APO.new, seq(0,100.,10), prob=TRUE)
lines(density(APO.new, na.rm=TRUE, bw="SJ"))
rug(APO.new) 


x<-seq(0,100,10)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q) 


plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE) 

qqnorm(APO.new); qqline(APO.new) 

qqplot(rnorm(250), APO.new, xlab = "Q-Q plot for norm dsn")
qqline(APO.new)

qqplot(rt(250, df = 5), APO.new, xlab = "Q-Q plot for t dsn")
qqline(APO.new)




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#NEW SECTION FOR WRS !!!!!
#WRS

WRS_data <- read.csv("epi2024results06022024.csv")

View(WRS_data)

attach(WRS_data) # sets the ‘default’ object
WRS.new # prints out values EPI_data$EPI.new

tf_WRS <- is.na(WRS.new) # records True values if the value is NA
WRS_filtered <- WRS.new[!tf_WRS] # filters out NA values, new array


range(WRS.new, na.rm = TRUE)


summary(WRS.new) # stats
fivenum(WRS.new, na.rm=TRUE)
stem(WRS.new) # stem and leaf plot
hist(WRS.new)

summary(WRS.new)
hist(WRS.new, seq(0,100.,1.0), prob=TRUE) #THIS WORKS KEEP CONTINUING!!
lines(density(WRS.new, na.rm=TRUE, bw=1)) # or try bw="SJ"
rug(WRS.new)


boxplot(WRS.new, WRS.new)

hist(WRS.new, seq(0,100.,1.0), prob=TRUE)
lines (density(WRS.new,na.rm=TRUE,bw=1.))
rug(WRS.new)

hist(WRS.new, seq(0,100.,1.0), prob=TRUE)
lines(density(WRS.new, na.rm=TRUE, bw="SJ"))
rug(WRS.new) 


x<-seq(0,100,1.0)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q) 


plot(ecdf(WRS.new), do.points=FALSE, verticals=TRUE) 

qqnorm(WRS.new); qqline(WRS.new) 

qqplot(rnorm(250), WRS.new, xlab = "Q-Q plot for norm dsn")
qqline(WRS.new)

qqplot(rt(250, df = 5), WRS.new, xlab = "Q-Q plot for t dsn")
qqline(WRS.new)