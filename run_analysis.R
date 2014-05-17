## Program     : Coursera Data Science - Reproducible Research Project 1
## Written By  : Gabriel Mohanna
## Date Created: May 9, 2014
##
## Narrative   : 
##
## Background  : 
##
## TBD         : 
##
## \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Code is Poetry >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
##
## **********************************************************************************************************************
## Steps
## -----
## (0) Load Libraries & Define Working Directory
## (1) Load and Preprocess the Data
## (2) Mean Total Number of Steps Taken Per Day?
## (3) Average Daily Activity Pattern
## (4) Impute Missing Values
## (5) Differences Between Weekdays and Weekends Activity Patterns
##
## **********************************************************************************************************************
## Notes
## ---------------------
##
## **********************************************************************************************************************

# ***********************************************************************************************************************
# (0) Load Libraries & Define Working Directory
# ***********************************************************************************************************************
# Load Libraries
library(lattice)
library(ggplot2)
library(chron)

# Define Data Path
data_path <- "D:/Users/gmohanna/SkyDrive/Documents/HW/Coursera/Data Science Specialization/5 - Reproducible Research/Projects/Project 1"
setwd(data_path)

# End Load Libraries & Define Working Directory


# ***********************************************************************************************************************
# (1) Load and Preprocess the Data
# ***********************************************************************************************************************
# Read data
data        <- read.csv("Data/activity.csv")

# Convert date from factor to date
data$date   <- as.Date(data$date)

# Convert interval from int to hour & minute
data$hour   <- data$interval %/% 100
data$minute <- data$interval %%  100

# Create Date/Time stamp
data$DateTime <- strptime(paste(data$date, data$hour, data$minute), format="%Y-%m-%d %H %M")

# End Load and Preprocess the Data


# ***********************************************************************************************************************
# (2) Mean Total Number of Steps Taken Per Day?
# ***********************************************************************************************************************
# Calculate total steps per day
StepsByDate <- tapply(X=data$steps, INDEX=data$date, FUN=sum, na.rm=T)

# Plot histogram of total steps per day
hist(StepsByDate, main="Histogram of Steps by Date", xlab="Steps")
rug(StepsByDate)

# Calculate mean & media total stpes per day
mean(StepsByDate)
median(StepsByDate)

# End Mean Total Number of Steps Taken Per Day?


# ***********************************************************************************************************************
# (3) Average Daily Activity Pattern
# ***********************************************************************************************************************
# Calculate total steps per day
ActivityByInterval <- aggregate(data$steps ~ format(data$DateTime, "%H:%M"), FUN=mean, na.rm=T)
names(ActivityByInterval) <- c("Hour_Minute", "Avg_Steps")

ActivityByInterval.max <- ActivityByInterval[ActivityByInterval$Avg_Steps==max(ActivityByInterval$Avg_Steps),]
ActivityByInterval.maxDate <- ActivityByInterval.max[, "Hour_Minute"]
ActivityByInterval.maxStep <- round(ActivityByInterval.max[, "Avg_Steps"],digits=0)

# Define a formatter function - converts time from string to strptime
timeHM_formatter <- function(x) {
    return(strptime(paste(Sys.Date(), x), format="%Y-%m-%d %H:%M"))
}

# Plot
plot(x=timeHM_formatter(ActivityByInterval$Hour_Minute),
     y=ActivityByInterval$Avg_Steps, 
     main="Average Daily Activity by Interval", 
     xlab="5 Minute Daily Intervals", ylab="Mean Steps per Day", type="l")
text(x=timeHM_formatter(ActivityByInterval.maxDate), y=ActivityByInterval.maxStep+5, ActivityByInterval.maxStep)

ActivityByInterval$Date.Time <- timeHM_formatter(ActivityByInterval$Hour_Minute)
ggplot(ActivityByInterval, aes(Date.Time, Avg_Steps)) + geom_line() + annotate("text", label=ActivityByInterval.maxStep, x=ActivityByInterval.maxDate, y=ActivityByInterval.maxStep+5)

# End Average Daily Activity Pattern


# ***********************************************************************************************************************
# (4) Impute Missing Values
# ***********************************************************************************************************************
# Number of missing values in the dataset
sum(is.na(data$steps))

# Separate complete cases from missing cases
data.complete <- data[!is.na(data$steps),]
data.missing  <- data[ is.na(data$steps),]

# Merge average steps per interval with missing cases
data.missing$Hour_Minute <- format(data.missing$DateTime, "%H:%M")
data.missing <- merge(x=data.missing, y=ActivityByInterval, by="Hour_Minute")
data.missing$steps <- data.missing$Avg_Steps
data.missing <- data.missing[, names(data.complete)]

# Combine complete cases dataset with imputed dataset
data2 <- rbind(data.complete, data.missing)
rm(data.complete, data.missing)

# Calculate total steps per day
StepsByDate.imputted <- tapply(X=data2$steps, INDEX=data2$date, FUN=sum)

# Plot histogram of total steps per day
hist(StepsByDate.imputted, main="Histogram of Steps by Date (w Imputted Values)", xlab="Steps")
rug(StepsByDate.imputted)

# Calculate mean & media total stpes per day
mean(StepsByDate.imputted)
median(StepsByDate.imputted)

# End Impute Missing Values


# ***********************************************************************************************************************
# (5) Differences Between Weekdays and Weekends Activity Patterns
# ***********************************************************************************************************************
data2$weekday <- factor(!is.weekend(data2$date), labels=c("weekend", "weekday"))

data2.weekday <- data2[data2$weekday=="weekday",]
data2.weekend <- data2[data2$weekday=="weekend",]

ActivityByInterval.weekday <- aggregate(data2.weekday$steps ~ format(data2.weekday$DateTime, "%H:%M"), FUN=mean, na.rm=T)
ActivityByInterval.weekend <- aggregate(data2.weekend$steps ~ format(data2.weekend$DateTime, "%H:%M"), FUN=mean, na.rm=T)
names(ActivityByInterval.weekday) <- c("Hour_Minute", "Avg_Steps")
names(ActivityByInterval.weekend) <- c("Hour_Minute", "Avg_Steps")

par(mfrow=c(2,1))
plot(x=timeHM_formatter(ActivityByInterval.weekday$Hour_Minute),
     y=ActivityByInterval.weekday$Avg_Steps, 
     type="l", 
     main="Weekday Activities", 
     xlab="5 Minute Daily Intervals", ylab="Mean Steps",
     ylim=c(0,250))
plot(x=timeHM_formatter(ActivityByInterval.weekend$Hour_Minute),
     y=ActivityByInterval.weekend$Avg_Steps, 
     type="l"
     main="Weekend Activites", 
     xlab="5 Minute Daily Intervals", ylab="Mean Steps",
     ylim=c(0,250))
par(mfrow=c(1,1))

# End Differences Between Weekdays and Weekends Activity Patterns
