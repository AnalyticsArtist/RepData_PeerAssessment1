# Personal Activity Monitoring Analysis
========================================================

## Background
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data Preparation

```r
# Load Libraries
library(chron)

# Define Data Path
data_path <- "/Users/gabrielm/OneDrive/Documents/HW/Coursera/Data Science Specialization/5 - Reproducible Research/Projects/Project 1"
setwd(data_path)

# Download & Read Data: Download was only ran once.
# download.file(url='http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip',
# destfile='Data/activity.zip')
data <- read.csv(unz("Data/activity.zip", "activity.csv"))

# Format Dates and Intervals
data$date <- as.Date(data$date)
data$hour <- data$interval%/%100
data$minute <- data$interval%%100

data$DateTime <- strptime(paste(data$date, data$hour, data$minute), format = "%Y-%m-%d %H %M")
```


## Question 1: What is the mean total number of steps taken per day?


The mean total number of steps taken per day is 9354.2295.
The median total number of steps taken per day is 10395.

```r
# Calculate total steps per day
StepsByDate <- tapply(X = data$steps, INDEX = data$date, FUN = sum, na.rm = T)

# Plot histogram of total steps per day
hist(StepsByDate, main = "Histogram of Steps by Date", xlab = "Steps")
rug(StepsByDate)
```

![plot of chunk Question1](figure/Question1.png) 

```r

# Calculate mean & media total stpes per day
mean(StepsByDate)
```

```
## [1] 9354
```

```r
median(StepsByDate)
```

```
## [1] 10395
```


## Question 2: What is the average daily activity pattern?


The data pattern shows people usually start exercising around 6:00AM every day with the peak around 08:35AM with 206 steps.

```r
# Calculate total steps per day
ActivityByInterval <- aggregate(data$steps ~ format(data$DateTime, "%H:%M"), 
    FUN = mean, na.rm = T)
names(ActivityByInterval) <- c("Hour_Minute", "Avg_Steps")

ActivityByInterval.max <- ActivityByInterval[ActivityByInterval$Avg_Steps == 
    max(ActivityByInterval$Avg_Steps), ]
ActivityByInterval.maxDate <- ActivityByInterval.max[, "Hour_Minute"]
ActivityByInterval.maxStep <- round(ActivityByInterval.max[, "Avg_Steps"], digits = 0)

# Define a formatter function - converts time from string to strptime
timeHM_formatter <- function(x) {
    return(strptime(paste(Sys.Date(), x), format = "%Y-%m-%d %H:%M"))
}

# Plot
plot(x = timeHM_formatter(ActivityByInterval$Hour_Minute), y = ActivityByInterval$Avg_Steps, 
    type = "l", main = "Average Daily Activity by Interval", xlab = "5 Minute Daily Intervals", 
    ylab = "Mean Steps per Day", ylim = c(0, 250))
text(x = timeHM_formatter(ActivityByInterval.maxDate), y = ActivityByInterval.maxStep + 
    5, ActivityByInterval.maxStep)
```

![plot of chunk Question2](figure/Question2.png) 


## Questions 3: Impute Missing Values
The strategy we recommend is to use the mean steps per interval.

```r
# Number of missing values in the dataset
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r

# Separate complete cases from missing cases
data.complete <- data[!is.na(data$steps), ]
data.missing <- data[is.na(data$steps), ]

# Merge average steps per interval with missing cases
data.missing$Hour_Minute <- format(data.missing$DateTime, "%H:%M")
data.missing <- merge(x = data.missing, y = ActivityByInterval, by = "Hour_Minute")
data.missing$steps <- data.missing$Avg_Steps
data.missing <- data.missing[, names(data.complete)]

# Combine complete cases dataset with imputed dataset
data2 <- rbind(data.complete, data.missing)
rm(data.complete, data.missing)

# Calculate total steps per day
StepsByDate.imputted <- tapply(X = data2$steps, INDEX = data2$date, FUN = sum)

# Plot histogram of total steps per day
hist(StepsByDate.imputted, main = "Histogram of Steps by Date (w Imputted Values)", 
    xlab = "Steps")
rug(StepsByDate.imputted)
```

![plot of chunk Question3](figure/Question3.png) 

```r

# Calculate mean & media total stpes per day
mean(StepsByDate.imputted)
```

```
## [1] 10766
```

```r
median(StepsByDate.imputted)
```

```
## [1] 10766
```


## Question 4: Are there differences in activity patterns between weekdays and weekends?
Weekday activities are mostly centerned around the morning with additional three smaller peaks at noon, 5PM and 9PM. This is not surprising as people usually like to run the most before work, and they also run during lunch, right after work and in evenings. Weekend activities, are slightly more distributed then the weekday activities. People still run in the morning time but not as much as they do during weekday. They also run with similar intencity to their weekend moringing time throughout the day between 12PM - 4PM.

```r
data2$weekday <- factor(!is.weekend(data2$date), labels = c("weekend", "weekday"))

data2.weekday <- data2[data2$weekday == "weekday", ]
data2.weekend <- data2[data2$weekday == "weekend", ]

ActivityByInterval.weekday <- aggregate(data2.weekday$steps ~ format(data2.weekday$DateTime, 
    "%H:%M"), FUN = mean, na.rm = T)
ActivityByInterval.weekend <- aggregate(data2.weekend$steps ~ format(data2.weekend$DateTime, 
    "%H:%M"), FUN = mean, na.rm = T)
names(ActivityByInterval.weekday) <- c("Hour_Minute", "Avg_Steps")
names(ActivityByInterval.weekend) <- c("Hour_Minute", "Avg_Steps")

par(mfrow = c(2, 1))
plot(x = timeHM_formatter(ActivityByInterval.weekday$Hour_Minute), y = ActivityByInterval.weekday$Avg_Steps, 
    type = "l", main = "Weekday Activities", xlab = "5 Minute Daily Intervals", 
    ylab = "Mean Steps", ylim = c(0, 250))
plot(x = timeHM_formatter(ActivityByInterval.weekend$Hour_Minute), y = ActivityByInterval.weekend$Avg_Steps, 
    type = "l", main = "Weekend Activites", xlab = "5 Minute Daily Intervals", 
    ylab = "Mean Steps", ylim = c(0, 250))
```

![plot of chunk Question4](figure/Question4.png) 



