setwd("C:/Users/Neil/SkyDrive/Data Science/Reproducible Research/repdata-data-activity")

## loads in the "activity" dataset and removes missing values
data <- read.csv("activity.csv", header = TRUE, sep = ",") 
cleandata <- data[!is.na(data),]

## aggregates total steps by date
steps_per_day <- aggregate(steps ~ date, data = cleandata, FUN = sum)

## calculates mean and median of the total number of steps taken per day
stepsMean <- mean(steps_per_day$steps)
print(stepsMean)
stepsMedian <- median(steps_per_day$steps)
print(stepsMedian)

## creates histogram of steps taken per day
library(ggplot2)

png("fig1.png", width = 1000, height = 500)
ggplot(data = steps_per_day, aes(x = steps_per_day$steps)) + geom_histogram(fill = "steelblue", binwidth = 1000) + 
    xlab("Steps per Day") + ggtitle("Distribution of Steps per Day") + geom_vline(xintercept = stepsMean, size = 1) +
    theme(plot.title = element_text(size = 20, face = "bold", vjust = 2))
dev.off()

## creates time series plot of average number of steps taken per 5-minute interval
steps_per_interval <- aggregate(steps ~ interval, data = cleandata, FUN = mean)

png("fig2.png", width = 1000, height = 500)
ggplot(data = steps_per_interval, aes(x = interval, y = steps)) + geom_line(color = "steelblue", size = 1) + 
    xlab("Interval") + ylab("Steps") + ggtitle("Average Number of Steps per 5 Minute Interval") +
    theme(plot.title = element_text(size = 20, face = "bold", vjust = 2))
dev.off()

## calculates which interval has, on average, the maximum number of steps
maxSteps <- max(steps_per_interval$steps)
maxSteps_interval <- steps_per_interval$interval[which(steps_per_interval$steps == maxSteps)]
print(maxSteps_interval)

## calculates and reports the total number of missing values in the dataset
num_NA <- sum(is.na(data))
print(num_NA)

## replaces NA values with the means for that interval
data_filledNA <- data
for(i in 1:nrow(data_filledNA)){
    if (is.na(data_filledNA$steps[i])) {
        intervalIndex <- data_filledNA$interval[i]
        data_filledNA$steps[i] <- steps_per_interval$steps[steps_per_interval$interval == intervalIndex]
    }
}

## creates a histogram of the total number of steps taken each day with the Data_filledNA dataset
steps_per_day_filledNA <- aggregate(steps ~ date, data = data_filledNA, FUN = sum)
png("fig3.png", width = 1000, height = 500)
ggplot(data = steps_per_day_filledNA, aes(x = steps_per_day_filledNA$steps)) + geom_histogram(fill = "steelblue", binwidth = 1000) + 
    xlab("Steps per Day") + ggtitle("Distribution of Steps per Day") + geom_vline(xintercept = mean(steps_per_day_filledNA$steps), size = 1) +
    theme(plot.title = element_text(size = 20, face = "bold", vjust = 2))
dev.off()

## Creates a new factor variable in the dataset with "weekday" and "weekend" levels 
data_filledNA$date <- as.Date(data_filledNA$date)
for(i in 1:nrow(data_filledNA)){
    if(weekdays(data_filledNA$date[i]) == "Saturday" | weekdays(data_filledNA$date[i]) == "Saturday") {
        data_filledNA$dayType[i] = "weekend"
    }
    else data_filledNA$dayType[i] = "weekday"
}

## creates a panel plot containing a time series plot of the 5-minute intercal and the average number of steps taken, averaged across all weekdays/weekends
steps_per_interval_filledNA <- aggregate(steps ~ interval + dayType, data = data_filledNA, FUN = mean)

png("fig4.png", width = 1000, height = 500)
ggplot(data = steps_per_interval_filledNA, aes(x = interval, y = steps)) + 
    geom_line(color = "steelblue", size = 1) + 
    facet_grid(dayType ~ .) + 
    xlab("Interval") + ylab("Steps") + ggtitle("Average Number of Steps per 5 Minute Interval") +
    theme(plot.title = element_text(size = 20, face = "bold", vjust = 2))
dev.off()