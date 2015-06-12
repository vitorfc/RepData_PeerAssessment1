---
title: "Reproducible Research: Peer Assessment 1"
---

## Loading and preprocessing the data
Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(ggplot2)
library(knitr)
setwd("C:/R/RepSearch/RepData_PeerAssessment1")
unzip("activity.zip")

data<-read.csv(file="activity.csv")
tot<-nrow(data)
data.ok<-data[!is.na(data$steps),]
tot.ok<-nrow(data.ok)
```
Total Rows: 17568

Total Rows Without NAs: 15264

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
steps.date <- aggregate(steps ~ date, data=data.ok, FUN=sum)
hist(steps.date$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps.interval <- aggregate(steps ~ interval, data=data, FUN=mean)
plot(steps.interval, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I´ll use the means of the 5-minutes intervals to fill the missing values.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newds <- merge(data, steps.interval, by="interval", suffixes=c("",".y"))
nas <- is.na(newds$steps)
newds$steps[nas] <- newds$steps.y[nas]
newds <- newds[,c(1:3)] 
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps.byday <- aggregate(steps ~ date, data=newds, FUN=sum)

hist(steps.byday$steps)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
mean(steps.byday$steps)
```

```
## [1] 10766.19
```

```r
median(steps.byday$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("sábado", "domingo")) {
        "weekend"
    } else {
        "weekday"
    }
}
newds$daytype <- as.factor(sapply(newds$date, daytype))
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
averages <- aggregate(steps ~ interval + daytype, data=newds, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .) +    xlab("5-minute interval") + ylab("Total steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
