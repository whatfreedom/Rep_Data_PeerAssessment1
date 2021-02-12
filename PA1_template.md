---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


Load the dplyr package


```r
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data

The code data has already been downloaded when I cloned the project repo, so we just need to unzip the zipfile and load it into Rstudio. We will store the activity data as a variable called data. 


```r
unzip(zipfile="activity.zip")

data <- read.csv(file="activity.csv")
```

## What is the mean total number of steps taken per day?

Missing values will be ignored for these analyses.

First, we need to group the data by date and summarize it by the sum to get the total number of steps taken each day (making sure to remove the NA rows and days). Then we can make a histogram with the total number of steps.


```r
dailysteps <- data %>%
  group_by(date) %>%
  summarize(total_steps=sum(steps)) %>%
  filter(!is.na(total_steps))

hist(dailysteps$total_steps, 
     main="Total daily steps in October-November 2012",
     xlab="Number of Steps",
     col="lightblue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean and median number of steps taken per day.

```r
mean(dailysteps$total_steps)
```

```
## [1] 10766.19
```

```r
median(dailysteps$total_steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

We need to convert the date column values to the date class using as.Date. We then can plot the date and total daily steps to see a time progression. To smooth the data out, I created a 5-day rolling average in a red line.


```r
#### Time series plot of number/steps taken
dailysteps$date <- as.Date(dailysteps$date, format = "%Y-%m-%d")

plot(dailysteps$date, dailysteps$total_steps, 
     type="l", 
     col="darkblue", 
     lwd=3,
     main="Daily steps taken in October-November 2012",
     xlab="Date",
     ylab="Total number of steps")
grid()

# turn total_steps to numeric
dailysteps$total_steps <- as.numeric(dailysteps$total_steps)

# 5-day rolling average
fiveDay <- rep(1/5, 5)
### we need the base stats filter func here
fiveDayAve <- stats::filter(dailysteps$total_steps, fiveDay, sides=2)

lines(dailysteps$date, fiveDayAve, col="red", lwd=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Now let's find which 5-minute time interval on average contains the max number of steps. To do this we need to group the original dataset by the interval and summarize the steps.


```r
intervalData <- data %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm=TRUE))

intervalData[which.max(intervalData$mean_steps),]
```

```
## # A tibble: 1 x 2
##   interval mean_steps
##      <int>      <dbl>
## 1      835       206.
```
The 835th minute interval contains the most steps on average.

## Imputing missing values

Let's first see how many rows are missing values and have NA values instead.

```r
NAs <- which(is.na(data$steps))
naRows <- data[NAs,]
nrow(naRows)
```

```
## [1] 2304
```
We have 2304 rows with NA values.

Now let's impute the data for these missing rows. We will take the mean value of other days which we have a value and use that to replace the NA values.

Taking the original data, we will loop through the rows and if the steps data is NA, we will take the mean from our previous data (the mean number of steps for each interval) and replace the value. Then we can take this new data, and group it by the date and make another histogram with the total number of steps each day with the completed 2 month period.


```r
imputedData <- data

for (i in 1:length(imputedData$steps)) {
  if (is.na(imputedData$steps[i])) {
    imputedData$steps[i] <- intervalData$mean_steps[intervalData$interval == imputedData$interval[i]]
  }
}

imputeddailysteps <- imputedData %>%
  group_by(date) %>%
  summarize(total_steps=sum(steps)) %>%
  filter(!is.na(total_steps))

hist(imputeddailysteps$total_steps, 
     main="Steps walked daily in October-November 2012",
     xlab="Number of Steps",
     col="lightblue")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Here are the new mean and median values. They end up being the same!

```r
mean(imputeddailysteps$total_steps)
```

```
## [1] 10766.19
```

```r
median(imputeddailysteps$total_steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

To see what the difference between weekdays and weekends is, we need to create a new variable for weekday or weekend. To do this we'll use the weekdays function after turning the date column values to POSIX class. After that we can categorize the day using a simple ifelse statement. 


```r
imputedData$date <- strptime(imputedData$date, format = "%Y-%m-%d")

imputedData$day <- weekdays(imputedData$date)

imputedData$dayType <- ifelse(imputedData$day == 'Saturday' | imputedData$day == 'Sunday', 'weekend', 'weekday')
```

Next, we can group the data by the interval and dayType to get the mean number of steps for each interval.


```r
dayTypeData <- imputedData%>%
  group_by(interval, dayType) %>%
  summarize(aveSteps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

Last, let's plot it out with ggplot2.


```r
g <- ggplot(dayTypeData, aes(interval, aveSteps))

g + geom_line(color = "steelblue") + facet_wrap(~dayType, nrow=2, ncol=1) +
  labs(x="Time Interval", y="Number of Steps", title="Average number of steps taken on weekdays and weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

