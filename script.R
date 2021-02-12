#### Install packages I want to use
install.packages("dplyr")
library(dplyr)

#### Download data set and load it up
download.file(
              url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
              destfile="movementdata.zip"
              )

unzip(zipfile="activity.zip")

data <- read.csv(file="activity.csv")


#### Make histogram of total number of steps taken each day
## group data by date and sum number of steps
# remove NA rows for days with no data
dailysteps <- data %>%
  group_by(date) %>%
  summarize(total_steps=sum(steps)) %>%
  filter(!is.na(total_steps))

hist(dailysteps$total_steps, 
     main="Steps walked daily in October-November 2012",
     xlab="Number of Steps",
     col="lightblue")


#### Mean and median steps taken each day
# mean
mean(dailysteps$total_steps)
meanDailySteps <- mean(dailysteps$total_steps)

# median
median(dailysteps$total_steps)
medianDailySteps <- median(dailysteps$total_steps)


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


#### 5 min interval that on average contains max number of steps
## group data by interval and get the mean

intervalData <- data %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm=TRUE))

mostStepInterval <- intervalData[which.max(intervalData$mean_steps),]
mostStepInterval


#### how to impute missing data
## I will take mean values from intervals and use them to fill in NAs for each interval
# Take interval data from previous part
intervalData

# Find rows with NA values
NAs <- which(is.na(data$steps))
naRows <- data[NAs,]

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

#### Mean and median steps taken each day
# mean
mean(imputeddailysteps$total_steps)

# median
median(imputeddailysteps$total_steps)



###### Subset the data up into weekday and weekend

install.packages("ggplot2")
library(ggplot2)

# convert date to POSIX class
imputedData$date <- strptime(imputedData$date, format = "%Y-%m-%d")

imputedData$day <- weekdays(imputedData$date)

imputedData$dayType <- ifelse(imputedData$day == 'Saturday' | imputedData$day == 'Sunday', 'weekend', 'weekday')

dayTypeData <- imputedData%>%
  group_by(interval, dayType) %>%
  summarize(aveSteps = mean(steps))

g <- ggplot(dayTypeData, aes(interval, aveSteps))

g + geom_line(color = "steelblue") + facet_wrap(~dayType, nrow=2, ncol=1) +
  labs(x="Time Interval", y="Number of Steps", title="Average number of steps taken on weekdays and weekends")
