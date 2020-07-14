---
title: "week2 assignment"
author: "ME"
date: "6/27/2020"
output: html_document
---

**Loading and preprocessing the data**

```{r setup, include=FALSE}
unzip("repdata_data_activity.zip")
data <- read.csv("activity.csv")
head(data)
dim(data)
```

**What is mean total number of steps taken per day?**

```{r}
library(ggplot2)
library(dplyr)
```

1.Calculate the total number of steps taken per day

```{r}
totaldata <- group_by(data,date) %>%
         summarise(sum(steps))
names(totaldata)[2] = "totalsteps"
ggplot(data = totaldata, aes(x = totalsteps))+
        geom_histogram(binwidth = 1000)+labs(title = "Total Steps Taken Per Day")
```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
#Mean of steps taken per day
mean(totaldata$totalsteps, na.rm = TRUE)

#Median of steps taken per day
median(totaldata$totalsteps, na.rm = TRUE)
```

**What is the average daily activity pattern?**

1.Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
averagedata <- aggregate(data$steps, by = list(data$interval), mean, na.rm = TRUE)
names(averagedata) <- c("intervals", "steps")
ggplot(data = averagedata, aes(x = intervals, y = steps))+
        geom_line()+
        xlab("5-minute interval") + ylab("average number of steps taken") 
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
maximumsteps <- averagedata[which.max(averagedata$steps), ] 
```

**Imputing missing values**

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```{r}
sum(is.na(data))
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r}
library(magrittr)
library(dplyr)
 replacedata <- function(x)
         replace(x, is.na(x), mean(x, na.rm = TRUE))
 meandata <- data %>%
         group_by(interval)%>%
         mutate(steps = replacedata(steps))
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in

```{r}
meandata
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
totaldata1 <- group_by(meandata, date) %>%
 summarise(sum(steps))
names(totaldata1)[2] <- "steps"
ggplot(data = totaldata1, aes(x = steps))+
    geom_histogram(binwidth = 1000)+labs(title = "Total Steps Taken Per Day")
#Mean of steps taken per day
meanfilleddata <- mean(totaldata1$totalsteps)

#Median of steps taken per day
medianfilleddata <- median(totaldata1$totalsteps)
```

**Are there differences in activity patterns between weekdays and weekends?**

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
meandata$date <- as.Date(meandata$date)
meandata$day <- ifelse(weekdays(meandata$date) %in% c("Saturday", "Sunday"), "weekend","weekday")
meandata$day <- as.factor(meandata$day)
```

2.Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
averagedata1 <- aggregate(meandata$steps, by = list(meandata$interval, meandata$day), sum)
names(averagedata1) <- c("intervals", "day", "steps")
ggplot(data = averagedata1, aes(x = intervals, y = steps, color = day))+
geom_line()+facet_grid(day ~.) + xlab("5-minute interval") + ylab("average number of steps taken")
```
