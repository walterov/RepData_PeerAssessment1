# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(lattice)

actData <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

#### Calculate the total number of steps taken per day

```r
stepDailyTotal = tapply(X = actData$steps, INDEX = actData$date, FUN = sum)
```

#### Make a histogram of the total number of steps taken each day

```r
hist(stepDailyTotal)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

#### Calculate and report the mean and median of the total number of steps taken per day

```r
mean(stepDailyTotal, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(stepDailyTotal, na.rm = T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
actData$factorInterval = as.factor(actData$interval)
stepIntervalAverage = tapply(X = actData$steps, INDEX = actData$factorInterval, FUN = mean, na.rm = T)
plot(y = stepIntervalAverage, x = names(stepIntervalAverage), 
     ylab = "Average Steps per Interval", xlab="Intervals", type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(stepIntervalAverage[stepIntervalAverage == max(stepIntervalAverage)])
```

```
## [1] "835"
```

## Imputing missing values

#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(actData$steps))
```

```
## [1] 2304
```

####Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newActData = actData
for(i in 1:length(newActData$steps)) {
        if(is.na(newActData$steps[i])) {
                newActData$steps[i] = stepIntervalAverage[as.character(newActData$interval[i])]
        }        
} 
```

#### Make a histogram of the total number of steps taken each day 

```r
newStepDailyTotal = tapply(X = newActData$steps, INDEX = newActData$date, FUN = sum)
hist(newStepDailyTotal)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

#### Calculate and report the mean and median total number of steps taken per day

```r
mean(newStepDailyTotal)
```

```
## [1] 10766.19
```

```r
median(newStepDailyTotal)
```

```
## [1] 10766.19
```

#### Do these values differ from the estimates from the first part of the assignment?
Answer: Yes

#### What is the impact of imputing missing data on the estimates of the total daily number of steps?
Answer: The histogram shows a slight larger number of instances in particular in the 10000 15000 range, this is explained by the fact that more values were added to the data set using the average of the particular intervals.The mean value remained intact, the mediam value changed to the same value of the mean.

## Are there differences in activity patterns between weekdays and weekends?

#### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
newActData$date1 = as.Date(x = newActData$date, format = "%Y-%m-%d")
newActData$daytype = as.factor(ifelse ((weekdays(newActData$date1) %in% c("Saturday","Sunday")), "weekend", "weekday"))
```

#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

Create a dataset containing the average of steps for the plot

```r
excludeCol <- names(newActData) %in% c("date", "factorInterval", "date1", "daytype") 
newStepIntervalAverage = aggregate(newActData[!excludeCol], 
                                   list(Interval = newActData$factorInterval, 
                                   DayType = newActData$daytype), 
                                   mean)
```

Create the plot with the average of steps as requested

```r
xyplot(newStepIntervalAverage$steps ~ newStepIntervalAverage$interval | newStepIntervalAverage$DayType,
       layout = c(1, 2), 
       type = "l", 
       scales = list(x = list(at=c(0,500,1000,1500,2000), limits=c(-45, 2400))),
       xlab = "Interval", 
       ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
