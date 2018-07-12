---
title: "P1_template.rmd"
author: "littlelight"
date: "12 de Julho de 2018"
output: 
  html_document: 
    keep_md: true
---

## 1. Code for reading in the dataset and/or processing the data


```r
setwd("C:/Users/carla/OneDrive/Coursera/Reproducible Research/Week2/Project1")
unzip("data.zip")
dir()
```

```
## [1] "activity.csv" "data.zip"
```

```r
data <- read.csv("activity.csv", header = TRUE, sep = ",")
```

## 2. Histogram of the total number of steps taken each day


```r
dataSteps_day <- tapply(data$steps,data$date,sum) # average of 5 min interval
```


```r
hist(dataSteps_day, col="red", main="Total steps per day")
```

![](PA1_template_files/figure-html/histogramstepsday-1.png)<!-- -->

## 3. Mean and median number of steps taken each day


```r
mean(dataSteps_day, na.rm = TRUE) 
```

```
## [1] 10766.19
```

```r
median(dataSteps_day, na.rm = TRUE)
```

```
## [1] 10765
```

## 4. Time series plot of the average number of steps taken 


```r
avr_steps_inter_5<- aggregate(steps ~ interval, data = data, 
                                   mean, na.rm=TRUE)
                                   
plot(avr_steps_inter_5, type="l", 
     main="Time series of the average number of steps 5 min interval")
```

![](PA1_template_files/figure-html/plotavrsteps5-1.png)<!-- -->

## 5. The 5-minute interval that, on average, contains the maximum number of steps



```r
maxsteps_5<- max(tapply(avr_steps_inter_5$steps, avr_steps_inter_5$interval, max))

maxinterval<- avr_steps_inter_5$interval[which.max(avr_steps_inter_5$steps)]

avr_steps_inter_5[avr_steps_inter_5$steps==max(avr_steps_inter_5$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## 6. Code to describe and show a strategy for imputing missing data




```r
dataImputed <- data

dataImputed$steps <- impute(data$steps, fun=mean)

dataStepsImputed_day <- tapply(dataImputed$steps,dataImputed$date,sum) # average of 5 min interval
```

## 7. Histogram of the total number of steps taken each day after missing values are imputed


```r
hist(dataStepsImputed_day, col="red", main="Total steps per day")
```

![](PA1_template_files/figure-html/histogramstepsdayimputed-1.png)<!-- -->

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
dataImputed$dateType <-  ifelse(as.POSIXlt(dataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')

averagedataImputed <- aggregate(steps ~ interval + dateType, data=dataImputed, mean)
```


```r
ggplot(averagedataImputed, aes(interval, steps)) +
        geom_line() +
        facet_grid(dateType ~ .) +
        xlab("5-minute interval") +
        ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/weekdaysweekends-1.png)<!-- -->

## 9. All of the R code can reproduce the results (numbers, plots, etc.) in the report
