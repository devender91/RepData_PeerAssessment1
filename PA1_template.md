---
title: "Course Project1"
author: "Devender Singh Saini"
date: "June 26, 2016"
output: html_document
---

# Analysis
## Loading and processing the data



```r
act <- read.csv("activity.csv",header = T)
act$date <- as.Date(act$date)
str(act)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
act1 <- act[complete.cases(act),]
```

## What is mean total number of steps taken per day?



```r
stepperday <- tapply(act1$steps,act1$date,sum)
library(ggplot2)
qplot(stepperday, xlab = "Steps per day", ylab = "Frequency",binwidth = 500)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
mean(stepperday)
```

```
## [1] 10766.19
```

```r
median(stepperday)
```

```
## [1] 10765
```



## What is the average daily activity pattern?


```r
intervalplot <- tapply(act1$steps,act1$interval, mean)
plot(x = names(intervalplot), y =intervalplot, type= "l", xlab = "Interval", ylab = "meanstepperinterval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
max(intervalplot)
```

```
## [1] 206.1698
```

```r
intervalplot[intervalplot==max(intervalplot)]
```

```
##      835 
## 206.1698
```

## Imputing missing values


```r
sum(!complete.cases(act))
```

```
## [1] 2304
```

```r
act_dum <- act
act_dum[which(is.na(act_dum$steps)),1] <-  intervalplot[as.character(act_dum[which(is.na(act_dum$steps)),3])]
sum(is.na(act_dum$steps))
```

```
## [1] 0
```

```r
new_stepperday <- tapply(act_dum$steps, act_dum$date, sum)
qplot(new_stepperday, xlab = "Steps per day", ylab = "Frequency",binwidth = 500)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
mean(new_stepperday)
```

```
## [1] 10766.19
```

```r
median(new_stepperday)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
library(dplyr)
act_dum <- mutate(act_dum,weekkind = ifelse(weekdays(act_dum$date) =="Saturday"| weekdays(act_dum$date)=="Sunday","Weekend","Weekday"))
act_dum$weekkind <- as.factor(act_dum$weekkind)
str(act_dum)               
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekkind: Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
two_graph <- act_dum %>%
  group_by(interval, weekkind) %>%
  summarise(steps = mean(steps))
plot_2 <- ggplot(two_graph, aes(x=interval, y=steps, color = weekkind)) +
  geom_line() +
  facet_wrap(~weekkind, ncol = 1, nrow=2)
print(plot_2)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
