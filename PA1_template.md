---
title: "Reproducible Research: Peer Assessment 1"
Author: "Ravi Addanki"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data



The data in this file is read into dataset fileData1. (Assumed that the file is unzipped already)


```r
#unzip("activity.zip")
fileData1 <- read.csv("activity.csv",na.strings = "NA",header=T)
```

The column date needs to be interpreted as Date, so we use as.date function for this.

Sample data from the file is shown below.


```r
fileData1$date <- as.Date(fileData1$date,"%Y-%m-%d")
#fileData1$steps <- as.integer(fileData1$steps)
class(fileData1$date)
```

```
## [1] "Date"
```

```r
head(fileData1,3)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
```


## What is mean total number of steps taken per day?

For this first we sum the steps for each day and then find the mean /median of steps by day. We try to remove the not applicable values using na.rm=TRUE. 

- Calculate the total number of steps taken per day
- Make a histogram of total number of steps taken each day
- Calculate the mean and median of total number of steps taken per day.

Histogram below shows the distribution of total number of steps in a day. 


```r
fileData3 <- tapply(fileData1$steps,fileData1$date,sum,na.rm=T)
hist(fileData3, xlab="Total Number of steps taken per day",main = "Data distribution")
```

![](PA1_template_files/figure-html/meancalculation-1.png)<!-- -->

```r
meanValue1 <- mean(fileData3,na.rm=T)
medianValue1 <- median(fileData3,na.rm=T)
```

### Conclusion
The mean total number of steps taken per day is 9354.2295082 and median is 10395

## What is the average daily activity pattern?

1. Make a time series plot.

Here we aggregate data based on interval. The time interval is given as HHMM format, so this can be divided by 100 to seperate hours and minutes by ".".  But sometimes HH could be missing. So we added 100 to make sure HH are read correctly and 0.00001 to make sure minutes are read correctly. Plot type "l" is line graph.


```r
fileData4 <- tapply(fileData1$steps,fileData1$interval,mean,na.rm=T)
timeSeries1 <- data.frame(time = names(fileData4), steps = fileData4)
timeSeries1$time <- as.POSIXlt(strptime(as.character(as.numeric(as.character(timeSeries1$time) ) /100 + 100.00001),"1%H.%M"))
plot(timeSeries1,type = "l")
```

![](PA1_template_files/figure-html/timeseries-1.png)<!-- -->


2. Which 5-minute interval contains the maximum number of steps?

The time intervals that correspond to max mumber of steps are 


```r
row.names(timeSeries1[timeSeries1$steps == max(timeSeries1$steps),])
```

```
## [1] "835"
```



## Imputing missing values

### Total number of missing values in dataset

Total number of missing values in the dataset is 2304 out of 17568 records.

### Strategy for filling in all of the missing values in the dataset

The best strategy for filling the missing values is to take the median for the 5 minute interval. Taking a mean or median of the day may not be representative as there is less likelyhood of the person's activity being same thought the day. For example, the person may be sleeping at midnight and most probably walking in work location between 8 AM to 9AM. Taking a median gives better result than mean as this value is less likely to be affected by the outsiders. For example, if a person has participated in a race on a particular day, this could skew the value choosen for missing value. For this we need to calculate the median values for all the time intervals.


```r
medianData <- tapply(fileData1$steps,fileData1$interval,median,na.rm=T)
timeSeries2 <- data.frame(interval = as.integer(names(medianData)), medianSteps = medianData)
```

### Dataset with missing values filled in

The new dataset created is fileData5. For this we have to join original data (fileData1) with the table that contains median values by interval (timeSeries2). Then we have to replace the steps from the median value. Finally we project the fields that are required.


```r
mergeData1 <- merge(fileData1,timeSeries2)
mergeData1$steps[is.na(mergeData1$steps)] <- mergeData1$medianSteps[is.na(mergeData1$steps)]
fileData5<- mergeData1[order(mergeData1$date,mergeData1$interval),c(colnames(fileData1))]
```

Sample data of original data set (records 2000 to 2020) is shown below


```r
fileData1[2000:2020,]
```

```
##      steps       date interval
## 2000     0 2012-10-07     2235
## 2001     0 2012-10-07     2240
## 2002     0 2012-10-07     2245
## 2003     0 2012-10-07     2250
## 2004     0 2012-10-07     2255
## 2005     0 2012-10-07     2300
## 2006     0 2012-10-07     2305
## 2007     0 2012-10-07     2310
## 2008     0 2012-10-07     2315
## 2009     0 2012-10-07     2320
## 2010     0 2012-10-07     2325
## 2011     0 2012-10-07     2330
## 2012     0 2012-10-07     2335
## 2013     0 2012-10-07     2340
## 2014     0 2012-10-07     2345
## 2015     0 2012-10-07     2350
## 2016     0 2012-10-07     2355
## 2017    NA 2012-10-08        0
## 2018    NA 2012-10-08        5
## 2019    NA 2012-10-08       10
## 2020    NA 2012-10-08       15
```

Sample data of new dataset is shown below



```r
fileData5[2000:2020,]
```

```
##       steps       date interval
## 16579     0 2012-10-07     2235
## 16633     0 2012-10-07     2240
## 16687     0 2012-10-07     2245
## 16768     0 2012-10-07     2250
## 16831     0 2012-10-07     2255
## 16880     0 2012-10-07     2300
## 16945     0 2012-10-07     2305
## 17004     0 2012-10-07     2310
## 17071     0 2012-10-07     2315
## 17128     0 2012-10-07     2320
## 17169     0 2012-10-07     2325
## 17237     0 2012-10-07     2330
## 17278     0 2012-10-07     2335
## 17332     0 2012-10-07     2340
## 17428     0 2012-10-07     2345
## 17484     0 2012-10-07     2350
## 17564     0 2012-10-07     2355
## 58        0 2012-10-08        0
## 89        0 2012-10-08        5
## 159       0 2012-10-08       10
## 199       0 2012-10-08       15
```

### Histogram of total steps in new dataset and the new mean/median


Histogram of total steps in a day from new dataset 


```r
fileData6 <- tapply(fileData5$steps,fileData5$date,sum)
hist(fileData6, xlab="Total Number of steps taken per day",main = "New Data distribution")
```

![](PA1_template_files/figure-html/newDailyset-1.png)<!-- -->

```r
meanValue2 <- mean(fileData6)
medianValue2 <- median(fileData6)
```


The new mean total number of steps taken per day is 9503.8688525 and new median is 10395.

Yes, imputing missing values had an impact on mean of total number of daily steps, but median is not affected. The values are slightly higher than the original. These values may change if we use different strategy becuase the median values are different from the mean values.



## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable with two values “weekday” and “weekend”  

Here we use weekdays function to find the week of the day and then classify it.


```r
fileData5$day <- weekdays(as.Date(as.character(fileData5$date),"%Y-%m-%d"))
fileData5$day[fileData5$day == "Saturday" |fileData5$day == "Sunday" ] <- 'weekend'
fileData5$day[fileData5$day != "weekend"  ] <- 'weekday'
fileData5$day <- factor(fileData5$day)
```

### Make a plot

Ploting the resulting dataset shows that during morning hours more steps are taken on weekdays than weekends (except around 11 AM), and during afternoon period more steps are taken on weekends than weekdays (except at around 6 PM).


```r
fileData8 <- tapply(fileData5$steps,list(fileData1$interval,fileData5$day),mean)
timeSeries4 <- as.data.frame(fileData8)
timeSeries4$time <-  as.POSIXlt(strptime(as.character(as.numeric(as.character(rownames(timeSeries4)) ) /100 + 100.00001),"1%H.%M"))
par(fin= c(5,5),mar=c(2,2,1,1),mfrow=c(2,1),oma=c(3,3,0,0))
plot(timeSeries4$time,timeSeries4$weekend,type="l",col="blue",xlab="",ylab="",main="weekend")
plot(timeSeries4$time,timeSeries4$weekday,type="l",col="blue",xlab="",ylab="",main="weekday")
mtext(text="Number of Steps",side=2,line=0,outer=TRUE)
mtext(text="Interval",side=1,line=0,outer=TRUE)
```

![](PA1_template_files/figure-html/timePlotByDaytype-1.png)<!-- -->
