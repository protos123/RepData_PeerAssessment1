# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Reads the data and saves it to a table called data.



```r
setwd("C:/RepData_PeerAssessment1") 
unzip("activity.zip")  
activity<-read.csv("activity.csv", header=TRUE) 
```



## What is mean total number of steps taken per day?

```r
stepsday<-rowsum(activity$steps,activity$date,na.rm = T)
hist(stepsday, xlab = "Steps by day",main = "Histogram of steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
meanst<-round(mean(stepsday))
medianst<-round(median(stepsday))
```



## What is the average daily activity pattern?


```r
stepsinterval<-tapply(activity$steps,activity$interval,mean, na.rm=TRUE)
plot(unique(activity$interval),stepsinterval,type = "l", xlab = "Intervals",
     ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 



```r
stepsinterval[which.max(stepsinterval)]
```

```
##      835 
## 206.1698
```


## Imputing missing values


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


```r
stepsint<-stepsinterval
intervs<-unique(activity$interval)

setinterval<-data.frame(intervs,stepsint)
activity2<-activity

for (i in 1:length(activity2$steps))
{
  if(is.na(activity2$steps[i]))
  {interv<-activity2$interval[i]
   activity2$steps[i]=setinterval$stepsint[which(setinterval$intervs==interv)]
  }
}
```



```r
stepsday2<-rowsum(activity2$steps,activity2$date)
hist(stepsday2, xlab = "Steps by day",main = "Histogram of steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
meanst2<-round(mean(stepsday))
medianst2<-round(median(stepsday))
```


## Are there differences in activity patterns between weekdays and weekends?
