Peer Assessment 1
=================


##Introduction##
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks.

##Data##
Data used ([here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)) is from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day

  -The variables included in this dataset are:  
      1.steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
      2.date: The date on which the measurement was taken in YYYY-MM-DD format
      3.interval: Identifier for the 5-minute interval in which measurement was taken

There are a total of 17,568 observations in this dataset

##Loading and preprocessing the data##

```r
data<-read.csv("activity.csv")
```
##Mean total number of steps taken per day##

```r
a<-mean(data$steps,na.rm = T)
library(plyr)
steps_day<-ddply(data,.(date),summarize,Mean=sum(steps))
barplot(steps_day$Mean,xlab = "Date",ylab = "steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
b<-mean(steps_day$Mean,na.rm = T)
c<-median(steps_day$Mean,na.rm = T)
```

Total steps taken per day are 37.3825996.  
Mean of total steps taken per day are 1.0766189 &times; 10<sup>4</sup>.  
Median of total steps taken per day are 10765.  

##Average daily activity pattern##

```r
daily_avg<-ddply(data[complete.cases(data),],.(interval),summarise,Mean=mean(steps))
plot(daily_avg,type='l')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
daily_avg$interval[which.max(daily_avg$Mean)]
```

```
## [1] 835
```

##Imputing missing values##

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

```r
imp_data=data
for(i in 1:dim(data)[1]){
  if(is.na(data[i,1]))
    imp_data[i,1]=daily_avg[daily_avg$interval==data[i,3],]$Mean
}
mean_steps<-ddply(imp_data,.(date),summarise,Mean=sum(steps))
barplot(mean_steps$Mean,xlab = "Date",ylab = "steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
mean(mean_steps$Mean)
```

```
## [1] 10766.19
```

```r
median(mean_steps$Mean)
```

```
## [1] 10766.19
```
The plot is similar except that there are no the missing values.


##Differences in activity patterns between weekdays and weekends##

```r
weekends<-c("Saturday","Sunday")
imp_data<-mutate(imp_data,newcol=ifelse(weekdays(as.Date(date))%in% c("Saturday","Sunday"),"weekend","weekday"))
imp_data$newcol<-as.factor(imp_data$newcol)
plt<-ddply(imp_data,.(newcol,interval),summarise,Mean=mean(steps))
#library(lattice)
#xyplot(Mean~interval|newcol,data=p,panel=function(x,y,...){
#  panel.xyplot(x,y,type='l',...)},layout=c(1,2))
#or
library(ggplot2)
qplot(interval,Mean,data=plt,geom = c("line"),facets=newcol~.)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

During weekdays steps start early and the steps reach peak in working hours. This is not so during weekends.  
