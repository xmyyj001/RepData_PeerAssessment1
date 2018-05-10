---
title: "PA1_template.Rmd"
author: "xmyyj001"
date: "2018??5??1??"
output: html_document
---
## Introduction
This course is about processing a large amount of data about personal movement using activity monitoring devices. The data is download from [activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip/) .

## Requerements
In this course project, I have done such processing steps:
1.Code for reading in the dataset and/or processing the data
2.Histogram of the total number of steps taken each day
3.Mean and median number of steps taken each day
4.Time series plot of the average number of steps taken
5.The 5-minute interval that, on average, contains the maximum number of steps
6.Code to describe and show a strategy for imputing missing data
7.Histogram of the total number of steps taken each day after missing values are imputed
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report.

## Loading and preprocessing the data
Process the data into a format suitable for the analysis,Code for reading in the dataset and processing the data as below:


```r
if(!file.exists("activity.csv")){
  down_url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(down_url,destfile = "./repdata_data_activity.zip")
  unzip("repdata_data_activity.zip")
}

aci_data<-read.csv("./activity.csv")
aci_data<-as.data.frame(aci_data)

library(dplyr)
```
##Histogram of the total number of steps taken each day

```r
by_da<-aci_data%>%group_by(date)%>%summarize(sum_step=sum(steps,na.rm=T))
hist(by_da$sum_step)
```

![](PA1_template_files/figure-html/hist_total-1.png)<!-- -->

## calculate mean and median number of steps taken each day
Calculate the total number of steps taken per day,then calculate and report the mean and median of the total number of steps taken per day.


```r
mea_steps<-mean(by_da$sum_step,na.rm = T)
```
Mean number of steps  taken per day: 9354.2295082.



```r
med_steps<-median(by_da$sum_step,na.rm = T)
```
Median number of steps taken per day: 10395

## Time series plot of the average number of steps taken

```r
# define a function for computing interval data frame
for_cal_data<-filter(aci_data,!is.na(aci_data$steps))
mean_na_steps<-tapply(for_cal_data$steps,for_cal_data$interval,mean)
mean_na_steps<-cbind(as.data.frame(unique(for_cal_data$interval)),as.data.frame(round(mean_na_steps,0)))
names(mean_na_steps)<-c("interval","mean_steps")
with(mean_na_steps,plot(interval,mean_steps,type="l",main="average number of steps taken per day"))
```

![](PA1_template_files/figure-html/plot_TS_average-1.png)<!-- -->

## The 5-minute interval that, on average, contains the maximum number of steps
5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:

```r
max_steps<-mean_na_steps[which.max(mean_na_steps[,2]),]
max_steps<-as.numeric(max_steps[1,2])
```
The maximum number of steps is 206


## Code to describe and show a strategy for imputing missing data

Below strategy to replace NA with the mean number of the according interval. The whole data is 
instored in rev_aci_data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
table(is.na(aci_data$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```
2.Fill in all of the missing values in the dataset with the mean for that 5-minute interval.  
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
aci_data$mean <- ifelse(is.na(aci_data$steps), mean_na_steps$mean_steps, aci_data$steps)
head(aci_data)
```

```
##   steps       date interval mean
## 1    NA 2012-10-01        0    2
## 2    NA 2012-10-01        5    0
## 3    NA 2012-10-01       10    0
## 4    NA 2012-10-01       15    0
## 5    NA 2012-10-01       20    0
## 6    NA 2012-10-01       25    2
```

```r
rev_aci_data <- aci_data[ ,-1]
names(rev_aci_data) <- c("date", "interval", "steps")
```

## Histogram of the total number of steps taken each day after missing values are imputed

```r
by_da<-rev_aci_data%>%group_by(date)%>%summarize(sum_step=sum(steps,na.rm=T))
hist(by_da$sum_step)
```

![](PA1_template_files/figure-html/plot_whole_data-1.png)<!-- -->

##Panel plot comparing intervals across weekdays and weekends
After replace the missing value, below code compare the average number of steps taken per 5-minute intervals across weekdays and weekends.


```r
rev_aci_data$weekDay <- as.factor(weekdays(as.Date(rev_aci_data$date)))
rev_aci_data$isWeekDay<-"weekday"
rev_aci_data$isWeekDay[rev_aci_data$weekDay=="??????" |rev_aci_data$weekDay=="??????"]<-"weekend"
rev_aci_data$isWeekDay<-as.factor(rev_aci_data$isWeekDay)
summary_by<-aggregate(steps ~ interval + isWeekDay,data=rev_aci_data,FUN=mean)
library("lattice")
xyplot(steps~interval|isWeekDay,summary_by,type="l",layout=c(1,2),
          xlab="interval",ylab="number of steps")
```

![](PA1_template_files/figure-html/plot_weekds_vs_weekends-1.png)<!-- -->

