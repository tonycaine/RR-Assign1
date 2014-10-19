# Reproducible Research: Peer Assessment 1
Information about my environment. Have a 'minor issue' that knitr in rstudio does not use the same library path.

Well i have a few hours before due date - it could be worse...


```r
 .libPaths()
```

```
## [1] "C:/Users/tcaine/R/win-library/3.1" 
## [2] "C:/Program Files/R/R-3.1.1/library"
```
To install the required libraries ran the following.

```r
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("gridExtra")
```


```r
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
```
## Loading and preprocessing the data

First step is to read the data. This is a local copy of the data available at https://github.com/rdpeng/RepData_PeerAssessment1.

Read in data using read.delim() which is same family as read.csv(). The data frame **df.activity** is used for the raw data. Later **df.activity.padded** will be used for the data where the NA's have been replaced.



```r
dir<-'C:/_store/@stats/data-course/3.-reproducable-research-Oct2014/assignment_1'
setwd(dir)

file<- paste(dir,'/activity.csv', sep='')
df.activity<-read.delim(file, header = TRUE, sep = ",", quote = "\"", stringsAsFactors =FALSE)
```

For analysis i need to add some date and time information to this data frame

```r
# minor tweak with data to deal with date  #########################
# inital date is a chr column
colnames(df.activity)<-c('steps','datechr','interval')

#to make a datetime column
#nb. dates are strings in "yyyy-mm-dd"
df.activity$date<-ymd(df.activity$datechr)

###make a datetime column #################################

df.activity$hhmm<-sprintf("%04d", df.activity$interval)
df.activity$datetime<-paste(df.activity$datechr, df.activity$hhmm, ' ')
df.activity$datetime<-ymd_hm(df.activity$datetime)

#to get just the time during the day - will use decimal hours - in plots

df.activity$time<- hour(df.activity$datetime)*60+minute(df.activity$datetime)
#fractional hours
df.activity$hrs<- hour(df.activity$datetime)+minute(df.activity$datetime)/60

#a number of tasks require separation of weekday and weekend

#determine if day is in the weekend or is a weekday
#wday() returns the day of the week as a decimal number (01-07, Sunday is 1).
df.activity$wday<-wday(df.activity$date)

#setup the weekday/weekend factor as a column
df.activity$weekend <- as.factor(
  ifelse(df.activity$wday == 1 | df.activity$wday==7 , 'weekend', 'weekday')
)

## and summary info
str(df.activity)
```

```
## 'data.frame':	17568 obs. of  10 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ datechr : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ hhmm    : chr  "0000" "0005" "0010" "0015" ...
##  $ datetime: POSIXct, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
##  $ time    : num  0 5 10 15 20 25 30 35 40 45 ...
##  $ hrs     : num  0 0.0833 0.1667 0.25 0.3333 ...
##  $ wday    : num  2 2 2 2 2 2 2 2 2 2 ...
##  $ weekend : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
summary(df.activity)
```

```
##      steps          datechr             interval           date           
##  Min.   :  0.00   Length:17568       Min.   :   0.0   Min.   :2012-10-01  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8   1st Qu.:2012-10-16  
##  Median :  0.00   Mode  :character   Median :1177.5   Median :2012-10-31  
##  Mean   : 37.38                      Mean   :1177.5   Mean   :2012-10-31  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2   3rd Qu.:2012-11-15  
##  Max.   :806.00                      Max.   :2355.0   Max.   :2012-11-30  
##  NA's   :2304                                                             
##      hhmm              datetime                        time       
##  Length:17568       Min.   :2012-10-01 00:00:00   Min.   :   0.0  
##  Class :character   1st Qu.:2012-10-16 05:58:45   1st Qu.: 358.8  
##  Mode  :character   Median :2012-10-31 11:57:30   Median : 717.5  
##                     Mean   :2012-10-31 11:57:30   Mean   : 717.5  
##                     3rd Qu.:2012-11-15 17:56:15   3rd Qu.:1076.2  
##                     Max.   :2012-11-30 23:55:00   Max.   :1435.0  
##                                                                   
##       hrs              wday      weekend     
##  Min.   : 0.000   Min.   :1   weekday:12960  
##  1st Qu.: 5.979   1st Qu.:2   weekend: 4608  
##  Median :11.958   Median :4                  
##  Mean   :11.958   Mean   :4                  
##  3rd Qu.:17.938   3rd Qu.:6                  
##  Max.   :23.917   Max.   :7                  
## 
```



## What is mean total number of steps taken per day?





## What is the average daily activity pattern?





## Imputing missing values





## Are there differences in activity patterns between weekdays and weekends?




