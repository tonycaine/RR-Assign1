# Reproducible Research: Peer Assessment 1
Information about my environment. Have a 'minor issue' that knitr in rstudio does not use the same library path and so will not run the Rmd script.

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
### Loading and preprocessing the data

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
### Imputing missing values.

the NA values are replaced by the following algorith.

1. calculating the mean of the interval across the 2 months taking into account the weekend as a factor. 
2. these are joined into the df.activity dataframe so that available as a 'mean_steps'.
3. where the row has a steps of NA using the mean_steps is used to replace the NA

This analysis, using dplyr, is done here, early so that the following plots can compare the raw and padded data.

An insight had during experimentation was that this replacment does shift the histogram as there are less zero activity days but does not shift the mean activity - as just added back the mean activity.


```r
############################################################
### clean up data by replacing the steps=NA with the mean number of steps taken in that interval.
### as the weekend and weekday will influence this mean
### use mean of weekday interval and mean of weekend interval

### firstly get the interval_mean_steps_w where there are means for 
# weekdays and weekend intervals

interval_steps_ww <- 
  df.activity  %>%
  group_by( weekend, interval)  %>%
  summarise(
    mean_steps = mean(steps, na.rm = TRUE)
  )

# add on a joining column 'interval_weekend' so that dplyr join() can be used (see below)
interval_steps_ww <- 
  interval_steps_ww  %>%
  mutate ( interval_weekend = paste (weekend,interval))

# add that same column to the df.activity dataframe 
 
df.activity<-
df.activity %>%
  mutate ( interval_weekend = paste (weekend,interval))

# join the dataframes back to df.activity so have mean_steps available if steps is NA
# this bvings the intervals mean in so that it can be used to replace the NA where appropriate
df.activity<-left_join(df.activity, interval_steps_ww, by=c("interval_weekend")) 

#clean up from the join 
df.activity <-
  df.activity %>%
  select( steps, datechr, interval=interval.x, date, hhmm, datetime, time, hrs, wday, weekend=weekend.x, mean_steps)
  
#create another dataframe replacing all the NA's with the interval mean of the weekday or weekend 
# df.activity.padded has the NA replaced otherwise the same as df.activity

df.activity.padded<- 
  df.activity %>%
  mutate(
    steps =
      ifelse(
        is.na( steps), 
        mean_steps, #an alternative that does make a visible difference is to set the NA to zero 
        steps
      )
  )
```


```r
## and summary info
str(df.activity)
```

```
## 'data.frame':	17568 obs. of  11 variables:
##  $ steps     : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ datechr   : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval  : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ date      : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ hhmm      : chr  "0000" "0005" "0010" "0015" ...
##  $ datetime  : POSIXct, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
##  $ time      : num  0 5 10 15 20 25 30 35 40 45 ...
##  $ hrs       : num  0 0.0833 0.1667 0.25 0.3333 ...
##  $ wday      : num  2 2 2 2 2 2 2 2 2 2 ...
##  $ weekend   : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  $ mean_steps: num  2.333 0.462 0.179 0.205 0.103 ...
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
##       hrs              wday      weekend        mean_steps     
##  Min.   : 0.000   Min.   :1   weekday:12960   Min.   :  0.000  
##  1st Qu.: 5.979   1st Qu.:2   weekend: 4608   1st Qu.:  2.103  
##  Median :11.958   Median :4                   Median : 24.769  
##  Mean   :11.958   Mean   :4                   Mean   : 37.368  
##  3rd Qu.:17.938   3rd Qu.:6                   3rd Qu.: 57.333  
##  Max.   :23.917   Max.   :7                   Max.   :234.103  
## 
```

```r
str(df.activity.padded)
```

```
## 'data.frame':	17568 obs. of  11 variables:
##  $ steps     : num  2.333 0.462 0.179 0.205 0.103 ...
##  $ datechr   : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval  : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ date      : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ hhmm      : chr  "0000" "0005" "0010" "0015" ...
##  $ datetime  : POSIXct, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
##  $ time      : num  0 5 10 15 20 25 30 35 40 45 ...
##  $ hrs       : num  0 0.0833 0.1667 0.25 0.3333 ...
##  $ wday      : num  2 2 2 2 2 2 2 2 2 2 ...
##  $ weekend   : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  $ mean_steps: num  2.333 0.462 0.179 0.205 0.103 ...
```

```r
summary(df.activity.padded)
```

```
##      steps          datechr             interval           date           
##  Min.   :  0.00   Length:17568       Min.   :   0.0   Min.   :2012-10-01  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8   1st Qu.:2012-10-16  
##  Median :  0.00   Mode  :character   Median :1177.5   Median :2012-10-31  
##  Mean   : 37.37                      Mean   :1177.5   Mean   :2012-10-31  
##  3rd Qu.: 24.00                      3rd Qu.:1766.2   3rd Qu.:2012-11-15  
##  Max.   :806.00                      Max.   :2355.0   Max.   :2012-11-30  
##      hhmm              datetime                        time       
##  Length:17568       Min.   :2012-10-01 00:00:00   Min.   :   0.0  
##  Class :character   1st Qu.:2012-10-16 05:58:45   1st Qu.: 358.8  
##  Mode  :character   Median :2012-10-31 11:57:30   Median : 717.5  
##                     Mean   :2012-10-31 11:57:30   Mean   : 717.5  
##                     3rd Qu.:2012-11-15 17:56:15   3rd Qu.:1076.2  
##                     Max.   :2012-11-30 23:55:00   Max.   :1435.0  
##       hrs              wday      weekend        mean_steps     
##  Min.   : 0.000   Min.   :1   weekday:12960   Min.   :  0.000  
##  1st Qu.: 5.979   1st Qu.:2   weekend: 4608   1st Qu.:  2.103  
##  Median :11.958   Median :4                   Median : 24.769  
##  Mean   :11.958   Mean   :4                   Mean   : 37.368  
##  3rd Qu.:17.938   3rd Qu.:6                   3rd Qu.: 57.333  
##  Max.   :23.917   Max.   :7                   Max.   :234.103
```

```

### What is mean total number of steps taken per day?

Use a function histo_mean_median() to generate a list containing the plot and the mean and median.
It will later be called for both the raw and padded data.


```r
# require a histogram of the steps per day and mean and median
# need this twice for different data so do it in a function

histo_mean_median <- function (df, title) {
  # get the steps per day
  df.perday <- 
    df %>%
    group_by( date)  %>%
    summarise(
      steps_per_day = sum(steps, na.rm = TRUE)
    )
  #plot it as a histogram - force scales to be same for comparison
  p<-ggplot(df.perday, aes(x=steps_per_day)) +
          geom_histogram(binwidth = 250) + 
          labs( title=title ,
                x = 'steps per day',
                y = 'number of days')+
          xlim(0,25000) +
          scale_y_continuous(breaks=0:10)
  # what where the mean and medium of step per day
  mean_median <- df.perday  %>%
    summarise(
      mean_per_day   = mean(steps_per_day, na.rm = TRUE),
      medium_per_day = median(steps_per_day, na.rm = TRUE)
    )
  #return the plot and the mean and median
  ret <- list("plot" = p, "mean_median" = mean_median)
  ret
}
```

Generate the plots.
Do I need to save these manually??


```r
####################################################
#Compare the raw and the padded data

raw <-histo_mean_median ( df.activity, 'Histogram of steps taken per day for the 2 months')
pad <-histo_mean_median ( df.activity.padded, 'Histogram  of padded data - the NAs that were biasing the zeros - shifted')

# histograms on a panel
grid.arrange(raw$plot, pad$plot, ncol=1, nrow=2)
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Mean per day increases when the NA's removed.

```r
# the means and median of the raw data
raw$mean_median
```

```
## Source: local data frame [1 x 2]
## 
##   mean_per_day medium_per_day
## 1      9354.23          10395
```

```r
# the means and median of the padded data
pad$mean_median
```

```
## Source: local data frame [1 x 2]
## 
##   mean_per_day medium_per_day
## 1     10762.05          10571
```


### What is the average daily activity pattern?


```r
############################################################
# plot activity for each 5 min interval for the month

# Firstly get the average of the intervals
interval_steps <- 
  df.activity  %>%
  group_by( hrs)  %>%
  summarise(
    interval = first(interval),
    mean_steps = mean(steps, na.rm = TRUE)
  )

#plot the mean steps - use the hour as the x/base because to have treat as timeseries
# if plot using the interval get gaps for times between end of hour and start of next. eg. 255 to 300

ggplot(interval_steps , aes(x=hrs, y=mean_steps)) +
  geom_line( colour='blue') + 
  labs( title='Mean Steps Vs Time for each 5 min Interval' ,
        x = 'time (as hour)',
        y = 'steps') + 
  theme(legend.position="none") +
  scale_x_continuous(breaks=0:24)
```

![](./PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
The maximum mean number of step for an interval was about 206 steps at  8:35.

```r
############################################################
#determine the interval with the max number of average steps

interval_steps %>%
  select( interval, mean_steps) %>% 
  filter( mean_steps == max(mean_steps))
```

```
## Source: local data frame [1 x 2]
## 
##   interval mean_steps
## 1      835   206.1698
```

```r
############################################################
```

### Are there differences in activity patterns between weekdays and weekends?
Another function was used to generate comparison plots of the raw and the padded data. Each plot has the mean steps for that interval taking into account the weekday or weekend nature of the data. 


```r
############################################################
# Plot the weekday and the weekend interval averages
# Doing this for both the raw and the 'padded' data - so use a function that returns the plot

plot_weekday_weekend <- function (df, title) {
  interval_steps <- 
    df %>%
    group_by( hrs, weekend)  %>%
    summarise(
      interval = first(interval),
      mean_steps = mean(steps, na.rm = TRUE)
    )
  p<-ggplot(interval_steps , aes(x=hrs, y=mean_steps, group=weekend, colour=weekend)) +
    geom_line() + 
    labs( title=title,
          x = 'interval (as hour)',
          y = 'steps') + 
    scale_x_continuous(breaks=0:24)+
    scale_y_continuous(breaks=seq(from=0, to=250, by=25))
  ret <- list("plot" = p)
  ret
}
```

There is a clear difference between the weekdays and the weekend for this individual.


```r
raw<- plot_weekday_weekend ( df.activity, 'Mean Steps Vs Time for each 5 min Interval grouped on Weekdays Vs Weekends')
pad<- plot_weekday_weekend ( df.activity.padded, 'Padded data - essentally the same because padded with the mean')

# Plot the weekday and the weekend interval averages on a panel
# this arranegment allows comparison of the weekend/weekday and the effect of the padding

#display results
grid.arrange(raw$plot, pad$plot, ncol=1, nrow=2)
```

![](./PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

