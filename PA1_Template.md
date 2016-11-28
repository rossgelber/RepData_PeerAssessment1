# Reproducible Research Course Project 1

## Load Necessary Packages

```r
library(dplyr)
library(ggplot2)
library(knitr)
library(markdown)
library(lattice)
```

## Loading the Data

```r
data <- read.csv("activity.csv")
```

## Calculate Mean Number of Steps per Day

```r
steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(steps, binwidth=500, xlab = "Steps per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
mean(steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(steps, na.rm=TRUE)
```

```
## [1] 10395
```

## Find Average Daily Activity Pattern

```r
average <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
ggplot(data=average, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("Time of Day") +
    ylab("Steps Taken")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

## Impute Missing Values

```r
df_impute <- df
ndx <- is.na(df_impute$steps)
```

```
## Error in df_impute$steps: object of type 'closure' is not subsettable
```

```r
int_avg <- tapply(df_ign$steps, df_ign$interval, mean, na.rm=TRUE, simplify=T)
```

```
## Error in tapply(df_ign$steps, df_ign$interval, mean, na.rm = TRUE, simplify = T): object 'df_ign' not found
```

```r
df_impute$steps[ndx] <- int_avg[as.character(df_impute$interval[ndx])]
```

```
## Error in eval(expr, envir, enclos): object 'int_avg' not found
```

```r
idata <- data
nodata <- is.na(idata$steps)
sum(nodata)
```

```
## [1] 2304
```

```r
averages <- tapply(idata$steps, idata$interval, mean, na.rm=TRUE)
idata$steps[nodata] <- averages[as.character(idata$interval[nodata])]

repl <- function(steps, interval) 
{
  replaced <- NA
  if (!is.na(steps))
    replaced <- c(steps)
  else
      replaced <- (averages[averages$interval==interval, "steps"])
  return(replaced)
}
newdata <- data
newdata$steps <- mapply(repl, newdata$steps, newdata$interval)
```

```
## Error in averages$interval: $ operator is invalid for atomic vectors
```

```r
stepss <- tapply(newdata$steps, newdata$date, FUN=sum)
qplot(stepss, binwidth=500, xlab="Steps per Day")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
mean(stepss)
```

```
## [1] NA
```

```r
median(stepss)
```

```
## <NA> 
##   NA
```

The mean and median are both higher after removing the NA values from the dataset.
These values were being replaced with zeros, which artificially reduced the calculated mean and median values. 
While we still do not know what these values, should be, we replaced these zeroes in the calculations, with the averages of the non-NA values, which is what caused these calculations to increase the second time around.


```r
partofweek <- function(date) 
  {
    days <- weekdays(date)
    ifelse (days == "Saturday" | days == "Sunday", "weekend", "weekday")
  }

doww <- sapply(newdata$date, partofweek)
```

```
## Error in UseMethod("weekdays"): no applicable method for 'weekdays' applied to an object of class "factor"
```

```r
newdata$dow <- as.factor(doww)
```

```
## Error in is.factor(x): object 'doww' not found
```

```r
idata <- aggregate(steps ~ dow+interval, data=newdata, FUN=mean)
```

```
## Error in eval(expr, envir, enclos): object 'dow' not found
```

```r
xyplot(steps ~ interval | factor(dow),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Steps",
       type="l",
       lty=1,
       data=newdata)
```

```
## Error in factor(dow): object 'dow' not found
```
