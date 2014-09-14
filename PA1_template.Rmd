---
output: html_document
---
# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

Load the data
```{r echo = TRUE}
setwd("/Users/meteozguz/GitHub/RepData_PeerAssessment1")
rawData <- read.csv(file.path(getwd(),"activity.csv"),sep=",")
```

Ignoring the missing values in the dataset...    

<b>Make a histogram of the total number of steps taken each day (altered xlabeling code taken from http://stackoverflow.com/questions/10286473/rotating-x-axis-labels-in-r-for-barplot)</b>
```{r echo = TRUE}
stepsEachDay <- aggregate(rawData$steps ~ rawData$date, rawData, sum)
names(stepsEachDay) <- c("date","steps")
lablist <- as.vector(stepsEachDay$date)
```
Adds room for the rotated labels
```{r echo = TRUE}
par(mar = c(7, 4, 2, 2) + 0.2) 
```
Line which does the trick (together with barplot "space = 1" parameter)
```{r echo = TRUE}
end_point = 0.5 + nrow(stepsEachDay) + nrow(stepsEachDay)-1

barplot(stepsEachDay$steps, ylab="Total Number of Steps",
        ylim = c(0,5+max(stepsEachDay$steps)),
        xlab ="Days", 
        main="Histogram of the Total Number of Steps taken each Day",
        col="red", space=1)

text(seq(1.5,end_point,by=2), par("usr")[3]+0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels = paste(stepsEachDay$date), cex=0.65) # Rotates 60 degrees, srt=60
```

<b>Calculate and report the mean and median total number of steps taken per day</b>
```{r echo = TRUE}
mean <- sum(stepsEachDay$steps)/length(stepsEachDay$steps)
```
<b>`r mean`</b> is the mean total number of steps taken per day 
```{r echo = TRUE}
median <- median(stepsEachDay$steps)
```
<b>`r median`</b> is the median total number of steps taken per day


<b>What is the average daily activity pattern?</b>
```{r echo = TRUE}
plot(as.numeric(as.character(stepsEachDay$steps)) ~ as.POSIXct(paste(as.Date(stepsEachDay$date))), 
     type="l", xlab="Days", ylab="Number of Steps Taken per Day",
     main ="Average Daily Activity Pattern")
```


<b>Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)</b>

```{r echo = TRUE}
stepsEachInterval <- aggregate(rawData$steps ~ rawData$interval, rawData, sum)
names(stepsEachInterval) <- c("interval","steps")

averages <- stepsEachInterval$steps / length(stepsEachDay$steps)

plot(as.numeric(as.character(averages)) ~ as.numeric(as.character(stepsEachInterval$interval)) , 
     type="l", xlab="Time Intervals", ylab="Mean of Steps Taken per Interval",
     main ="Time series plot of the 5-minute interval \nand the average number of steps taken, averaged across all days")
```     
     

<b>Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?</b>
```{r echo = TRUE}
theInterval <- stepsEachInterval$interval[which(averages==max(averages))]
```
<b>`r theInterval`</b>. 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r echo=TRUE}
missing <- length(which(is.na(rawData$steps)==TRUE))
```
<b>`r missing`</b> is the total number of missing values in the dataset.


# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r echo=TRUE}
meanPerDay <- stepsEachDay$steps/length(unique(rawData$interval))
stepsEachDay<-cbind(stepsEachDay,meanPerDay)
```
Chosing the mean for that day...

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r echo=TRUE}
cookedData <- rawData
for(i in 1:nrow(cookedData )) {
    if(is.na(cookedData$steps[i])==TRUE){
      tmp<- stepsEachDay$meanPerDay[which(stepsEachDay$date==cookedData$date[i])]
      if(length(tmp)!=0){
        cookedData$steps[i]<-tmp
      }
    }
    # do stuff with row
}
```
<b>Not working probably because of aggregate function</b>


# 4. Make a histogram of the total number of steps taken each day and...
```{r echo=TRUE}
stepsEachDay <- aggregate(cookedData$steps ~ cookedData$date, cookedData, sum)
names(stepsEachDay) <- c("date","steps")
lablist <- as.vector(stepsEachDay$date)
```
Adds room for the rotated labels
```{r echo = TRUE}
par(mar = c(7, 4, 2, 2) + 0.2) 
```
Line which does the trick (together with barplot "space = 1" parameter)
```{r echo = TRUE}
end_point = 0.5 + nrow(stepsEachDay) + nrow(stepsEachDay)-1

barplot(stepsEachDay$steps, ylab="Total Number of Steps",
        ylim = c(0,5+max(stepsEachDay$steps)),
        xlab ="Days", 
        main="Histogram of the Total Number of Steps taken each Day",
        col="red", space=1)

text(seq(1.5,end_point,by=2), par("usr")[3]+0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels = paste(stepsEachDay$date), cex=0.65) # Rotates 60 degrees, srt=60
```


<b>Calculate and report the mean and median total number of steps taken per day</b>
```{r echo = TRUE}
mean <- sum(stepsEachDay$steps)/length(stepsEachDay$steps)
```
<b>`r mean`</b> is the mean total number of steps taken per day 
```{r echo = TRUE}
median <- median(stepsEachDay$steps)
```
<b>`r median`</b> is the median total number of steps taken per day

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
<b>It should differ but it didn't</b>

## Are there differences in activity patterns between weekdays and weekends?

It won't since filling na strategy didn't work!

--- End of Fun ---