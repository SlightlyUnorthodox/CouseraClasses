library(ggplot2)
# Loading and preprocessing the data

## Q1. Load the data
data <- read.table("activity.csv",header=TRUE,sep=",")
data <- na.omit(data)
## Q2. Process/transform the data (if necessary) into a format suitable for analysis
steps <- tapply(data$steps,data$date,FUN=sum,na.rm=TRUE)

# What is mean total number of steps taken per day?

## Q1. Calculate the total number of steps taken per day
stepsTotal <- sum(steps,na.rm=TRUE)
## Q2. Make a histogram of the toal number of steps taken per day
qplot(steps,binwidth=1000,main="Steps Taken Per Day",xlab="Steps Taken")
## Q3. Calculate and report the mean and median of the total number of steps taken per day
mean(steps,na.rm=TRUE)
median(steps,na.rm=TRUE)

# What is the average daily activity pattern?

## Q1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
avgSteps <- aggregate(x=list(steps=data$steps),by=list(interval=data$interval),FUN=mean,na.rm=TRUE)
ggplot(data=avgSteps,aes(x=interval,y=steps))+
  geom_line()+
  xlab("Interval (5-minute)")+
  ylab("Average # of Steps Taken")
## Q2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
avgSteps[which.max(avgSteps$steps),]

# Inputing Missing Values

## Q1.
sumMissing <- is.na(data$steps)
table(sumMissing)
## Q2.
fill <- function(steps,interval) {
  rep <- NA
  if(!is.na(steps))
    rep <- c(steps)
  else
    rep <- (avgSteps[avgSteps$interval == interval,"steps"])
  return(rep)
}
## Q3.
fullData <- data
fullData$steps <- mapply(fill,fullData$steps,fullData$interval)
## Q4.
stepsTotal <- tapply(fullData$steps,fullData$date,FUN=sum)
qplot(stepsTotal,binwidth=1000,xlab="Total # of Steps Taken Per Day")
mean(stepsTotal,na.rm=TRUE)
median(stepsTotal,na.rm=TRUE)

# Are there differences in activity patters between weekdays and weekends

## Q1.
weekSort <- function(date) {
  day <- weekdays(date)
  if(day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
    return("weekday")
  else if (day %in% c("Saturday","Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
fullData$date <- as.Date(fullData$date)
fullData$day <- sapply(fullData$date, FUN=weekSort)
## Q2.
avgSteps <- aggregate(steps ~ interval + day, data=fullData, mean)
ggplot(avgSteps, aes(interval,steps))+ geom_line()+ facet_grid(day ~ .)+ xlab("Interval (5-minute)")+ ylab("# of Steps")
