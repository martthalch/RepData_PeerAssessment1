library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)

#read data
data<- read.csv("activity.csv", header=TRUE, sep=",", na.strings="NA")
data$date<-ymd(data$date)
#Filter out NAs
dataClean<- subset(data, steps!= "NA") 

##1 What is mean total number of steps taken per day?
#Aggregate by day
byDay<-aggregate(dataClean$steps, by=list(Date=dataClean$date), FUN= sum)
#a
byDay<-setNames(byDay, c("Date", "TotalSteps"))

#b Histogram
hist(byDay$TotalSteps, breaks=25,
     main="Histogram of Number of Steps by Date",
     xlab="Number of Steps",
     ylab= "Frequency",
     border="white",
     col="blue")

#c Calculate mean and median
mean(byDay$TotalSteps)
median(byDay$TotalSteps)

##2 What is the average daily activity pattern?
#Aggregate by interval
byInterval<-aggregate(dataClean$steps, 
                      by=list(interval=dataClean$interval), FUN= mean)
byInterval<-setNames(byInterval, c("interval", "IntervalMean"))

#Plot graph
plot(byInterval,type="l", main="Steps by Interval", xlab="Interval", ylab="Steps")

#get Max from intervals
byInterval[which.max(byInterval$IntervalMean),]

##3 Imputing missing values
#Number of NAs in the original DB
sum(is.na(data$steps))

##Substitute NAs with Interval mean by first doing an inner join and then subtituting with the x column
IntervalJoin<- inner_join(data, byInterval)
IntervalJoin$steps[is.na(IntervalJoin$steps)]<-IntervalJoin[is.na(IntervalJoin$steps),4]
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
IntervalJoin<-IntervalJoin[,c(1,2,3)]
#histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
byDayClean<-aggregate(IntervalJoin$steps, by=list(Date=IntervalJoin$date), FUN= sum)
byDayClean<-setNames(byDayClean, c("Date","TotalSteps"))
hist(byDayClean$TotalSteps, breaks=25,
     main="Histogram of Number of Steps by Date",
     xlab="Number of Steps",
     ylab= "Frequency",
     border="white",
     col="orange")
#Calculate and report the mean and median total number of steps taken per day
mean(byDayClean$TotalSteps)
median(byDayClean$TotalSteps)

##4 Are there differences in activity patterns between weekdays and weekends?
x<-IntervalJoin
x$Weekday<-weekdays(x$date)
x$Weekday.type <- ifelse(x$Weekday == "Saturday" | x$Weekday == "Sunday", "Weekend", "Weekday")

#Plot
xAggr<- aggregate(x$steps, by=list(Weekday.type=x$Weekday.type, interval=x$interval), FUN=mean)
xAggr<-setNames(xAggr, c("Weekday.type","interval","StepsMean"))
p<- ggplot(xAggr, aes(x=interval,y=StepsMean, group=Weekday.type))
p +geom_line(colour="blue", size=0.6) + facet_grid(Weekday.type~.) + 
  labs(title="Average steps by Weekend vs Weekday", x="Interval", y="Number of Steps")