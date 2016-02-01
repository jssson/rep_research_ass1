#Loading and preprocessing the data
if (!file.exists("./acivity.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "acivity.zip" )
    unzip("acivity.zip")
}
DFactivities <- read.csv("activity.csv")
DFactivities$date<- as.Date(DFactivities$date,format="%Y-%m-%d")
activities<-DFactivities[is.na(DFactivities$steps)==FALSE,]


#What is mean total number of steps taken per day?
#1 Calculate the total number of steps taken per day
stepPerDay.Sum<-aggregate(activities$steps,list(activities$date),sum)

names(stepPerDay.Sum)<-c("date","steps")
print("Sum step per day:")
print(stepPerDay.Sum)

#2 Histogram of the total number of steps taken each day
hist(stepPerDay.Sum$steps,xlab = "steps per day",ylab="frequency",main = "total number of steps/day")

#3 Calculate and report the mean and median of the total number of steps taken per day
stepPerDay.Mean<-aggregate(activities$steps,list(activities$date),mean)
stepPerDay.Median<-aggregate(activities$steps,list(activities$date),median)
print("Mean step per day:")
names(stepPerDay.Mean)<-c("date","steps")
print(stepPerDay.Mean)
print("Median step per day:")
names(stepPerDay.Median)<-c("date","steps")
print(stepPerDay.Median)

#What is the average daily activity pattern?
#1 Make a time series plot (i.e. type = "l") of the 5-minute 
#interval (x-axis) and the average number of steps taken, 
#averaged across all days (y-axis)
stepPerIntvl.Mean<-aggregate(activities$steps,list(activities$interval),mean)
names(stepPerIntvl.Mean)<-c("interval","steps")
plot(stepPerIntvl.Mean$interval,stepPerIntvl.Mean$steps,type='l',xlab="interval",ylab="Avg steps", main ="Steps/Interval")

#5 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
print("Interval with max. number of steps:")
print(stepPerIntvl.Mean$interval[stepPerIntvl.Mean$steps==max(stepPerIntvl.Mean$steps)])

#Imputing missing values
#1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
print("Number of missing values:")
print(sum(complete.cases(DFactivities)==FALSE))

#2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
print("Missing values will be replaced by the mean of the interval")

#3 Create a new dataset that is equal to the original dataset but with the missing data filled in.
repNaActivities<-DFactivities[is.na(DFactivities$steps)==TRUE,]
repNaActivities$steps<- stepPerIntvl.Mean$steps

#4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
repNaActivities.Sum<-aggregate(repNaActivities$steps,list(repNaActivities$date),sum)
names(repNaActivities.Sum)<-c("date","steps")
hist(stepPerDay.Sum$steps,xlab = "steps per day",ylab="frequency",main = "Total number of steps/day:\n NA replaced by mean/interval")
repNaActivities.Mean<-aggregate(repNaActivities$steps,list(repNaActivities$date),mean)
names(repNaActivities.Mean)<-c("date","steps")
repNaActivities.Median<-aggregate(repNaActivities$steps,list(repNaActivities$date),median)
names(repNaActivities.Median)<-c("date","steps")

print("New mean step per day:")
print(repNaActivities.Mean)
print("New median step per day:")
print(repNaActivities.Median)

#Are there differences in activity patterns between weekdays and weekends?
#1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
library("timeDate")
repNaActivities$weekdays<-isWeekday(repNaActivities$date)

#2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
stepsPerWdIntvl.mean<-aggregate(repNaActivities$steps,list(repNaActivities$weekdays,repNaActivities$interval),mean)
names(stepsPerWdIntvl.mean)<-c("isWeekday","interval","meanSteps")
library(lattice)
stepsPerWdIntvl.mean$weekday[stepsPerWdIntvl.mean$isWeekday]<-"Weekday"
stepsPerWdIntvl.mean$weekday[stepsPerWdIntvl.mean$isWeekday==FALSE]<-"Weekend"
print(xyplot(meanSteps~interval|weekday,stepsPerWdIntvl.mean,type="l",xlab="intervals",ylab="mean steps",main="Steps per workday/weekend/interval"))