## Libraries

#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("RColorBrewer")
#install.packages("chron")
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(chron)

## Decompressing the File
unzip(zipfile="repdata_data_activity.zip")

## Color palette for the Plots
cols<-brewer.pal(7,"Set1")
pal<-colorRampPalette(colors = cols)
palette(pal(7))
ColorDays<-pal(7)

## Reading the file
data4work<-read.csv(file="activity.csv",na.strings = "NA")
head(data4work,n=5)

#### Part (1) Steps by Day

## Counting the Steps taken each day
data4work<-mutate(data4work,date=factor(data4work$date))
StepsByDay<-tapply(data4work$steps,data4work$date,sum)
StepsDayData<-data.frame(StepsByDay,Days=factor(unique(data4work$date)))
rm(StepsByDay)

## Histogram for the StepsDayData
par(mar=c(5,4,1,1), las=1)
with(data = StepsDayData,hist(StepsByDay,breaks = 20,col=pal(20),
                              xlab = "Steps by Day", ylab = "Days", 
                              main = "Frequency of Steps on a Day",
                              labels = TRUE))


#### Part (2) Daily Pattern (Steps by Interval)

#After removing the NAs

data4work<-mutate(data4work,interval=factor(data4work$interval))
data2work<-subset(data4work,!is.na(data4work[,1]))
StepsByIntervalMean<-tapply(data2work$steps,data2work$interval,mean)
StepsIntervalData<-data.frame(StepsByIntervalMean,
                              Interval=factor(unique(data4work$interval)))
rm(list=c("StepsByIntervalMean","data2work"))
head(StepsIntervalData)

## Plotting the daily behaviour. Scatterplot.

plot(as.numeric(levels(StepsIntervalData$Interval)),
     StepsIntervalData$StepsByIntervalMean,
     main = "Steps by Five Minute Interval", 
     xlab = "Interval",ylab = "Steps Average",
     col=ColorDays[4], type = "l")

grid(col = "lightgray", lty = "dotted", lwd = 1, equilogs = TRUE)

### Part (3) Imputing the Missing Values. NAs

missingDays<-unique(data4work$date[is.na(data4work$steps)])
data2work<-data4work
MeanSteps<-StepsIntervalData$StepsByIntervalMean
data2work$steps[data2work$date%in%missingDays]<-MeanSteps
rm(list=c("missingDays","MeanSteps","data4work"))

## Counting Steps by day (again)
StepsByDay<-tapply(data2work$steps,data2work$date,sum)
StepsDayData2<-data.frame(StepsByDay,Days=factor(unique(data2work$date)))
rm(StepsByDay)

## Repeating Histogram
par(mar=c(5,4,1,1), las=1)
with(data = StepsDayData2,hist(StepsByDay,breaks = 20,col=pal(20),
                              xlab = "Steps by Day", ylab = "Days", 
                              main = "Frequency of Steps on a Day",
                              labels = TRUE))

## Calculating the new Mean and Median
StepsByDaySummary2<-summary(StepsDayData2$StepsByDay)
print(StepsByDaySummary2)

## Comparing Weeks and Weekends
WeekyDays<-strptime(data2work$date, format = "%Y-%m-%d")
IsWeek<-!is.weekend(WeekyDays)
WeekFactor<-as.character(IsWeek)
WeekFactor[IsWeek]<-"Weekday"
WeekFactor[!IsWeek]<-"Weekend"
WeekyLevels<-c("Weekday","Weekend")
WeekFactor<-factor(WeekFactor,levels = WeekyLevels)
data2work<-mutate(data2work,weekday_or_weekend=WeekFactor)
rm(list = c("WeekyDays","IsWeek","WeekFactor","WeekyLevels"))


## Grupping the data
dataByWeekday<-group_by(data2work,by=weekday_or_weekend)
data4Weekend<-subset(data2work,grepl("Weekend",data2work$weekday_or_weekend))
data4Weekdays<-subset(data2work,grepl("Weekday",data2work$weekday_or_weekend))
StepsByIntervalWeekends<-as.numeric(tapply(data4Weekend$steps,
                                           data4Weekend$interval,mean))
StepsByIntervalWeekdays<-as.numeric(tapply(data4Weekdays$steps,
                                           data4Weekdays$interval,mean))
Interval<-as.numeric(levels(data4Weekend$interval))

## Plotting
par(mfrow=c(2,1), mar=c(5,4,1,1))
plot(Interval,StepsByIntervalWeekdays,type="l", main="Weekdays Behaviour",
     xlab= "Five Minutes Interval", ylab="Steps Taken - Average",
     col=ColorDays[4])
lines(Interval,StepsByIntervalWeekends,col="powderblue")
plot(Interval,StepsByIntervalWeekends,type="l", main="Weekends Behaviour",
     xlab= "Five Minutes Interval", ylab="Steps Taken - Average",
     col=ColorDays[4])
lines(Interval,StepsByIntervalWeekdays,col="powderblue")
par(mfrow=c(1,1))

rm(list=ls())

### La chuleta del plot
#with(data=Data2Work, plot(Days,MeanStepsByDay,type = "p",
#                          main = "Average Steps by Day", xlab = "Day", 
#                          ylab = "Steps Average",col=WeekyDays,
#                          pch=19,xlim = DaysLim),)
#
#grid(col = "lightgray", lty = "dotted", lwd = 2, equilogs = TRUE)
#legend("left", legend=levels(WeekDays),
#       pch=19,col=ColorDays,bty = "n", title = "Weekday")
#lines(x=Days,y=MeanStepsByDay,col="lightpink")
















