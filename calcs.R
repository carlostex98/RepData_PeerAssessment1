library(lattice)

activity <- read.csv(unz("activity.zip","activity.csv"),header=T)
activity$date <- as.Date(activity$date,'%Y-%m-%d')
activity$interval <- as.factor(activity$interval)
head(activity,5)

histogram(tapply(activity$steps, activity$date,sum,na.rm=T),
          xlab = "steps per day", main = "Histogram of Steps per day")

message(paste("Average steps per day:",
              round(mean(tapply(activity$steps, activity$date,sum,na.rm=T)),
                    digits = 2)))

message(paste("Median steps per day:",
              round(median(tapply(activity$steps, activity$date,sum,na.rm=T)),
                    digits = 2)))

xyplot(tapply(activity$steps,activity$interval,mean,na.rm=T) ~ 
         as.numeric(unique(activity$interval)),type='l', 
       xlab = 'Number of period of the day',
       ylab = 'Average number of steps',
       main = "Average number of steps per n-th 5 interval period of the day")


max.steps <-max(tapply(activity$steps,activity$interval,mean,na.rm=T))
max.interval <- which(tapply(activity$steps,activity$interval,mean,na.rm=T) %in% 
                        max.steps)
message(paste0("The maximum average number of steps was ",round(max.steps,2),
               " and it occured in interval ",max.interval,"."))

activity.imp <- activity
activity.imp[is.na(activity.imp$steps),]$steps <- 
  as.vector(tapply(activity.imp$steps,activity.imp$interval,mean,na.rm=T)
            [activity.imp[is.na(activity.imp$steps),]$interval])

days <- weekdays(as.Date(activity.imp$date,"%Y-%m-%d"))
days[days=='Sunday' | days == 'Saturday'] <- 'weekend'
days[days!='weekend'] <- 'weekday'
activity.imp <- cbind(activity.imp,days)

persdaysmeans <- tapply(activity.imp$steps,
                        list(activity.imp$interval,activity.imp$days),mean)
activity.imp <- cbind(activity.imp,persdaysmeans = 
                        vector('numeric',length = dim(activity.imp)[1]))
activity.imp[activity.imp$days == 'weekday',5] <- 
  as.vector(persdaysmeans[activity.imp[activity.imp$days == 'weekday',3],1])
activity.imp[activity.imp$days == 'weekend',5] <- 
  as.vector(persdaysmeans[activity.imp[activity.imp$days == 'weekend',3],2])

histogram(tapply(activity.imp$steps,activity.imp$date,sum),
          xlab = "Number of steps per day", main = "Histogram of Daily Steps")

message(paste("Average steps per day:",
              round(mean(tapply(activity.imp$steps, activity.imp$date,sum)),
                    digits = 2)))
message(paste("Median steps per day:",
              round(median(tapply(activity.imp$steps, activity.imp$date,sum)),
                    digits = 2)))

with(unique(activity.imp[,3:5]),
     xyplot(persdaysmeans ~ as.numeric(interval) | days,type='l',
            layout = c(1,2),xlab = 'Interval', ylab = 'Number of Steps'))

