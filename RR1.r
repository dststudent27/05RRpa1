# Coursera Reproducible Research PA1, oct2015

# Section 0: Preprocessing
# display date & time
print(currentDate <- date())

# load/read the original csv data file
data <- read.csv("activity.csv", header = TRUE, sep = ',', 
                colClasses = c("numeric", "character", "integer"))
dFr <- data
# examine the structure of the data frame
str(dFr)

# 1st five rows
head(dFr, 5)

# last five rows
tail(dFr, 5)

# take a summary of the data
summary(dFr)

# determine if any missing values exist (as NAs) in the data frame
missing <- sum(is.na(dFr$steps))
print(missing)

# if necessary, create data frame with NAs omitted
dFrNotNA <- na.omit(dFr)

# format the date-time object as 'ymd' as necessary
library(lubridate)
dFr$date <- ymd(data[, 2])


# Section 1: Determine the Average Number of Steps Taken per Day
# aggregate total steps per date, omitting missing data
dailyStepsByDate <- aggregate(steps ~ date, dFrNotNA, sum)
# print(dailyStepsByDate)

# create histogram of total number of steps in a day
hist(dailyStepsByDate$steps,
     breaks = 5,
     col = "#33FF33",     
     border = "#3300FF", 
     lwd = 2, lty = 1,
     main = "Histogram: Number of Steps per Day", 
     xlab = "Daily Total Number of Steps")     

# mean/median total number of steps per day
mean (dailyStepsByDate$steps)
median (dailyStepsByDate$steps)


# Section 2: Determine the Average Daily Activity Pattern

# aggregate steps by interval vs days for daily activity pattern
dailyStepsByInterval <- aggregate(steps ~ interval, dFrNotNA, mean)
library(ggplot2)
avgDailyPattern <- aggregate(x = list(steps = dFrNotNA$steps), 
                             by = list(interval = dFrNotNA$interval), 
                             FUN = mean)

# ggplot method of average daily activity pattern 
# time series plot of average number of steps per 5-min. intervals vs days
ggplot(data = avgDailyPattern, aes(x = interval, y = steps)) +
      labs(title = "Time Series Plot: Average #Steps per 5-minute Interval") +
      geom_line(size = 1, color = "#990099") +
      xlab("5-min. Interval (averaged across all days)") +
      ylab("Average Number of Steps")

# Determine which 5-min. interval, averaged, contains maximum number of steps
maxStepsInterval <- which.max(avgDailyPattern$steps)
avgDailyPattern[maxStepsInterval, ]


# Section 3: Imput Missing Values
# display count of missing values (also observed from the 'str' command)
print(missing)

# assign a temporary data frame & a variable for the missing values, 'NAs'  
dFr2 <- data
theNAs <- is.na(dFr2$steps)

# replace each 'NA' with its respective 'average step count per interval';
# house the resulting dataset in the variable 'newSetNoNAs'
thisInterval <- tapply(dFr2$steps, dFr2$interval, mean, na.rm = TRUE)
newSetNoNAs <- thisInterval[as.character(dFr2$interval[theNAs])]

# verify that the newly created dataset contains no missing values...
sum(is.na(newSetNoNAs))

# dFr2 verification status before ...
nrow(dFr2[!complete.cases(dFr2), ])
summary(dFr2)

# selectively, row-by-row, impute each 'NA' value with its average step count 
# per interval value
dailyStepsByInterval <- aggregate(steps ~ interval, dFrNotNA, mean)
for (i in 1:nrow(dFr2)) {
      if (is.na(dFr2$steps[i])) {
            intervalValue <- dFr2$interval[i]
            thisRow <- which(dailyStepsByInterval$interval == intervalValue)
            sCountValue <- dailyStepsByInterval$steps[thisRow]
            dFr2$steps[i] <- sCountValue
      } # end if
} # end for

# dFr2 verification status after ...
nrow(dFr2[!complete.cases(dFr2), ])
summary(dFr2)

# create histogram of imputed values
dailyStepsImputed <- aggregate(steps ~ date, dFr2, sum)
hist(dailyStepsImputed$steps,
     breaks = 5,
     col = "#FF0033",     
     border = "#000000", 
     lwd = 2, lty = 1,
     main = "Histogram: Number of Steps per Day (Imputed)", 
     xlab = "Daily Total Number of Steps")

# mean/median total number of steps per day of imputed values
mean(dailyStepsImputed$steps)
median(dailyStepsImputed$steps)

# determine the impact of imputing missing values into the estimates
cat("Imputing causes minimal impact.")

# Section 4: Asses differences in activity patterns between weekdays & weekends
# format date-time object to ymd as necessary
dFr2$date <- ymd(data[, 2])

# add new column to indicate day of week 
dFr2$day <- weekdays(dFr2$date)

# add new column & initialize to weekday
dFr2$dayType <- c("weekday")

# assign Saturdays & Sundays to dayType = weekend
for (i in 1:nrow(dFr2)){
      if (dFr2$day[i] == "Saturday" || dFr2$day[i] == "Sunday"){
            dFr2$dayType[i] <- "weekend"
      }
}

# convert dayType from character to factor
dFr2$dayType <- as.factor(dFr2$dayType)

# create the weekdays/weekends panel plot
# library(ggplot2) already called ...
# aggregate steps by interval to get average steps per interval across days

intervalStepsImputed <- aggregate(steps ~ interval + dayType, dFr2, FUN = mean)

ggplot(intervalStepsImputed, aes(interval, steps)) +
      labs(title = "Time Series Panel Plot: Weekday/Weekend Activity") +
      geom_line(size = 1, color = "#660099") + 
      xlab("Interval") + 
      ylab("Number of Steps") + 
      facet_wrap(~ dayType, ncol = 1)

