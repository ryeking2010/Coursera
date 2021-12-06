# this script will look at R L3 coursera course project 1 
# By Ryan King

library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)
file_path <- "C:/Users/I0485672/Downloads/activity.csv"
data <- read.csv(file_path)

# there are some NAs here
# perhaps the device was not on. Shall we impute zero or drop data?
# going with dropping as we cannot say they didn't move!

# let's look at the total number of steps taken each day
data_summary <- data[complete.cases(data), ] %>% group_by(date) %>% summarize(mean=sum(steps, na.rm = TRUE))
data_summary

data_summary %>% ggplot(aes(x = mean)) + geom_histogram(bins = 20) +
  theme_bw() + labs(x='Mean steps per day', y='Number of occurances', 
                    title = 'Frequency of mean steps per day')

# let's look at the mean steps per day. We will group by the date
data_summary <-  data[complete.cases(data), ] %>% group_by(date) %>% summarize(StepsSum=sum(steps, na.rm = TRUE))
 # THE MEAN:
mean(data_summary$StepsSum)

# THE MEDIAN:
median(data_summary$StepsSum)

# What's the max steps in a given interval?
data_summary.max <-  data[complete.cases(data), ] %>% group_by(interval) %>% summarize(StepsMax=max(steps, na.rm = TRUE))
tophit <- data_summary.max %>% arrange(desc(StepsMax)) %>% head(n=1)
tophit
# The 615 interval has the max steps

# But that's max... what's the max (highest) average steps taken
data_summary <-  data[complete.cases(data), ] %>% group_by(interval) %>% summarize(mean=mean(steps, na.rm = TRUE))
data_summary %>% arrange(desc(mean))


max.avg.results <- data_summary %>% arrange(desc(mean)) %>% head(n=1)
print(paste("The largest interval is,", max.avg.results$interval, "with", max.avg.results$mean, "max steps on average"))



# let's look at a time series
# there's multiple intrepretations of the request, but I think they want
# per time, what is the average steps for each day. So let's groupby time
data_summary <-  data[complete.cases(data), ] %>% group_by(interval) %>% summarize(mean=mean(steps, na.rm = TRUE))

ggplot(data_summary, aes(x = interval, y = mean)) + 
  geom_point() + geom_line() + theme_bw() + 
  labs(y="Mean steps", x = "Minutes from start of day", 
       title = "Average number of steps during the day")

# let's look at the highest steps taken
data_summary <-  data[complete.cases(data), ] %>% group_by(interval) %>% summarize(mean=mean(steps, na.rm = TRUE))
data_summary %>% arrange(desc(mean))

# the largest interval
data_summary %>% arrange(desc(mean)) %>% head(n=1)


# How to impute data? There's many many theories out there
# mean - on each interval
# nearest neighbor - for example, steps on a Saturday vs. Monday and take mean
# actually, not bad, let's do weekdays + time
# with the avg steps per invterval, an improvement could be a lagging avg

data.avg.per.weekday <- data
data.avg.per.weekday$date <- as.Date(data$date) %>% weekdays()
data.avg.per.weekday <-  data.avg.per.weekday[complete.cases(data.avg.per.weekday), ] %>% group_by(date, interval) %>% summarize(mean=mean(steps, na.rm = TRUE))


data.meanImpute <- data
for (i in 1:nrow(data.meanImpute)){
  if(is.na(data.meanImpute[i, 1])){
    data.meanImpute[i, 1] <- data.avg.per.weekday %>% 
      filter(date == weekdays(as.Date(data[i, 2])) & interval == data[i, 3]) %>%
      .[[3]]
  }
}
# cha ching
any(is.na(data.meanImpute))

# let's look at average steps each day
data.check <- data.meanImpute %>% group_by(date) %>% summarize(DailySteps = sum(steps))

data.check %>% ggplot(aes(x = DailySteps)) +
  geom_histogram() + theme_bw() + 
  labs(y="Days with number of steps", x = "Daily steps", 
       title = "Average number of steps during the day")


# let's get the days of the week reordered properly for the graph
data.meanImpute$date <- weekdays(as.Date(data.meanImpute$date))
data.meanImpute$date <- factor(data.meanImpute$date, levels = 
                                    c("Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday", "Saturday", 
                                      "Sunday"))

data.meanImpute.eachday <- data.meanImpute %>% group_by(date, interval) %>% summarise(meanSteps = mean(steps))

data.meanImpute.eachday %>% ggplot(aes(x = interval, y = meanSteps, color = date)) +
  geom_line() + theme_bw() + facet_wrap(~date) + 
  labs(title = "Average steps across days of the week\nwith imputed data",
       x = "Time in minutes", y = "Average mean steps")

# and now we can tell that people are more active saturday night and weekday mornings

data.meanImpute$date <- str_replace(data.meanImpute$date, "Saturday", "Weekend")
data.meanImpute$date <- str_replace(data.meanImpute$date, "Sunday", "Weekend")
data.meanImpute$date <- if_else(data.meanImpute$date == 'Weekend', 'Weekend', 'Weekday')

data.meanImpute.final <- data.meanImpute %>% group_by(date, interval) %>% summarise(meanSteps = mean(steps))

data.meanImpute.final %>% ggplot(aes(x = interval, y = meanSteps, color = date)) +
  geom_line() + theme_bw() + facet_wrap(~date) + 
  labs(title = "Average steps across days of the week\nwith imputed data",
       x = "Time in minutes", y = "Average mean steps")
