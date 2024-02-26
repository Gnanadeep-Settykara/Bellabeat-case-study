# Loading packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

# Importing Datasets
activity <- read.csv("C:/Users/Deep/Desktop/SE/Case study Data Analytics/Dataset/fitbit/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
calories <- read.csv("C:/Users/Deep/Desktop/SE/Case study Data Analytics/Dataset/fitbit/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
intensities <- read.csv("C:/Users/Deep/Desktop/SE/Case study Data Analytics/Dataset/fitbit/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
steps <- read.csv("C:/Users/Deep/Desktop/SE/Case study Data Analytics/Dataset/fitbit/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
sleep <- read.csv("C:/Users/Deep/Desktop/SE/Case study Data Analytics/Dataset/fitbit/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight <- read.csv("C:/Users/Deep/Desktop/SE/Case study Data Analytics/Dataset/fitbit/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

# activity
head(activity)

# intensities
head(intensities)

# calories
head(calories)

# steps
head(steps)

# sleep
head(sleep)

# weight
head(weight)

# There are some missing values in weight in 'fat' column

# Converting column names and fixing date formats
# Since I won't be using "Daily_Intensities" dataset, there's no need to change it.

# activity
activity_new <- activity %>%
  rename_with(tolower) %>%
  mutate(date = as.Date(activitydate, "%m/%d/%y")) # fixing date formats
str(activity_new) # preview the results

# intensities
intensities_new <- intensities %>%
  rename_with(tolower) %>%
  mutate(date = as.Date(activityday, "%m/%d/%y")) # fixing data formats
str(intensities_new) # preview the results

# calories
calories_new <- calories %>%
  rename_with(tolower) %>%
  rename(activitydate = activityday) %>%
  mutate(date = as.Date(activitydate, "%m/%d/%y")) # fixing date formats
str(calories_new) # preview the results

# steps
steps_new <- steps %>%
  rename_with(tolower) %>%
  rename(activitydate = activityday) %>%
  mutate(date = as.Date(activitydate, "%m/%d/%y")) # fixing date formats
str(steps_new) # preview the results

# sleep
sleep_new <- sleep %>%
  rename_with(tolower) %>%
  rename(activitydate = sleepday) %>%
  mutate(date = as.Date(activitydate, "%m/%d/%y")) # fixing date formats
str(sleep_new) # preview the results

# weight
weight_new <- weight %>%
  rename_with(tolower) %>%
  rename(activitydate = date) %>%
  mutate(date = as.Date(activitydate, "%m/%d/%y")) # fixing date formats
str(weight_new) # preview the results

# Exploring the data
n_distinct(activity$Id)
n_distinct(intensities$Id)
n_distinct(calories$Id)
n_distinct(steps$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

# activity
summary(activity_new)

# intensities
summary(intensities_new)

# calories
summary(calories_new)

# steps
summary(steps_new)

# sleep
summary(sleep_new)

#weight
summary(weight_new)

# Hyphothesis 1: The total steps taken are postively correlated with calories burned
ggplot(data = activity_new, mapping = aes(x=totalsteps, y=calories, color=date)) + 
  geom_point() + geom_smooth() +
  labs(title = "Total Steps vs. Calories")

# Hyphothesis 2: The total sleep minutes has a negative correlation with calories burned
merged1 <- merge(sleep_new, calories_new, by=c('id', 'date'))
head(merged1)

# Visualizing
ggplot(data = merged1, mapping = aes(x=totalminutesasleep, y=calories, color=date)) +
  geom_point() + geom_smooth() +
  labs(title = "Total Sleep Minutes vs. Calories")

# Hyphothesis 3: The total sedentary minutes has a negative correlation with total calories burned.
merged2 <- merge(intensities_new, calories_new, by=c('id', 'date'))
head(merged2)

# Visualizing
ggplot(data = merged2, mapping = aes(x=sedentaryminutes, y=calories, color=date)) +
  geom_point() + geom_smooth() +
  labs(title = "Total Sedentary Minutes vs. Calories")

# Hyphothesis 4: The sedentary minutes has a positive correlation with the bmi
merged3 <- merge(weight_new, activity_new, by=c('id', 'date'))
head(merged3)

# Visualizing
ggplot(data = merged3, mapping = aes(y=bmi, x=sedentaryminutes, color=date)) +
  geom_point() + geom_smooth() +
  labs(title = "Sedentary Mintues vs. Calories")

# Hyphothesis 5: Total time asleep has a positive correlation with total time in bed
ggplot(data = sleep_new, mapping = aes(x=totaltimeinbed, y=totalminutesasleep, color=date)) + 
  geom_point() + geom_smooth() +
  labs(title = "Total Time Asleep vs. Total Time in Bed")
