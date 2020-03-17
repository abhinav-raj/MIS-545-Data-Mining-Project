

#### New York Taxi trip duration ####

#### Libraries or Packages #####################################
# install.packages('vcd')
# install.packages('ggplot2')
# install.packages('geosphere')
# install.packages('lubridate')

## load packages
library(vcd) # for distribution plot
library(ggplot2) # for visualisation
library(geosphere) # for distance
library(lubridate) # for date-time formatting


################################################################
# train_data <- read.csv('train.csv')
# 
# set.seed(123)
# sample_size <- 70000
# sample_train_data <- train_data[sample(nrow(train_data), size=sample_size,
#                             replace=FALSE), ]
# 
# ## Save sample_train_data as 'sample_train.csv' file
# write.csv(sample_train_data, "sample_train.csv", row.names = FALSE)

######################################################################

new_train_data <- read.csv('sample_train.csv')
cat('Dimension of Sample Train Data: \n')
print(dim(new_train_data))
cat('Column Names: \n')
print(colnames(new_train_data))


## Check for missing value
cat('Check for missing value: \n')
print(sum(is.na(new_train_data)))
cat('check missing value in each column: \n')
print(colSums(is.na(new_train_data)))


## Summary of data
# cat('Summary of New Train Data: \n')
# print(summary(new_train_data))
cat('Origina Data Format: \n')
print(str(new_train_data))



## formatting target variable 'trip_duration'
# Dr. Zhang suggestion comment, "Since we mainly use classification- 
# methods, I suggest you convert your Ride Duration to Long/Short."

############## Summary for target variable "trip_duration"
summary_trip_duration <- summary(new_train_data$trip_duration)
# print(summary_trip_duration)
median_trip_duration <- summary_trip_duration['Median']
max_trip_duration <- summary_trip_duration['Max.']
cat('Median of trip_duration in Seconds: ')
print(median_trip_duration)

#### Some plots to decide where to cutoff for short and long -- 'trip_duration' ####
# distribution plot
distplot(new_train_data$trip_duration)

# density plot
dens_dist <- ggplot(new_train_data, aes(x=trip_duration)) +
  geom_density()
dens_dist


# boxplot
boxplot(new_train_data$trip_duration)


cat('Changing regression problem to classification problem: \n')
new_train_data$trip_duration <- cut(new_train_data$trip_duration,
                                    breaks=c(0, 2000, max_trip_duration),
                                    labels=c("Short Trip","Long Trip"))


########################################################################

###### Distance ####################################################
## 'Distance Travelled during each Trip' using
# 'pickup_latitude', 'pickup_longitude', 'dropoff_latitude', 'dropoff_longitude' features

i  <- cbind(pick_longitude = new_train_data$pickup_longitude,
            pick_latitude = new_train_data$pickup_latitude)
j <- cbind(drop_longitude = new_train_data$dropoff_longitude,
           drop_latitude = new_train_data$dropoff_latitude)

## computing haversine distance from co-ordinates
# distance column is added at the end
new_train_data$distance <- distHaversine(i, j)


#### Formatting Features: #####################################################
## 'pickup_datetime' and 'dropoff_datetime'
# convert 'Factor' into 'data/time' data type
new_train_data$pickup_datetime <- ymd_hms(new_train_data$pickup_datetime)
new_train_data$dropoff_datetime <- ymd_hms(new_train_data$dropoff_datetime)

new_train_data$pickup_month <- month(new_train_data$pickup_datetime)
new_train_data$pickup_day <- day(new_train_data$pickup_datetime)
new_train_data$pickup_hour <- hour(new_train_data$pickup_datetime)
new_train_data$pickup_minutes <- minute(new_train_data$pickup_datetime)



## formatting feature 'store_and_fwd_flag'
# convert 'Factor' to 'numeric' data type
new_train_data$store_and_fwd_flag <- as.numeric(as.factor(new_train_data$store_and_fwd_flag))

# cat('Summary after data formatting: \n')
# print(summary(new_train_data))
cat('List the types of Attributes: \n')
print(str(new_train_data))

######################################################################################




##
## Analyis of "trip_duration" target/dependent variable
cat('Summary of Trip Duration: \n')
print(summary(new_train_data$trip_duration))


