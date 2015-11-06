# Setting up the workspace and installing the required libraries.
setwd("/Users/Agam/Desktop/Sopra_project/CabPrediction")
install.packages("leaps")
install.packages("lubridate")
library(lubridate)

# Reading the data into R-studios
# The traindata used to create the model
traindata <- read.csv("Kaggle_YourCabs_training.csv", na.strings = "NULL")
# The testdata used to apply the model to! 
given_testdata <- read.csv("Kaggle_YourCabs_score.csv", na.strings = "NULL")
# The na.strings has been used to convert the NULL values to NAs

## Cleaning the data, missing value and extreme value treatment.

# Creating a copy of the read data so at to safeguard it from unwarranted changes.
traindata_dummy = traindata
traindata_dummy_short = traindata_dummy
# Now to decide what to replace the missing values with - either mean, median or mode - we see the percentage of NAs and the type of data in each column. If the percentage is more than 10%, then we simply remove the column, as imputing that much data would have effects on the result.
# I see that package_id, to_area_id, from_city_id, to_city_id, to_date, to_long, to_lat have to be ommitted as they are severely impurified.
traindata_dummy_short$package_id <- NULL
traindata_dummy_short$from_city_id <- NULL
traindata_dummy_short$to_city_id <- NULL
traindata_dummy_short$to_date <- NULL
traindata_dummy_short$to_long <- NULL
traindata_dummy_short$to_lat <- NULL
traindata_dummy_short$to_area_id <- NULL

# In the remaining columns where the NA % is less than 10% replae the NAs with median, mean or mode, depending on type of data.
replace_mean <- function(x){
  x[is.na(x)] = round(mean(x, na.rm = TRUE))
  return(x)
}
mode_function <- function(x){
  temp_vec = x[!is.na(x)]
  temp <- table(as.vector(temp_vec))
  return (max(temp))
}

traindata_dummy_short$from_lat = replace_mean(traindata_dummy_short$from_lat)
traindata_dummy_short$from_long[is.na(traindata_dummy_short$from_long)] = round(mean(traindata_dummy_short$from_long, na.rm = TRUE))
traindata_dummy_short$from_area_id[is.na(traindata_dummy_short$from_area_id)] = mode_function(traindata_dummy_short$from_area_id)

## Preparing the data, so that it makes good sense/efficiency when being used with the model.

# Since dates can't be used as it is. We convert them to usable/Rstudios supported format.
traindata_dummy_short$from_date = strptime(traindata_dummy_short$from_date, format ="%m/%d/%Y %H:%M")
traindata_dummy_short$booking_created = strptime(traindata_dummy_short$booking_created, format ="%m/%d/%Y %H:%M")
# Cluster dates on the basis of day
traindata_dummy_short$from_date = wday(traindata_dummy_short$from_date)
traindata_dummy_short$booking_created = wday(traindata_dummy_short$booking_created)
# 1 -> Sun, 2-> Mon and so on.

## Premodelling variable reduction and best variables' selection :

# Now our general data preparation is mostly done, in general cases. However since this is a huge dataset we can see
# if it is possible to reduce the no.of variables for ease of analyses. 
### Using the regsubs function mostly for data having categorial data
require(leaps)

best_subset = regsubsets(Car_Cancellation ~ travel_type_id + vehicle_model_id
                         + from_area_id + online_booking + mobile_site_booking
                         + from_lat + from_long + from_date + booking_created,
                         data = traindata_dummy_short, method = "backward")
## Creating a model
# Use random forest when you know there is a lot of data and can exploratory look makes you think that the data is arbitrarily distributed

# Installing the required package
install.packages("randomForest")
library(randomForest)
# Converting the Car_Cancellation columns to sensible data.
traindata_dummy_short$Car_Cancellation = as.factor(traindata_dummy_short$Car_Cancellation)
# Applying  the random Forest approach
model = randomForest(Car_Cancellation ~  travel_type_id + vehicle_model_id 
                     + online_booking + mobile_site_booking + 
                       from_lat + from_long + from_date + from_area_id ,
                     data = traindata_dummy_short, ntree = 50, mtry = 4, default = y)

## Making the predictions: 

# We would need to clean and prepare the given_testdata, i.e the data to which we apply the model to, just as we had done for the traindata while creating the model.
# This is done so that the testdata is sensible enough to be interpreted by the model.

# Note here you would have to convert the date columns to interpretable format, cluster them and then run the model.
given_testdata$from_date = strptime(given_testdata$from_date, format ="%m/%d/%Y %H:%M")
given_testdata$booking_created = strptime(given_testdata$booking_created, format ="%m/%d/%Y %H:%M")
given_testdata$from_date = wday(given_testdata$from_date)
given_testdata$booking_created = wday(given_testdata$booking_created)
# Replacing the NAs with the mean, mode or mode, depending on the tpe of data in them.
given_testdata$from_area_id[is.na(given_testdata$from_area_id)] = mode_function(given_testdata$from_area_id)
given_testdata$from_long[is.na(given_testdata$from_long)] = round(mean(given_testdata$from_long, na.rm = TRUE))
given_testdata$from_lat = replace_mean(given_testdata$from_lat)

# Running the random forest model on the cleaned testdata.
pred = predict(model, given_testdata)
# Storing the prediction results in the Car_Cancellation column of the testdata
given_testdata$Car_Cancellation = pred

## Writing the data onto the file: 
id = given_testdata$id
Car_Cancellation = given_testdata$Car_Cancellation
# Combining the id of the booking with its prediction result into a data frame.
result = data.frame(id,Car_Cancellation)
# Writing it onto files.
# A file with just the id and prediction result.
write.csv(result, file = "Kaggle_YourCabs_score_dum.csv", row.names = FALSE)
# A file with all the columns.
write.csv(given_testdata,file = "Kaggle_results_with_atts.csv", row.names = FALSE)
