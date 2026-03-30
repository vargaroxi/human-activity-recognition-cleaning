# libraries
library(dplyr)
library(stringr)

# Read the data
features <- "./UCI HAR Dataset/features.txt"
featureVector <- read.table(features, header = FALSE)

activity <- "./UCI HAR Dataset/activity_labels.txt"
activityLabels <- read.table(activity, header = FALSE)

subjectTest <- "./UCI HAR Dataset/test/subject_test.txt"
subjectTestIndex <- read.table(subjectTest, header = FALSE)

XTest <- "./UCI HAR Dataset/test/X_test.txt"
XTestData <- read.table(XTest, header = FALSE)

YTest <- "./UCI HAR Dataset/test/Y_test.txt"
YTestData <- read.table(YTest, header = FALSE)

subjectTrain <- "./UCI HAR Dataset/train/subject_train.txt"
subjectTrainIndex <- read.table(subjectTrain, header = FALSE)

XTrain <- "./UCI HAR Dataset/train/X_train.txt"
XTrainData <- read.table(XTrain, header = FALSE)

YTrain <- "./UCI HAR Dataset/train/Y_train.txt"
YTrainData <- read.table(YTrain, header = FALSE)

# Merge the training and test data
measurementData <- rbind(XTestData, XTrainData) 
subjects <- rbind(subjectTestIndex, subjectTrainIndex)
activity <- rbind(YTestData, YTrainData)
rm(XTestData, XTrainData, subjectTestIndex, subjectTrainIndex, YTestData, YTrainData)

# extract only the mean and std for each measurement and name the variables 
names(measurementData) <- featureVector[,2]
cols <- grep("mean\\(\\)|std\\(\\)", names(measurementData))
measurementData <- measurementData[,cols]

names(measurementData) <- names(measurementData) %>% 
  str_replace("^t", "time_") %>% 
  str_replace("^f", "frequency_") %>% 
  str_replace_all("Body", "body_") %>%
  str_replace_all("Gravity", "gravity_") %>%
  str_replace_all("Acc", "acceleration_") %>%
  str_replace_all("Gyro", "gyroscopic_") %>%
  str_replace_all("Jerk", "jerk_") %>%
  str_replace_all("Mag", "magnitude_") %>%
  str_replace_all("mean\\(\\)", "mean_") %>%
  str_replace_all("std\\(\\)", "std_") %>%
  str_replace_all("-", "")

# Name the subject vector
names(subjects) <- "subject"

# Use descriptive activity names
featureMap <- setNames(activityLabels[[2]], activityLabels[[1]])
activity <- featureMap[as.character(activity[,1])]

# Merge all the columns together and convert the df to tibble
measurementData <- as_tibble(cbind(subjects, activity, measurementData))

# Create a second summary table
measurementSummary  <- measurementData %>% group_by(subject, activity) %>% summarize(across(-(1:2), \(x) mean(x, na.rm = TRUE) ))
write.table(measurementSummary, "measurementSummary.txt", row.names = FALSE)