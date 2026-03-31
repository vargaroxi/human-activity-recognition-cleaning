# libraries
library(dplyr)
library(stringr)

# Read the data
featuresPath <- "./UCI HAR Dataset/features.txt"
featureVector <- read.table(featuresPath, header = FALSE)

activityPath <- "./UCI HAR Dataset/activity_labels.txt"
activityLabels <- read.table(activityPath, header = FALSE)

subjectTestPath <- "./UCI HAR Dataset/test/subject_test.txt"
subjectTestIndex <- read.table(subjectTestPath, header = FALSE)

XTestPath <- "./UCI HAR Dataset/test/X_test.txt"
XTestData <- read.table(XTestPath, header = FALSE)

YTestPath <- "./UCI HAR Dataset/test/Y_test.txt"
YTestData <- read.table(YTestPath, header = FALSE)

subjectTrainPath <- "./UCI HAR Dataset/train/subject_train.txt"
subjectTrainIndex <- read.table(subjectTrainPath, header = FALSE)

XTrainPath <- "./UCI HAR Dataset/train/X_train.txt"
XTrainData <- read.table(XTrainPath, header = FALSE)

YTrainPath <- "./UCI HAR Dataset/train/Y_train.txt"
YTrainData <- read.table(YTrainPath, header = FALSE)

# Merge the training and test data
measurementData <- rbind(XTestData, XTrainData) 
subjects <- rbind(subjectTestIndex, subjectTrainIndex)
activity <- rbind(YTestData, YTrainData)
rm(XTestData, XTrainData, subjectTestIndex, subjectTrainIndex, YTestData, YTrainData)

# extract only the mean and std for each measurement and name the variables 
names(measurementData) <- featureVector[,2]
cols <- grepl("mean\\(\\)|std\\(\\)", names(measurementData))
measurementData <- measurementData[, cols]

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
  str_replace_all("-", "") %>% 
  str_remove("_$") %>% 
  tolower()

# Name the subject vector
names(subjects) <- "subject"

# Use descriptive activity names
featureMap <- setNames(activityLabels[[2]], activityLabels[[1]])
activity <- featureMap[as.character(activity[[1]])]

# Merge all the columns together and convert the df to tibble
measurementData <- bind_cols(subjects, tibble(activity = activity), measurementData)

# Create a second summary table
measurementSummary  <- measurementData %>% 
  group_by(subject, activity) %>% 
  summarize(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
                                                                                
write.table(measurementSummary, "measurementSummary.txt", row.names = FALSE)