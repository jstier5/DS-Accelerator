
library(tidyverse)         

############################
# PROBLEM 1: Merges the training and the test sets to create one data set.
############################

#set working directory
setwd('C:/UserData/OneDrive - Red Ventures/Documents/UCI HAR Dataset/')

#Read in data files
features <-  read.table('./features.txt')
activity_labels <- read.table('./activity_labels.txt')

subject_train <- read.table('./train/subject_train.txt')
x_train <- read.table('./train/x_train.txt')
y_train <- read.table('./train/y_train.txt')

subject_test <- read.table('./test/subject_test.txt')
x_test <- read.table('./test/x_test.txt')
y_test <- read.table('./test/y_test.txt')

#Organize training data
colnames(activity_labels) <- c('ActivityID', 'ActivityLabel')
colnames(subject_train) <- 'SubjectID'
colnames(x_train) <- features[,2]
colnames(y_train) <- 'ActivityID'

#Organize test data
colnames(subject_test) <- 'SubjectID'
colnames(x_test) <- features[,2]
colnames(y_test) <- 'ActivityID'

#Bind the data together
TrainingData <- cbind(y_train, subject_train, x_train)
TestData <- cbind(y_test, subject_test, x_test)
AllTheData <- rbind(TrainingData, TestData)


############################
# PROBLEM 2: Extracts only the measurements on the mean and standard deviation for each measurement.
############################

ColumnNames <- colnames(AllTheData)
ColsToKeep1 <- grep('mean()', ColumnNames, ignore.case = FALSE)
ColsToKeep2 <- grep('std()', ColumnNames, ignore.case = FALSE)
ColsToKeep <- c(1, 2, ColsToKeep1, ColsToKeep2)
AllTheData2 <- AllTheData[,ColsToKeep]


############################
# PROBLEM 3: Uses descriptive activity names to name the activities in the data set
############################

AllTheData3 <- left_join(activity_labels, AllTheData2, by = 'ActivityID') %>% select(-ActivityID)
  

############################
# PROBLEM 4: Appropriately labels the data set with descriptive variable names.
############################

AllTheData4 <- AllTheData3
ColumnNames <- colnames(AllTheData4)

ColumnNames <- lapply(ColumnNames, function(x) gsub('\\()', '', x))
ColumnNames <- lapply(ColumnNames, function(x) gsub('-mean', ' Mean', x))
ColumnNames <- lapply(ColumnNames, function(x) gsub('-std', ' Std Dev', x))
ColumnNames <- lapply(ColumnNames, function(x) gsub('Gyro', ' Gyroscope ', x))
ColumnNames <- lapply(ColumnNames, function(x) gsub('Acc', ' Accelerometer ', x))
ColumnNames <- lapply(ColumnNames, function(x) gsub('^(f)', 'Freq ', x))
ColumnNames <- lapply(ColumnNames, function(x) gsub('^(t)', 'Time ', x))

colnames(AllTheData4) <- ColumnNames


############################
# PROBLEM 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
############################

AllTheData5 <- group_by(AllTheData4, ActivityLabel, SubjectID) %>% 
  summarise_all(mean)
