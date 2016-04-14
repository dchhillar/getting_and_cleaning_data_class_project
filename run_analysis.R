# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
#The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
#A full description is available at the site where the data was obtained:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#Here are the data for the project:
  
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#You should create one R script called run_analysis.R that does the following.

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#review criteria
# 1.The submitted data set is tidy.
# 2.The Github repo contains the required scripts.
# 3.GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
# 4.The README that explains the analysis files is clear and understandable.
# 5.The work submitted for this project is the work of the student who submitted it.

#set working directory to the location of my data
setwd('/Users/liz9189/Desktop/Random Files/R Class/JHUClass/UCI HAR Dataset')

#--------------------------------------------------------------------------------
# STEP 1. Merges the training and the test sets to create one data set.
#--------------------------------------------------------------------------------

# Read in the data from the .txt files in the downloaded zip file
#labelling data
features <- read.table('./features.txt',header=FALSE)
activity_type <- read.table('./activity_labels.txt',header=FALSE)

#training data
subject_train <- read.table('./train/subject_train.txt',header=FALSE)
x_train <- read.table('./train/x_train.txt',header=FALSE)
y_train <- read.table('./train/y_train.txt',header=FALSE)

# test data
subject_test <- read.table('./test/subject_test.txt',header=FALSE)
x_test       <- read.table('./test/x_test.txt',header=FALSE) 
y_test       <- read.table('./test/y_test.txt',header=FALSE)

# Assigin column names to the data imported above
colnames(activity_type) <- c('activityid','activitytype')
#train data
colnames(subject_train) <- "subjectid"
colnames(x_train)       <- features[,2]
colnames(y_train)       <- "activityid"
#test data
colnames(subject_test)  <- "subjectid"
colnames(x_test)        <- features[,2] 
colnames(y_test)        <- "activityid"

# Create the final train dataset by merging the x_train, y_train, and subject_train files
training_data <- cbind(y_train,subject_train,x_train)
View(training_data)

#creating the final test dataset by merging the x_test, y_test, and subject_y files
test_data <- cbind(y_test, subject_test, x_test)
View(test_data)

# Combine training and test data to create a final dataset
test_and_train <- rbind(training_data,test_data)
View(test_and_train)

#--------------------------------------------------------------------------------
# STEP: 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#--------------------------------------------------------------------------------

library(dplyr)

#removing the non-standard characters from the column names and making them each unique column names
#this takes out the ()'s and the []s
#it leaves in .'s and _'s
unique_column_names   <- make.names(names=names(test_and_train), unique=TRUE, allow_ = FALSE)
names(test_and_train) <- unique_column_names

#picking out only the variables that I want (those with mean and stdev)
mean_std_only <- select(test_and_train, activityid, subjectid, contains("mean"), contains("std"))
View(mean_std_only)

#--------------------------------------------------------------------------------
# STEP: 3. Uses descriptive activity names to name the activities in the data set
#--------------------------------------------------------------------------------

final_data <- merge(mean_std_only,activity_type,by='activityid',all.x=TRUE)

#--------------------------------------------------------------------------------
# STEP: 4. Appropriately labels the data set with descriptive variable names.
#--------------------------------------------------------------------------------

#removing all the periods from the variable names
no_period_column_names = gsub("\\.","",names(final_data))

#making the variable names something that makes sense
std_names       = gsub("std","stdev", no_period_column_names)
time_names      = gsub("^(t)","time", std_names)
freq_names      = gsub("^(f)","freq", time_names)
acc_mag_names   = gsub("AccMag","AccMagnitude", freq_names)
body_jerk_names = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude", acc_mag_names)
jerk_mag_names  = gsub("JerkMag","JerkMagnitude", body_jerk_names)
final_names     = gsub("GyroMag","GyroMagnitude", jerk_mag_names)

#assigning the new clean column names to the table
names(final_data) <- final_names

#--------------------------------------------------------------------------------
# STEP: 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#--------------------------------------------------------------------------------
#creating a new table w/o activitytype in it
finalDataNoActivityType  = finalData[,names(final_data) != 'activitytype']

# Summarizing data to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityid','subjectid')],by=list(activityid=finalDataNoActivityType$activityid,subjectid = finalDataNoActivityType$subjectid),mean);

# Merging the data with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activity_type,by='activityid',all.x=TRUE)
