# CodeBook
This CoodBook describes the variables, data, and any transformations performed to clean up the data used in the week 3 "Getting and Cleaning Data course" project for the Johns Hopkins.

## The data source
* Data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
* Description of the dataset: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


### Set the source directory 
After setting the source directory for the files, read into tables the data located in
```r
setwd("c:/Users/SS026381/R/")
```

### Transformation details
There are 5 steps involve in this project. Each step is explained below in detail:

* Merges the training and the test sets to create one data set.
* Extract only the measurements on the mean and standard deviation for each measurement.
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive activity names.
* Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


### The dataset includes the following files:
* README.txt
* features_info.txt
* features.txt
* activity_labels.txt
* train/X_train.txt
* train/y_train.txt
* test/X_test.txt
* test/y_test.txt


### Step1. Merges the training and the test sets to create one data set.

```r
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_data <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt") 
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
join_data <- rbind(x_train, x_data)
join_label <- rbind(y_train, y_test)
join_subject <- rbind(train_subject, test_subject)
```

### Step2. Extracts only the measurements on the mean and standard deviation for each measurement. 

```r
features <- read.table("UCI HAR Dataset/features.txt")
mean_std <- grep("mean\\(\\)|std\\(\\)", features[, 2])
join_data <- join_data[, mean_std]
names(join_data) <- gsub("\\(\\)", "", features[mean_std, 2]) # remove "()"
names(join_data) <- gsub("mean", "Mean", names(join_data)) # capitalize M
names(join_data) <- gsub("std", "Std", names(join_data)) # capitalize S
names(join_data) <- gsub("-", "", names(join_data)) # remove "-" in column names 
```

### Step3. Uses descriptive activity names to name the activities in the data set

```r
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[join_label[, 1], 2]
join_label[, 1] <- activityLabel
names(join_label) <- "activity"
```

### Step4. Appropriately labels the data set with descriptive activity names. 

```r
names(join_subject) <- "subject"
cleanedData <- cbind(join_subject, join_label, join_data)
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset
```

### Step5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

```r
subjectLen <- length(table(join_subject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(join_subject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
write.table(result, "tidy.txt") # write out the 2nd dataset
```

### Running the script run_analysis.R:

* Create a folder and unzip the dataset root directory
* Clone the repo in the root directory.
* Change the path to the folder before running the script. i.e. '''r setwd("c:/Users/SS026381/R/") '''
* Type source("run_analysis.R") to execute the script
