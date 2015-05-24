setwd("c:/Users/SS026381/R/")

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")

x_data <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt") 
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")

join_data <- rbind(x_train, x_data)
join_label <- rbind(y_train, y_test)
join_subject <- rbind(train_subject, test_subject)

features <- read.table("UCI HAR Dataset/features.txt")
mean_std <- grep("mean\\(\\)|std\\(\\)", features[, 2])
join_data <- join_data[, mean_std]

names(join_data) <- gsub("\\(\\)", "", features[mean_std, 2])
names(join_data) <- gsub("mean", "Mean", names(join_data))
names(join_data) <- gsub("std", "Std", names(join_data))
names(join_data) <- gsub("-", "", names(join_data))

activity <- read.table("UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[join_label[, 1], 2]
join_label[, 1] <- activityLabel
names(join_label) <- "activity"

names(join_subject) <- "subject"
cleanedData <- cbind(join_subject, join_label, join_data)
write.table(cleanedData, "merged_data.txt")

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
write.table(result, "tidy.txt")
