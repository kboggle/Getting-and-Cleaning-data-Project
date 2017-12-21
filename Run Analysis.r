
library(tidyverse)


# 1. Merges the training and the test sets to create one data set.
# test data
test_x <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE) #%>% colClean()# %>% as_data_frame()
test_activity <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE) #%>% colClean()# %>% as_data_frame()
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE) #%>% colClean() #%>% as_data_frame()

#train data
train_x <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE) #%>% colClean() %>% as_data_frame()
train_activity <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE) #%>% colClean() %>% as_data_frame() 
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE) #%>% colClean() %>% as_data_frame() 

# Reading feature vector:
features <- read.table("UCI HAR Dataset/features.txt", header = FALSE) #%>% colClean() %>% as_data_frame() 
col
# Reading activity labels:
activitylabels = read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE) #%>% colClean() %>% as_data_frame() 

#Assigning Column names
colnames(train_x) <- features[,2]
colnames(test_x) <- features[,2]
colnames(train_activity) <- "activity_id"
colnames(test_activity) <- "activity_id"
colnames(test_subject) <- "subject_id"
colnames(train_subject) <- "subject_id"
colnames(activitylabels) <- c('activity_id','activity_type')

colnames(train_x) <-  gsub("\\()-","",colnames(train_x))
colnames(test_x)  <-  gsub("\\()-","",colnames(test_x))

#merging data

Completefile <- rbind(
  cbind(train_subject,train_x, train_activity), 
  cbind(test_subject, test_x, test_activity)
)
rm(train_activity,train_subject, train_x, test_activity, test_subject, test_x)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement

cols<- grepl("mean()",colnames(Completefile)) | grepl("std()",colnames(Completefile))
Activity_mean_std <- Completefile[,cols] #%>% colClean() %>% as_data_frame()


# 3. Uses descriptive activity names to name the activities in the data set

activitylabels = read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE) #%>% as_data_frame()
Completefile$activity_id <- factor(Completefile$activity_id, 
                                   levels = activitylabels[, 1], labels = activitylabels[, 2])

# 4. Appropriately labels the data set with descriptive variable names.

CompletefileCols <- colnames(Completefile)
CompletefileCols <- gsub("^f", "frequencyDomain", CompletefileCols)
CompletefileCols <- gsub("^t", "timeDomain", CompletefileCols)
CompletefileCols <- gsub("Acc", "Accelerometer", CompletefileCols)
CompletefileCols <- gsub("Gyro", "Gyroscope", CompletefileCols)
CompletefileCols <- gsub("Mag", "Magnitude", CompletefileCols)
CompletefileCols <- gsub("Freq", "Frequency", CompletefileCols)
CompletefileCols <- gsub("std", "StandardDeviation", CompletefileCols)


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

TidySet <- aggregate(. ~subject_id + activity_id, Completefile, mean)
write.table(TidySet, "Tidyset.txt", row.names = FALSE)

