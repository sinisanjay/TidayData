setwd(file.path("~/rclass/3GettingAndCleaningData/cleanproject"))

#Load Reqd Libraries
library("tidyr")
library("dplyr")
library("data.table")

#Download dataset
projurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!dir.exists("UCI HAR Dataset")){ 
  if(file.exists("proj_data.zip")){
    unzip("proj_data.zip")
  }else{
    download.file(projurl, destfile = "proj_data.zip", "curl")
    unzip("proj_data.zip")
  }
}

#activity
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
vec_activitylabel <- activityLabels[[2]]

#training data
training<-fread("UCI HAR Dataset/train/X_train.txt", na.strings=c("", "NA"))
train_activity <- fread("UCI HAR Dataset/train/y_train.txt", na.strings=c("", "NA"))
train_subject <- fread("UCI HAR Dataset/train/subject_train.txt", na.strings=c("", "NA"))
#3. Uses descriptive activity names to name the activities in the data set
traindf <- training %>% mutate(activity=vec_activitylabel[train_activity[[1]]], subject=train_subject[[1]])
train <- tbl_df(traindf)

#test data
testing<-fread("UCI HAR Dataset/test/X_test.txt", na.strings=c("", "NA"))
test_activity <- fread("UCI HAR Dataset/test/y_test.txt", na.strings=c("", "NA"))
test_subject <- fread("UCI HAR Dataset/test/subject_test.txt", na.strings=c("", "NA"))
#3. Uses descriptive activity names to name the activities in the data set
testdf <- testing %>% mutate(activity=vec_activitylabel[test_activity[[1]]], subject=test_subject[[1]])
test <- tbl_df(testdf)

#1. Merges the training and the test sets to create one data set.
fulldataset <- rbind(train, test)

#feature list
allfeature <- read.table("UCI HAR Dataset/features.txt")
vec_allfeature <- allfeature[[2]]

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
requiredcolumns <- grep(".*mean.*|.*std.*", vec_allfeature)
reqfeaturenames <- vec_allfeature[requiredcolumns]
reqfeaturenames = gsub('-mean', 'Mean', reqfeaturenames)
reqfeaturenames = gsub('-std', 'Std', reqfeaturenames)
reqfeaturenames <- gsub('[-()]', '', reqfeaturenames)
requiredcolumns <- c(requiredcolumns, 562,563)

reqdataset <- fulldataset[,requiredcolumns]

#final data set
#4. Appropriately labels the data set with descriptive variable names.
colnames(reqdataset) <- c(reqfeaturenames, "activity", "subject")

#[1] 0.1267078 mean(reqdataset[[79]])
#2nd dataset
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy2 <- reqdataset %>% group_by(activity, subject) %>% summarize_each(funs(mean))

write.table(reqdataset, sep=" ", file="TidyData1.txt", row.names = FALSE, col.names = FALSE)
write.table(tidy2, sep=" ", file="TidyData2.txt", row.names = FALSE, col.names = FALSE)