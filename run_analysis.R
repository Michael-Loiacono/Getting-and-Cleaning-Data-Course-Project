## This script is set up to do the following tasks 
## 1) Merges the training and the test sets to create one data set.
## 2) Extracts only the measurements on the mean and standard deviation 
##    for each measurement
## 3) Uses descriptive activity names to name the activities in the data set
## 4) Appropriately labels the data set with descriptive variable names
## 5) From the data set in step 4, creates a second, independent tidy data set with 
##    the average of each variable for each activity and each subject

## Download the UCI HAR dataset from the following link. You can use download.file
## (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
#

## Ensure your working directory is set to the location of the UCI HAR dataset was unzipped
## ex. setwd("~/Desktop/R Code/UCI HAR Dataset")


## 1) Merges the training and the test sets to create one data set.

## Read in all data
## TRAIN
xTrain <- read.table('./train/x_train.txt',header=FALSE);
yTrain <- read.table('./train/y_train.txt',header=FALSE);
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE);

##TEST
xTest <- read.table('./test/x_test.txt',header=FALSE);
yTest <- read.table('./test/y_test.txt',header=FALSE);
subjectTest <- read.table('./test/subject_test.txt',header=FALSE);


## Fatrues and Activity Labels
features <- read.table('./features.txt',header=FALSE);
activityLabels <- read.table('./activity_labels.txt',header=FALSE);


## Add column names to the data

colnames(xTrain) <- features[,2];
colnames(yTrain) <- "activityID";
colnames(subjectTrain) <- "subjectID";
colnames(xTest) <- features[,2];
colnames(yTest) <- "activityID";
colnames(subjectTest) <- "subjectID";
colnames(activityLabels) <- c("activityID", "activityType");

## Combine data into full Train and Test sets

trainData <- cbind(subjectTrain, yTrain, xTrain);
testData <- cbind(subjectTest, yTest, xTest);

#combine the Train and Test data

combinedData <- rbind(trainData, testData);

## PART 1 COMPLETE

## 2) Extracts only the measurements on the mean and standard deviation 
##    for each measurement

## We will subset the data using the column names and match those which we need
## using grep1 to get a logical vector of the columns

colNamesCombined <- colnames(combinedData);

## columns we need
colsactivity<- grepl("activity", colNamesCombined);
colssubject <- grepl("subject", colNamesCombined);
colsMean <- grepl("mean", colNamesCombined);
colsStd <- grepl("std", colNamesCombined);
## we want to remove the Mean Freq calculation since colsMean will include them
colsMeanFreq <- grepl("meanFreq", colNamesCombined);
## Combine all needed columns
colsNeeded <- (colssubject | colsactivity | colsMean & !colsMeanFreq | colsStd);

## Subset the combinedData set to get the desired columns
finalDatap2 <- combinedData[colsNeeded];

## PART 2 COMPLETE

## 3) Uses descriptive activity names to name the activities in the data set
## I will merge the activityLabels and finalDatap2 to create descriptive names

newLabelData <- merge(activityLabels, finalDatap2);

## PART 3 COMPLETE

## 4) Appropriately labels the data set with descriptive variable names
## Work with only column names for speed then reassign after

colNamesp4 <- colnames(newLabelData)

for (i in 1:length(colNamesp4)){
    colNamesp4[i] <- gsub("Acc", "Accelerometer", colNamesp4[i])
    colNamesp4[i] <- gsub("Gyro", "Gyroscope", colNamesp4[i])
    colNamesp4[i] <- gsub("\\()", "", colNamesp4[i])
    colNamesp4[i] <- gsub("-std", "StandardDeviation", colNamesp4[i])
    colNamesp4[i] <- gsub("BodyBody", "Body", colNamesp4[i])
    colNamesp4[i] <- gsub("Mag", "Magnitude", colNamesp4[i])
    colNamesp4[i] <- gsub("-mean", "Mean", colNamesp4[i])
    colNamesp4[i] <- gsub("^(t)", "time", colNamesp4[i])
    colNamesp4[i] <- gsub("^(f)", "frequency", colNamesp4[i])
    colNamesp4[i] <- gsub("-X", "ForX", colNamesp4[i])
    colNamesp4[i] <- gsub("-Y", "ForY", colNamesp4[i])
    colNamesp4[i] <- gsub("-Z", "ForZ", colNamesp4[i])
}

## Rename columns using new column names
colnames(newLabelData)<-colNamesp4

## PART 4 COMPLETE

## 5) From the data set in step 4, creates a second, independent tidy data set with 
##    the average of each variable for each activity and each subject
## Remove activityType from data for easier manipulation
aT <- "activityType"
cutData <- newLabelData[, ! names(newLabelData) %in% aT, drop = F]

## Get the mean of each column

finalTidy <- (cutData %>% group_by(subjectID,activityID) %>% summarise_each(funs(mean)));

## Add the activityType back in and order by activityID then subjectID

finalTidy <- merge(activityLabels, finalTidy);
finalTidyO<-arrange(finalTidy, activityID, subjectID)

## Write the table to the working directory

write.table(finalTidyO, './tidyData.txt', row.names = FALSE)
##PART 5 COMPLETE