#Download dataset:
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)

#Read data (561 comlums, each containing data concerning one feature):
testData <- read.table(unz(temp, "UCI HAR Dataset/test/X_test.txt"))
testData <- cbind(testData, read.table(unz(temp, "UCI HAR Dataset/test/y_test.txt"),col.names="activity"))

trainData <- read.table(unz(temp, "UCI HAR Dataset/train/X_train.txt"))
trainData <- cbind(trainData, read.table(unz(temp, "UCI HAR Dataset/train/y_train.txt"),col.names="activity"))
#Read info on features and activities:
featureLabels <- read.table(unz(temp, "UCI HAR Dataset/features.txt"),
                            stringsAsFactors=FALSE)
activityLabels <- read.table(unz(temp, "UCI HAR Dataset/activity_labels.txt")) 

#Merging test- and train data into a data.table:
rawData <- rbind(trainData,testData)

#Find features concerning mean and SD, to use along with activity:
labelsMeanAndSd <- 562 #column 562 is the "activity"-column
labels <- "activity"
oldLabels <- "activity"
for (i in seq_along(featureLabels[,2])) {
  if(grepl("mean",featureLabels[i,2]) | grepl("std",featureLabels[i,2])) {
    labelsMeanAndSd <- c(labelsMeanAndSd,i)
    labels <- c(labels,featureLabels[i,2])
    oldLabels <- c(oldLabels,colnames(rawData)[i])
  }
}

#Create the dataset as a data.table:
require("data.table")
data <- data.table(rawData[,labelsMeanAndSd])
setnames(data,old=oldLabels,new=labels)

#Exchange activity labels with descriptive activity names:
data$activity <- data[,activity<-activityLabels[activity,2]]

#The creation of a second dataset with average of each variable for each activity and each subject:
#Add subject-info to train- and test datasets:
testData <- cbind(testData, read.table(unz(temp, "UCI HAR Dataset/test/subject_test.txt"),col.names="subject",colClasses="factor"))
trainData <- cbind(trainData, read.table(unz(temp, "UCI HAR Dataset/train/subject_train.txt"),col.names="subject",colClasses="factor"))
rawData <- rbind(trainData,testData)

#Find features with averages, to use along with activity and subject ID:
labelsMean <- c(562,563) #"activity"- and "subject"-columns
labels <- c("activity","subject")
oldLabels <- c("activity","subject")
features <- character()
for (i in seq_along(featureLabels[,2])) {
  if(grepl("mean",featureLabels[i,2])) {
    labelsMean <- c(labelsMean,i)
    labels <- c(labels,featureLabels[i,2])
    features <- c(features,featureLabels[i,2])
    oldLabels <- c(oldLabels,colnames(rawData)[i])
  }
}

#Create data.frame with all raw data and add descriptive activity names:
combinedData <- data.frame(rawData[,labelsMean])
setnames(combinedData,old=oldLabels,new=labels)
combinedData$activity <- data[,activity<-activityLabels[activity,2]]


#Create the second dataset:
data2 <- data.frame(matrix(ncol=0,nrow=1))
for (i in seq_along(features)) {
  for (j in seq_along(levels(combinedData$activity))) {
    data2[,paste(features[i],"_",levels(combinedData$activity)[j],sep="")] <- 
      mean(combinedData[as.character(combinedData$activity)==levels(combinedData$activity)[j],
                   features[i]])
    
  }
  for (j in seq_along(levels(combinedData$subject))) {
    data2[,paste(features[i],"_SUBJECT_",levels(combinedData$subject)[j],sep="")] <- 
      mean(combinedData[as.character(combinedData$subject)==levels(combinedData$subject)[j],
                        features[i]])
    
  }
}

#Done reading:
unlink(temp)
