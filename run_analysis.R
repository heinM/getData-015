isWantedFeature <- function (x) {
  substrs <- c("mean()", "std()")
  any(sapply(substrs, function (y) grepl(y, x, fixed=TRUE)))
}

featureToColClass <- function(feature) {
  if (isWantedFeature(feature)) {
    "numeric"
  } else {
    "NULL"
  }
}

features <- read.table(
  "UCI HAR Dataset/features.txt",
  col.names=c("id", "name"))

activityMap <- read.table(
  "UCI HAR Dataset/activity_labels.txt",
  col.names=c("ActivityId", "ActivityName"))


featureColClasses <- sapply(features$name, featureToColClass)


getDataset <- function (trainOrTest) {
  data <- read.table(
    paste("UCI HAR Dataset/", trainOrTest, "/X_",
          trainOrTest, ".txt", sep=""),
    col.names=features$name,
    colClasses=featureColClasses)
  
  subjectData <- read.table(
    paste("UCI HAR Dataset/", trainOrTest,
          "/subject_", trainOrTest, ".txt", sep=""),
    col.names=c("SubjectId")
  )
  
  # Add subject data to the main data
  data$SubjectId <- subjectData$SubjectId
  
  # Read in Y variable data
  yData <- read.table(
    paste("UCI HAR Dataset/", trainOrTest,
          "/y_", trainOrTest, ".txt", sep=""),
    col.names=c("ActivityId")
  )
  
  # Replace ActivityID with ActivityName
  # and add to main data frame
  data$Activity <- merge(
    yData, activityMap)[,"ActivityName"]
  
  # Return main data frame
  data
}


## Get data for both the training set and test data
trainingData <- getDataset("train")
testData <- getDataset("test")


## Append this data together
allData <- rbind(testData, trainingData)

## Take the mean of all columns except SubjectId and Activity
## and output to tidyData
tidyData <- aggregate(
  allData[,!names(allData) %in% c("SubjectId", "Activity")],
  by=list(allData$SubjectId, allData$Activity), mean)

## Write tidyData to a file in the working directory
write.table(tidyData, file="tidyData.txt")
