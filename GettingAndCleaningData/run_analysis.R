run_analysis <- function() {
  #Step 1
  
  # Run this function in the same directory which contains the "UCI HAR Dataset" folder
  # Reads full contents on training set files
  trainingSet = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
  trainingSet[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
  trainingSet[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
  
  # Reads full contents of training set files
  testSet = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
  testSet[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
  testSet[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

  # Merged trainingSet and testSet
  mergedSet = rbind(trainingSet, testSet)
  
  #Step 2
  
  # Read features and make the feature names better suited for R with some substitutions
  feat= read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
  feat[,2] = gsub('-mean', 'Mean', feat[,2])
  feat[,2] = gsub('-std', 'Std', feat[,2])
  feat[,2] = gsub('[-()]', '', feat[,2])
  
  # Extracts only the measurements on the mean and standard deviation
  dat <- grep(".*Mean.*|.*Std.*", feat[,2])
  # Reduces to wanted feat values
  feat <- feat[dat,]
  # Bind final two columns
  dat <- c(dat, 562, 563)
  # Remove extra data
  mergedSet <- mergedSet[,dat]
  
  #Steps 3 & 4
  
  #Reads activity labels file
  actLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
  
  # Add the column names to mergedSet
  colnames(mergedSet) <- c(feat$V2, "Activity", "Subject")
  colnames(mergedSet) <- tolower(colnames(mergedSet))
  
  i <- 1
  for (h in actLabels$V2) {
    mergedSet$activity <- gsub(i, h, mergedSet$activity)
    i <- i + 1
  }
  
  mergedSet$activity <- as.factor(mergedSet$activity)
  mergedSet$subject <- as.factor(mergedSet$subject)
  
  # Step 5
  
  tidySet = aggregate(mergedSet, by=list(activity = mergedSet$activity, subject=mergedSet$subject), mean)
  write.table(tidySet, "tidy.txt", sep="\t")
}