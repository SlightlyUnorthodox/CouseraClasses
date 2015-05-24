# CourseraGettingAndCleaningDataFinalProject

##Overview

This repository contains the required work for the course project in Cousera's "Getting and Cleaning Data" segment of the Data Science specialization. This file contains a general overview of the project's contents.

##Data

The provided data from the dataset, "UCI HAR Dataset" contained a total of 561 unlabeled features describing human interactions with smartphones/"wearable technology". Feature labels and subjects could be found in additional files. More information about the data can be found here at "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones".

##Script

The script "run_analysis.R" carries out a number of operations on the data set. They are as follows.

*Note: To use the script it must be placed in the same directory as the "UCI HAR Dataset". It does not take any arguments.

1. Take the data provided in multiple files in "UCI HAR Dataset" and merge it into one cohesive dataset.
2. Extract only the values of interest to this particular study: mean and standard deviation.
3. Apply descript activity name to the the dataset.
4. Apply descriptive variable names to the dataset.
5. Create an output "tidy" set contains averages of values of interest.

##Output

This projects output, which is included in this repository, is called "tidy.txt" and contains the average values of interest from the merged data set.
