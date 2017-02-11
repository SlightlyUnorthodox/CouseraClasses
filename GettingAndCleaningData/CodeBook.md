# CodeBook: tidy dataset

Information about the collection of data and in-depth analysis can be found at "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones". A detailed description of the the involved variables and their meanings can be found there as well.

## Variable substitution

In the creation of the tidy data set several variable names were adjusted for coherence with R code syntax.

1. "-mean" was replaced with Mean
2. "-std" was replaced with Std
3. "-()" was removed entirely

## Variable names (561 total values)

Variable names were  originally obtained from "UCI HAR Dataset/features.txt". As a general rule, only alphanumeric characters were retained in variable names.

### Variable name examples

* tbodyyaccmeanx
* tbodygyromeanx
* tbodygyrojerkstdz

## Activity names (180 total values) 

Activity names were originally obtained from "UCI HAR Dataset/activity_labels.txt".

### Activity name examples

* LAYING
* SITTING
* STANDING
* WALKING
* WALKING_DOWNSTAIRS
* WALKING_UPSTAIRS
