## useing the following libraries
library("dplyr")
library("reshape2")

## seting data directory
ds_dir <- "UCI Human Activity Recognition Dataset"

## Creating the 3 functions that take the name of the file to be loaded and directories
## 1 Load a file 
load_file <- function(filename, ...) {
  file.path(..., filename) %>%
  read.table(header = FALSE)
}

## 2 Load the training file
load_train_file <- function(filename) {
  load_file(filename, ds_dir, "train")
}

## 3 Load the test file
load_test_file <- function(filename) {
  load_file(filename, ds_dir, "test")
}

## Useing list of activity values to describe test / training labels
describe_lbl_ds <- function(ds) {
  names(ds) <- activity_col  
  ds$Activity <- factor(ds$Activity, levels = activity_lbl$V1, labels = activity_lbl$V2)
  ds
}

## Taking a dataset capturing results of feature tests and associates columns with individual features
describe_act_ds <- function(ds) {
  col_names <- gsub("-", "_", features$V2)
  col_names <- gsub("[^a-zA-Z\\d_]", "", col_names)
  names(ds) <- make.names(names = col_names, unique = TRUE, allow_ = TRUE)
  ds
}

## Adjusting column name in the data set identifying test participants
describe_sub_ds <- function(ds) {
  names(ds) <- subject_col
  ds
}

## Downloading and extracting a zip file with datasets
if (!file.exists(ds_dir)) {
  source_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  dest_file <- "Dataset.zip"
  download.file(source_url, destfile = dest_file, method = "curl")
  unzip(dest_file)
  if (!file.exists(ds_dir)) 
    stop("The downloaded dataset doesn't have the expected structure!")
}

## Custom columns
subject_col <- "Subject"
activity_col <- "Activity"

## Loading features as human-readable column names
features <- load_file("features.txt", ds_dir)

## Loading activity labels
activity_lbl <- load_file("activity_labels.txt", ds_dir)

## Useing descriptive activity names to name the activities in the data set
## Training data:
train_set <- load_train_file("X_train.txt") %>% describe_act_ds
train_lbl <- load_train_file("y_train.txt") %>% describe_lbl_ds
train_sub <- load_train_file("subject_train.txt") %>% describe_sub_ds

## Test data:
test_set <- load_test_file("X_test.txt") %>% describe_act_ds
test_lbl <- load_test_file("y_test.txt") %>% describe_lbl_ds
test_sub <- load_test_file("subject_test.txt") %>% describe_sub_ds

## Mergeing the training and the test sets to create one dataset
merge_data <- rbind(
                cbind(train_set, train_lbl, train_sub),
                cbind(test_set, test_lbl, test_sub)
              ) %>%
              select(
                matches("mean|std"), 
                one_of(subject_col, activity_col)
              )

## Creating a second, independent tidy data set with the average of each variable for each activity and each subject
id_cols <- c(subject_col, activity_col)
tidy_data <- melt(
               merge_data, 
               id = id_cols, 
               measure.vars = setdiff(colnames(merge_data), id_cols)
             ) %>%
             dcast(Subject + Activity ~ variable, mean)
             
## Saveing the result
## in text
write.table(tidy_data, file = "tidy_data.txt", sep = ",", row.names = FALSE)

### in csv
write.table(tidy_data, file = "tidy_data.csv", sep = ",", row.names = FALSE)
