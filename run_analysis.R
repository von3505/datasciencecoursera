## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# download the file and unzip
fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile="./Dataset.zip")
unzip("./Dataset.zip", files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)
# check out which files are in there
pathIn <- file.path(getwd(), "UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)

#dl package to fread
install.packages ("data.table", lib="C:/Users/Yvonne Low/Documents/R")
#load the library
library(data.table)

require("reshape2")

#read the subject files
subject_train <- fread(file.path(pathIn, "train", "subject_train.txt"))
subject_test <- fread(file.path(pathIn, "test", "subject_test.txt"))

#read the activity files
y_train <- fread(file.path(pathIn, "train", "Y_train.txt"))
y_test <- fread(file.path(pathIn, "test", "Y_test.txt"))

#read the data files (fread can't process)
fileToDataTable <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}
X_train <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
X_test <- fileToDataTable(file.path(pathIn, "test", "X_test.txt"))

###

# Load: activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
# Load: data column names
features <- read.table("./UCI HAR Dataset/features.txt")[,2]
# Extract only the measurements on the mean and standard deviation for each measurement.
extract_features <- grepl("mean|std", features)

names(X_test) = features
# Extract only the measurements on the mean and standard deviation for each measurement.
X_test = X_test[,extract_features]
# Load activity labels
y_test[,2] = activity_labels[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Label")
names(subject_test) = "subject"

# Bind data
test_data <- cbind(as.data.table(subject_test), y_test, X_test)
# Load and process X_train & y_train data.
names(X_train) = features
# Extract only the measurements on the mean and standard deviation for each measurement.
X_train = X_train[,extract_features]
# Load activity data
y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Label")
names(subject_train) = "subject"
# Bind data
train_data <- cbind(as.data.table(subject_train), y_train, X_train)
# Merge test and train data
data = rbind(test_data, train_data)
id_labels = c("subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(data), id_labels)
melt_data = melt(data, id = id_labels, measure.vars = data_labels)
# Apply mean function to dataset using dcast function
tidy_data = dcast(melt_data, subject + Activity_Label ~ variable, mean)
write.table(tidy_data, file = "./tidy_data.txt")
