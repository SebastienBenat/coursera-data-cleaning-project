library(stringr)
library(dplyr)


### Step 1: Merges the training and the test sets to create one data set.

# Read the feature list
features <- sapply(strsplit(readLines('UCI HAR Dataset/features.txt'), " "), function(x) {x[2]})
features <- gsub("-", "_", features);
features <- gsub("\\(\\)", "", features);

# Read the activity labels
activity_labels <- sapply(strsplit(readLines('UCI HAR Dataset/activity_labels.txt'), " "), function(x) {x[2]})

# Read the training data
subject_train <- readLines('UCI HAR Dataset/train/subject_train.txt')
X_train <- strsplit(str_trim(readLines('UCI HAR Dataset/train/X_train.txt'))," +")
Y_train <- as.integer(readLines('UCI HAR Dataset/train/y_train.txt'))

# Read the test data
subject_test <- readLines('UCI HAR Dataset/test/subject_test.txt')
X_test <- strsplit(str_trim(readLines('UCI HAR Dataset/test/X_test.txt'))," +")
Y_test <- as.integer(readLines('UCI HAR Dataset/test/y_test.txt'))

# Merge the training and the test data
subject_all <- as.integer(c(subject_train, subject_test))
X_all <- append(X_train, X_test);
Y_all <- c(Y_train, Y_test)
df1 <- data.frame(Subject = subject_all);


### Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
measurement_indices <- grep("mean|std", features)
for (i in 1:length(measurement_indices)) {
    cur_index <- measurement_indices[i];
    df1[features[cur_index]] <- sapply(X_all, function(x){as.numeric(x[cur_index])})
}


### Step 3: Uses descriptive activity names to name the activities in the data set
Y_all <- sapply(Y_all, function(x) {activity_labels[x]})
df1["Activity"] <- Y_all


### Step 4: Appropriately labels the data set with descriptive variable names.
### At this point, the data set (df1) is already labeled with descriptive variable names.


### Step 5: From the data set in step 4, creates a second, independent tidy data set
### with the average of each variable for each activity and each subject.
tmp_df <- as.data.frame(df1 %>% group_by(Subject, Activity) %>% summarise_each(funs(mean)))

tmp_subject_column <- c()
tmp_activity_column <- c()
tmp_variable_column <- c()
tmp_mean_column <- c()

for (i in 1:nrow(tmp_df)) {
    cur_row = tmp_df[i,];
    for (j in measurement_indices) {
        tmp_subject_column <- c(tmp_subject_column, cur_row$Subject);
        tmp_activity_column <- c(tmp_activity_column, cur_row$Activity);
        tmp_variable_column <- c(tmp_variable_column, features[j]);
        tmp_mean_column <- c(tmp_mean_column, cur_row[[features[j]]]);
    }
}

df2 <- data.frame(Subject = tmp_subject_column, Activity = tmp_activity_column, Variable = tmp_variable_column, Average = tmp_mean_column)
write.table(df2, 'processed_data.txt', row.names = FALSE);
