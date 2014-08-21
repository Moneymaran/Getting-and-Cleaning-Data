#Set working directory.
setwd("~/Arjun/College/Coursera/Data Science/Course 3 - Getting and Cleaning Data/UCI HAR");

##Step 1: Merge the train and test data sets.
#Read in the data.
features <- read.table('features.txt');
activity <- read.table('activity_labels.txt');
subject_train <- read.table('./train/subject_train.txt');
x_train <- read.table('./train/x_train.txt');
y_train <- read.table('./train/y_train.txt');
subject_test <- read.table('./test/subject_test.txt');
x_test <- read.table('./test/x_test.txt');
y_test <- read.table('./test/y_test.txt');
#Merge and name the data.
train <- cbind(subject_train, y_train, x_train);
test <- cbind(subject_test, y_test, x_test);
total_data <- rbind(train, test);
colnames(total_data) = c("subject", "activity", as.character(features[,2]));

##Step 2: Extract only the measurements on mean and standard deviation.
mean_std_indices <- grep("mean|std", features[, 2]);
all_indices <- c(1,2,mean_std_indices+2);
mean_std_data <- total_data[all_indices];

##Step 3: Use descriptive activity names to name the activities in the data set.
#Merge activity and name into one column.
colnames(activity) = c("activity", "activityName");
named_data <- merge(mean_std_data, activity, by = "activity", all.x=TRUE);
named_data[,1] <- paste(named_data[,1], named_data[,82]);
named_data[,82] = NULL;
#Switch subject back to being the first column.
names <- names(named_data);
names <- tail(names, length(names)-2);
fixed_data <- named_data[c("subject","activity",names)];

##Step 4: Appropriately label the data set with descriptive variable names. 
names <- colnames(fixed_data);
names <- gsub("\\(\\)", "", names);
names <- gsub("-", " ", names);
names <- gsub("mean", "Mean", names);
names <- gsub("std", "Standard Deviation", names);
names <- gsub("MeanFreq", "Mean Frequency", names);
colnames(fixed_data) = names;

##Step 5: Creates a new, independent data set with the average for each variable for each activity and each subject.
tidy_data <- aggregate(fixed_data[,3:ncol(fixed_data)], 
list(subject=fixed_data$subject, activity=fixed_data$activity), mean);
tidy_data <- tidy_data[order(tidy_data$subject),];

#Write out the tidy data set to a file.
write.table(tidy_data, "tidy_data.txt", row.name = FALSE);

